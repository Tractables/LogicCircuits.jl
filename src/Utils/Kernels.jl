#####################
# helper arithmetic to fuse complex high-arity operations
#####################

function __init__()
    set_zero_subnormals(true) # this is supposed to speed up floating point arithmetic on certain architectures
end

@inline safe_div(x::T, y::T) where T = iszero(y) ? 0 : x/y # seems to be much slower than replacing NaN after the fact

@inline replace_nan(x::T) where T = isnan(x) ? zero(T) : x

# need to define this because Julia says true + true = 2 instead of true
Base.@pure accumulator_op(::Type{<:Number}) = :(@fastmath +)
Base.@pure accumulator_op(::Type{Bool}) = :(|)
Base.@pure product_op(::Type{<:Number}) = :(@fastmath *)
Base.@pure product_op(::Type{Bool}) = :(&)

@inline assign(acc::AbstractArray, pr::AbstractArray) =
    acc .= pr # seems to be just as fast as copyto!

@inline @generated accumulate_val(acc::AbstractArray, x::AbstractArray) =
    :(@fastmath  acc .= $(accumulator_op(eltype(acc))).(acc, x))


# generate product expressions

const max_unroll_products = 15

"generate an expression for the loop-unrolled product of xs..."
function expand_product(num_factors, ::Type{E}, xs::Union{Symbol,Expr})  where E
    if num_factors<=0
        :()
    elseif num_factors==1
        :($xs[1])
    else
        :($(product_op(E)).($(expand_product(num_factors-1,E,xs)),$xs[$num_factors])) #strangely adding @inbounds here slows things down a lot
    end
end

"generate an expression for the loop-unrolled product of x1 and xs..."
function expand_product(num_products, ::Type{E}, x1::Union{Symbol,Expr}, xs::Union{Symbol,Expr}) where E
    if num_products == 0
        x1
    else
        @assert num_products <= max_unroll_products "Unrolling loop too many times ($num_products), the compiler will choke... implement a loopy version instead."
        #strangely adding @inbounds here slows things down a lot
        :($(product_op(E)).($(expand_product(num_products-1,E,x1,xs)),xs[$num_products]))
    end
end

# number array
const NArr = AbstractArray{<:Number}


# computer a product efficiently with almost no allocations

@inline @generated function prod_fast_unroll(x1::NArr, xs::NArr...)
    :(@fastmath $(expand_product(length(xs),eltype(x1),:x1,:xs)))
end

@inline function prod_fast(xs::AbstractArray{<:NArr})
    if length(xs) > max_unroll_products
        tmp = prod_fast(xs[max_unroll_products:end])
        prod_fast_unroll(tmp, xs[1:max_unroll_products-1]...)
    else
        prod_fast_unroll(xs...)
    end
end

@inline function prod_fast(x1::NArr, xs::AbstractArray{<:NArr})
    if length(xs) > max_unroll_products-1
        tmp = prod_fast(xs[max_unroll_products-1:end])
        prod_fast_unroll(x1, tmp, xs[1:max_unroll_products-2]...)
    else
        prod_fast_unroll(x1, xs...)
    end
end


# assign a product to an accumulator argument 

@inline @generated function assign_prod_unroll(acc::NArr, x1::NArr, xs::NArr...)
    :(acc .= $(expand_product(length(xs),eltype(acc),:x1,:xs)))
end

@inline function assign_prod(acc::NArr, xs::AbstractVector{<:NArr})
    if length(xs) > max_unroll_products
        tmp = prod_fast(xs[max_unroll_products:end])
        assign_prod_unroll(acc, tmp, xs[1:max_unroll_products-1]...)
    else
        assign_prod_unroll(acc, xs...)
    end
end

@inline function assign_prod(acc::NArr, x1::NArr, xs::AbstractVector{<:NArr})
    if length(xs) > max_unroll_products-1
        tmp = prod_fast(xs[max_unroll_products-1:end])
        assign_prod_unroll(acc, tmp, x1, xs[1:max_unroll_products-2]...)
    else
        assign_prod_unroll(acc, x1, xs...)
    end
end


# accumulate a product into an accumulator argument

# Many implementations of `accumulate_prod` are very slow, and somehow don't properly fuse broadcasts':
# - acc .|= (&).(xs...) is terrible!
# - acc .|= unrolled_reduce((x,y) -> x .& y, x1, xs) is also bad, it still does memory allocations!
# Hence, meta-programming to the rescue.
@inline @generated function accumulate_prod_unroll(acc::NArr, x1::NArr, xs::NArr...)
    :(@fastmath acc .= $(accumulator_op(eltype(acc))).(acc, $(expand_product(length(xs),eltype(x1),:x1,:xs))))
end

# specialize for when there is a constant x0 involved, so that x1, xs... can still be multiplied as Bools. Make sure multiplication priorities are set correctly!
@inline @generated function accumulate_prod_unroll(acc::NArr, x0::Real, x1::NArr, xs::NArr...)
    :(@fastmath acc .= $(accumulator_op(eltype(acc))).(acc, x0 .* ($(expand_product(length(xs),eltype(x1),:x1,:xs)))))
end

@inline function accumulate_prod(acc::NArr, xs::AbstractVector{<:NArr})
    if length(xs) > max_unroll_products
        tmp = prod_fast(xs[max_unroll_products:end])
        accumulate_prod_unroll(acc, tmp, xs[1:max_unroll_products-1]...)
    else
        accumulate_prod_unroll(acc, xs...)
    end
end

@inline function accumulate_prod(acc::NArr, x1::Real, xs::NArr...)
    if length(xs) > max_unroll_products-1
        tmp = prod_fast(collect(xs)[max_unroll_products-1:end])
        accumulate_prod_unroll(acc, x1, tmp, collect(xs)[1:max_unroll_products-2]...)
    else
        accumulate_prod_unroll(acc, x1, xs...)
    end
end


# assign a product into an accumulator argument and divide by a normalizer `z`

@inline @generated function assign_prod_normalized_unroll(acc::NArr, z::NArr,
    x1::NArr, xs::NArr...)
    x1ze = (eltype(z) <: Bool) ? :x1 : :(replace_nan.(x1 ./ z)) # Bool z is always equal to 0 or 1, and hence does nothing useful for flows (assuming z > x1)
    # use fastmath for all but the division where we require proper NaN handling
    :(acc .= $(expand_product(length(xs),eltype(acc),x1ze,:xs)))
end

@inline function assign_prod_normalized(acc::NArr, z::NArr,
x1::NArr, xs::AbstractArray{<:NArr})
    if length(xs) > max_unroll_products
        tmp = prod_fast(xs[max_unroll_products:end])
        assign_prod_normalized_unroll(acc, z , x1, tmp, xs[1:max_unroll_products-1]...)
    else
        assign_prod_normalized_unroll(acc, z , x1, xs...)
    end
end


# accumulate a product into an accumulator argument and divide by a normalizer `z`

@inline @generated function accumulate_prod_normalized_unroll(acc::NArr, z::NArr,
                                                        x1::NArr, xs::NArr...)
    x1ze = (eltype(z) <: Bool) ? :x1 : :(replace_nan.(x1 ./ z)) # Bool z is always equal to 0 or 1, and hence does nothing useful for flows (assuming z > x1)
    # use fastmath for all but the division where we require proper NaN handling
    :(acc .= $(accumulator_op(eltype(acc))).(acc, $(expand_product(length(xs),eltype(acc),x1ze,:xs))))
end

@inline function accumulate_prod_normalized(acc::NArr, z::NArr,
                                        x1::NArr, xs::AbstractArray{<:NArr})
    if length(xs) > max_unroll_products
        tmp = prod_fast(xs[max_unroll_products:end])
        accumulate_prod_normalized_unroll(acc, z , x1, tmp, xs[1:max_unroll_products-1]...)
    else
        accumulate_prod_normalized_unroll(acc, z , x1, xs...)
    end
end



# Specialized version of sum_of_products for each 2-layer subcircuit
# Adds a lot of compile time but no noticable speedup over the 1-layer operators above
# test: spe3(Float16,Tuple{NTuple{3,Float16},NTuple{2,Float16},NTuple{2,Float16}})
@generated function sum_of_products(y, ps::Tuple)
    sum = expand_product(length(ps.parameters[1].parameters), Float32, :(ps[1]))
    for i in 2:length(ps.parameters)
        prod = expand_product(length(ps.parameters[i].parameters), Float32, :(ps[$i]))
        sum = :($(accumulator_op(eltype(y))).($sum, $prod))
    end
    # println(:(y .= $sum))
    :(y .= $sum)
end


#TODO: make this method rebust to very large xs sets as above
@inline @generated function count_conjunction(x1::BitVector, xs::BitVector...)::UInt32
    :(count($(expand_product(length(xs),eltype(x1),:x1,:xs))))
end

#TODO: make this method rebust to very large xs sets as above
# @inline @generated function sum_weighted_product(weights::NArr, x1::NArr, xs::NArr...)
#     :(sum($(expand_product(length(xs),eltype(x1),:x1,:xs)) .* weights))
# end
@inline @generated function sum_weighted_product(weights::NArr, x1::NArr, xs::NArr...)
    :(sum(weights[$(expand_product(length(xs),eltype(x1),:x1,:xs))]))
end
