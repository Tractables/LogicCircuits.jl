using Test
using LogicCircuits


@testset "Unbatch test" begin # wrapping all the statements into a function also trigers signal (4) due to `XDataset`
    for name in ["msnbc", "jester", "plants", "ad"]
        one_batch_set = dataset(twenty_datasets(name); do_shuffle=false, batch_size=-1)
        batches_set = dataset(twenty_datasets(name); do_shuffle=false, batch_size=100)
        unbatched_set = XDataset(unbatch(batches_set)...)
        for f in [train, valid, test]
            # unweighted data
            @test feature_matrix(f(unbatched_set)) == feature_matrix(f(one_batch_set))
    
            # weighted data
            w = rand(num_examples(f(one_batch_set)))
            w_one_batch = WXData(f(one_batch_set), w)
    
            w_batches = batch(w_one_batch, 100)
            w_unbatched = unbatch(w_batches)

            @test w_one_batch.xd.x == w_unbatched.xd.x
            @test w_one_batch.w == w_unbatched.w
        end
    end
end

