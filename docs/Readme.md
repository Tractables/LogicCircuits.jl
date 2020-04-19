## Building Docs

To locally build the docs, run the following:

    julia --project=docs docs/make.jl

If you are inside the `docs` folder, you can run the following command instead: 

    julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate(); include("make.jl");'
    
You can see the results under `/docs/build`.



#### Note about Pretty URLs
For easier navigation for local builds its probably easier to disable pretty URLs. To disable that, go to `make.jl` and set `prettyurls = false`. For more information about pretty URLs, check out  [the documentation](https://juliadocs.github.io/Documenter.jl/stable/man/guide/) for `Documenter.jl`.