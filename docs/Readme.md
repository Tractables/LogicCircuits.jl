## Building Docs

To locally build the docs, run the following commands from root of the repository to Instantiate the docs environment and build the docs.

```
julia -e 'using Pkg; Pkg.activate("./docs"); Pkg.instantiate(); include("./docs/make.jl");'
```

The build results will be stored under `docs/build`.


Alternatively, if you have `LogicCircuits` in development mode and have already instantiated the docs environment, you can simply run the following:

```bash
    julia --project=docs docs/make.jl
```
    

#### Note about Pretty URLs
For easier navigation for local builds its probably easier to disable pretty URLs. To disable that, go to `make.jl` and set `prettyurls = false`. For more information about pretty URLs, check out  [the documentation](https://juliadocs.github.io/Documenter.jl/stable/man/guide/) for `Documenter.jl`.