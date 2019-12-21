## Building Docs

If you are inside the `docs` folder, run the following command: 

    julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate(); include("make.jl");'
    
You can see the results under `/docs/build`.
