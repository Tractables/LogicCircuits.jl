# LogicCircuits.jl for Developers

Follow these instructions to install and use LogicCircuits.jl as a developer of the package.

## Installation

Install the Julia package in development mode by running

    julia -e 'using Pkg; Pkg.develop(PackageSpec(url="https://github.com/Tractables/LogicCircuits.jl.git"))'

By default this will install the package at `~/.julia/dev` and allow you to change the code there. See the [Pkg manual](https://julialang.github.io/Pkg.jl/v1/managing-packages/#Developing-packages-1) for more details. One can adjust the development directory using environment variables or simply create a symbolic link to/from your favorite development directory.


## Building Docs

To locally build the docs, first run the following command from root of the repository to instantiate the docs environment and run the build script:

```
julia --project=docs/ -e 'using Pkg; Pkg.develop(PackageSpec(path=pwd())); Pkg.instantiate(); include("./docs/make.jl");'
```

The build results will be stored under `docs/build`.

Alternatively, if you have `LogicCircuits` in development mode and have already instantiated the docs environment, you can simply run the following:

```bash
julia --project=docs docs/make.jl
``` 

## Updating main README.md

After running the build script for docs the README.md file gets updated automatically, check the diff and commit the desired changes to README.md.

## Testing

### Prerequisite
Set the following environment variable, to automatically download data artifacts needed during tests without user input. Otherwise the tests would fail or timeout waiting for user input if the artifact is not already downloaded.

    export DATADEPS_ALWAYS_ACCEPT=1

Additionally, if you want the tests to run faster, you can use more cores by setting the following variable. The default value is 1.

    export JIVE_PROCS=8

### Running the tests:
Make sure to run the tests before commiting new code.

To run all the tests:

    julia --project=test --color=yes test/runtests.jl

You can also run any specific test:

    julia --project=test --color=yes test/_manual_/aqua_test.jl
    
## Releasing New Versions

Only do this for when the repo is in stable position, and we have decent amount of changes from previous version.

1. Bump up the version in `Project.toml`
2. Use [Julia Registrator](https://github.com/JuliaRegistries/Registrator.jl) to submit a pull request to julia's public registry. 
    - The web interface seems to be the easiest. Follow the instructions in the generated pull request and make sure there is no errors. For example [this pull request](https://github.com/JuliaRegistries/General/pull/15349).
3. Github Release. TagBot is enabled for this repo, so after the registrator merges the pull request, TagBot automatically does a github release in sync with the registrar's new version. 
   - Note: TagBot would automatically include all the closed PRs and issues since the previous version in the release note, if you want to exclude some of them, refer to [Julia TagBot docs](https://github.com/JuliaRegistries/TagBot).
4. As much as possible, make sure to also release a new version for `ProbabilisticCircuits.jl`.


## Updating Artifacts

The example is for Circuit Model Zoo, but should work for others:

1. Push new updates to [UCLA-StarAI/Circuit-Model-Zoo](https://github.com/UCLA-StarAI/Circuit-Model-Zoo)
2. Do a [new zoo release](https://github.com/UCLA-StarAI/Circuit-Model-Zoo/releases).
3. Update the `LogicCircuits.jl`'s `Artifact.toml` file with new git tag and hash. Example commit can be found [here](https://github.com/Tractables/LogicCircuits.jl/commit/1cd3fda02fa7bd82d1fa02898ee404edce0d7b14).
4. Do the same for `ProbabilisticCircuits.jl`'s `Artifact.toml` file. Example commit [here](https://github.com/Tractables/ProbabilisticCircuits.jl/commit/da7d3678b5f2254e60229632f74cc619505e2b2d).
5. Note that for each Artifact.toml, 2 things need to change: `git-tree-sha1` and `sha256`.
6. Update the `const zoo_version = "/Circuit-Model-Zoo-0.1.4"` inside LogicCircuits.jl to the new zoo version. No changes needed in ProbabilisticCircuits since it uses the same constant.

### Question: How to get the hashes:

Download the new Zoo release from Github. Now you can use the following code snippet to get the hashes (check the [julia Artifact page](https://julialang.github.io/Pkg.jl/dev/artifacts/) for latest instructions):
```
using Tar, Inflate, SHA
filename = "socrates.tar.gz"
println("sha256: ", bytes2hex(open(sha256, filename)))
println("git-tree-sha1: ", Tar.tree_hash(IOBuffer(inflate_gzip(filename))))
```
