[![Build Status](https://travis-ci.org/Juice-jl/LogicCircuits.jl.svg?branch=master)](https://travis-ci.org/Juice-jl/LogicCircuits.jl)	
[![codecov](https://codecov.io/gh/Juice-jl/LogicCircuits.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/Juice-jl/LogicCircuits.jl)	
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://juice-jl.github.io/LogicCircuits.jl/stable)


# LogicCircuits.jl
Logic Circuits - part of Juice (Julia Circuit Empanada)

## Installation

To install the latest stable release, run:

    julia -e 'using Pkg; Pkg.add("LogicCircuits")'

To install the package with the latest commits on master branch, run:

    julia -e 'using Pkg; Pkg.add(PackageSpec(url="https://github.com/Juice-jl/LogicCircuits.jl.git"))'

These will automatically install all dependencies described in `Project.toml`.
The first time you run `using LogicCircuits` in Julia, it will precompile the package and all its dependencies.

## Testing
To make sure everything is working correctly, you can run our test suite as follows. The first time you run the tests will trigger a few slow downloads of various test resources.

    julia --color=yes -e 'using Pkg; Pkg.test("LogicCircuits")'

## Documentation

To build the documentation locally, run the following and then open `docs/build/index.html`.

    julia -e 'using Pkg; Pkg.activate("./docs"); Pkg.instantiate(); include("docs/make.jl");

## Development

If you are interested in modifying the package please see the [development README](README_DEV.md).

## Troubleshooting

If the MLDataSets package fails to build, install the following (Ubuntu):

  ``sudo apt-get install zlib1g-dev libncurses5-dev``
