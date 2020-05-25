| Build Status                                                                                                                                                                                                                                                                       	|                                              Documentation                                             	|
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	|:------------------------------------------------------------------------------------------------------:	|
| [![Build Status](https://travis-ci.org/Juice-jl/LogicCircuits.jl.svg?branch=master)](https://travis-ci.org/Juice-jl/LogicCircuits.jl)	 [![codecov](https://codecov.io/gh/Juice-jl/LogicCircuits.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/Juice-jl/LogicCircuits.jl) 	| [![](https://img.shields.io/badge/docs-stable-green.svg)](https://juice-jl.github.io/LogicCircuits.jl/stable) [![](https://img.shields.io/badge/docs-dev-blue.svg)](https://juice-jl.github.io/LogicCircuits.jl/dev) 	|

# LogicCircuits.jl
Logic Circuits - part of Juice (Julia Circuit Empanada)

## Installation

To install the latest stable release, run:

```bash
julia -e 'using Pkg; Pkg.add("LogicCircuits")'
```

To install the package with the latest commits on master branch, run:

```bash
julia -e 'using Pkg; Pkg.add(PackageSpec(url="https://github.com/Juice-jl/LogicCircuits.jl.git"))'
```

These will automatically install all dependencies described in `Project.toml`.
The first time you run `using LogicCircuits` in Julia, it will precompile the package and all its dependencies.

## Testing
To make sure everything is working correctly, you can run our test suite as follows. The first time you run the tests will trigger a few slow downloads of various test resources.

```bash
julia --color=yes -e 'using Pkg; Pkg.test("LogicCircuits")'
```

## Documentation

To build the documentation locally, follow the instructions in the [docs Readme](docs/Readme.md).

## Development

If you are interested in modifying the package please see the [development Readme](README_DEV.md).

## Troubleshooting

If the MLDataSets package fails to build, install the following (Ubuntu):

```bash
sudo apt-get install zlib1g-dev libncurses5-dev
```
