#src Generate README.md by running `docs/make.jl` 

# <img align="right" width="180px" src="https://avatars.githubusercontent.com/u/58918144?s=200&v=4">

# <!-- DO NOT EDIT README.md directly, instead edit docs/README.jl and generate the markdown-->

# # Logic<wbr>Circuits<wbr>.jl

# [![Unit Tests](https://github.com/Tractables/LogicCircuits.jl/workflows/Unit%20Tests/badge.svg)](https://github.com/Tractables/LogicCircuits.jl/actions?query=workflow%3A%22Unit+Tests%22+branch%3Amaster) [![codecov](https://codecov.io/gh/Tractables/LogicCircuits.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/Tractables/LogicCircuits.jl) [![](https://img.shields.io/badge/docs-stable-green.svg)](https://Tractables.github.io/LogicCircuits.jl/stable) [![](https://img.shields.io/badge/docs-dev-blue.svg)](https://Tractables.github.io/LogicCircuits.jl/dev)

# This package provides basic functionality for doing logical reasoning using logical circuits. It has the stand-alone functionality illustrated below, and it serves as the logical foundations for other [Juice packages](https://github.com/Tractables) (Julia Circuit Empanada).

include("usage.jl")

# Please see [![](https://img.shields.io/badge/docs-stable-green.svg)](https://Tractables.github.io/LogicCircuits.jl/stable) or [![](https://img.shields.io/badge/docs-dev-blue.svg)](https://Tractables.github.io/LogicCircuits.jl/dev) for further details.

# ## Development

# If you are interested in modifying the package please see the [development readme](https://Tractables.github.io/LogicCircuits.jl/dev/development/).

# ## Acknowledgements

# To acknowledge this package, please cite:
# ```
# @inproceedings{DangAAAI21,
#     title   = {Juice: A Julia Package for Logic and Probabilistic Circuits},
#     author = {Dang, Meihua and Khosravi, Pasha and Liang, Yitao and Vergari, Antonio and Van den Broeck, Guy},
#     booktitle = {Proceedings of the 35th AAAI Conference on Artificial Intelligence (Demo Track)},
#     year    = {2021}
# }
# ```