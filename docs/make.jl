using Documenter

# include("../src/LogicCircuits.jl")
using LogicCircuits

makedocs(
    sitename = "LogicCircuits.jl Documentation",
    format = Documenter.HTML(prettyurls = false),
    doctest = true,
    modules = [LogicCircuits],
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
    target = "build",
    repo = "github.com/Juice-jl/LogicCircuits.jl.git",
    branch = "gh-pages",
    devbranch = "master",
    devurl = "dev",
    versions = ["stable" => "v^", "v#.#"],
)