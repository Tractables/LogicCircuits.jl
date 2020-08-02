using Documenter
using DocumenterLaTeX
using LogicCircuits

# The DOCSARGS environment variable can be used to pass additional arguments to make.jl.
# This is useful on CI, if you need to change the behavior of the build slightly but you
# can not change the .travis.yml or make.jl scripts any more (e.g. for a tag build).
if haskey(ENV, "DOCSARGS")
    for arg in split(ENV["DOCSARGS"])
        (arg in ARGS) || push!(ARGS, arg)
    end
end


const pages = [
    "Home" => "index.md",
    "Installation" => "installation.md",
    "Manual" => [
        "manual/properties.md",
        "manual/transformations.md",
        "manual/queries.md",            
        "manual/compilation.md"
    ],
    "API" => [
        "api/public.md",
        "api/types.md",
        "Internals" => map(
            s -> "api/internals/$(s)",
            sort(readdir(joinpath(@__DIR__, "src/api/internals")))
        ),
    ],
]

const format = if ("pdf" in ARGS)
    LaTeX(platform  = "native")
else
    format = Documenter.HTML(
        prettyurls = !("local" in ARGS),
        canonical = "https://juice-jl.github.io/LogicCircuits.jl/stable/",
        assets = ["assets/favicon.ico"],
        analytics = "UA-136089579-2",
        highlights = ["yaml"],
        collapselevel = 1,
    )
end

makedocs(
    sitename = "LogicCircuits.jl",
    format   = format,
    doctest  = true,
    modules  = [LogicCircuits],
    pages    = pages,
    linkcheck_ignore = [
        # We'll ignore links that point to GitHub's edit pages, as they redirect to the
        # login screen and cause a warning:
        r"https://github.com/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)/edit(.*)"
    ], 
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