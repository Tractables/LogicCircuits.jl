using Documenter
using DocumenterLaTeX
using LogicCircuits
using Literate

#######################################
# 1/ generate the top-level README.md
#######################################

source_dir = "$(@__DIR__)/src"
# also make it available to Literate.jl scripts through an environment variable
ENV["JUICE_MAKE_DOC_SRC"] = source_dir

"replace script includes with file content in Literate code"
function replace_includes(str)
    pat = r"include\(\"(.*)\"\)"
    m = match(pat, str)
    while !isnothing(m)
        str = replace(str, "$(m.match)" =>
                read("$source_dir/$(m[1])", String))
        m = match(pat, str)
    end
    str
end

"hide `#plot` lines in Literate code"
function hide_plots(str)
    str = replace(str, r"#plot (.)*[\n\r]" => "")
    replace(str, r"#!plot (.*)[\n\r]" => s"\g<1>\n")
end

"show `#plot` lines in Literate code"
function show_plots(str)
    str = replace(str, r"#!plot (.)*[\n\r]" => "")
    replace(str, r"#plot (.*)[\n\r]" => s"\g<1>\n")
end

Literate.markdown("$source_dir/README.jl", "$(@__DIR__)/../"; documenter=false, credit=false, execute=true, 
    preprocess = hide_plots ∘ replace_includes)

#######################################
# 2/ generate notebooks
#######################################

Literate.notebook("$source_dir/usage.jl", "$source_dir/generated"; credit=false, execute=true,
    preprocess = show_plots ∘ replace_includes)

#######################################
# 3/ generate the documentation HTML
#######################################

pages = [
    "Home" => "index.md",
    "Usage" => "generated/usage.md",
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
    "Development" => "development.md",
]

format = Documenter.HTML(
    prettyurls = !("local" in ARGS),
    canonical = "https://juice-jl.github.io/LogicCircuits.jl/stable/",
    assets = ["assets/favicon.ico"],
    analytics = "UA-136089579-2",
    highlights = ["yaml"],
    collapselevel = 1,
)

Literate.markdown("$source_dir/usage.jl", "$source_dir/generated"; documenter=true, credit=false,
    preprocess = show_plots)

makedocs(
    sitename = "LogicCircuits.jl",
    pages    = pages,
    format   = format,
    doctest  = true,
    modules  = [LogicCircuits],
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