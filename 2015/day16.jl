Aunt = Dict{AbstractString, Int}

function parseline(s::String)
    sue_id, attributes = match(r"Sue (\d+): (.*)", s)
    sue_id = parse(Int, sue_id)
    attr_matches = match.(r"(.*): (.*)", split(attributes, ", "))
    attr_pairs = [attrname => parse(Int, value) for (attrname, value) in attr_matches]
    parsed_attributes = Aunt(attr_pairs...)
    return sue_id, parsed_attributes
end

match_aunt(aunt::Aunt, pattern::Aunt) = all(value == pattern[key] for (key, value) in aunt)

function match_aunt_better(aunt::Aunt, pattern::Aunt)
    for (key, value) in aunt
        if key ∈ ["cats", "trees"]
            if value ≤ pattern[key]
                return false
            end
        elseif key ∈ ["pomeranians", "goldfish"]
            if value ≥ pattern[key]
                return false
            end
        elseif pattern[key] != value
            return false
        end
    end
    return true
end


aunts = parseline.(readlines())
pattern = Aunt(
    "children" => 3,
    "cats" => 7,
    "samoyeds" => 2,
    "pomeranians" => 3,
    "akitas" => 0,
    "vizslas" => 0,
    "goldfish" => 5,
    "trees" => 3,
    "cars" => 2,
    "perfumes" => 1,
)
println(filter(aunt -> match_aunt(aunt[2], pattern), aunts)[1][1])
println(filter(aunt -> match_aunt_better(aunt[2], pattern), aunts)[1][1])
