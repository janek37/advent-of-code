Rule = Pair{<:AbstractString, <:AbstractString}

function parseinput(lines::Vector{String})
    rules = parserule.(lines[firstindex(lines):lastindex(lines)-2])
    molecule = lines[lastindex(lines)]
    return rules, molecule
end

function parserule(s::String)
    pattern, replacement = match(r"^(.*) => (.*)$", s)
    return pattern => replacement
end

function newmolecules(molecule::String, rules::Vector{<:Rule})
    molecules = Set{AbstractString}()
    for rule in rules
        union!(molecules, applyrule(molecule, rule)) 
    end
    return molecules
end

function applyrule(molecule::String, rule::Rule)
    molecules = Set{AbstractString}()
    for match in eachmatch(Regex(rule[1]), molecule)
        newmolecule = molecule[1:match.offset - 1] * rule[2] * molecule[match.offset + length(match.match):lastindex(molecule)]
        push!(molecules, newmolecule)
    end
    return molecules
end

# not general, but works for my input
function find_step_count(rules::Vector{<:Rule}, molecule::String)
    if molecule == "e"
        return 0
    end
    previous_options = newmolecules(molecule, rules)
    return 1 + find_step_count(rules, first(previous_options))
end

reverse_rules(rules::Vector{<:Rule}) = [rule[2] => rule[1] for rule in rules]


rules, molecule = parseinput(readlines())
println(length(newmolecules(molecule, rules)))
println(find_step_count(reverse_rules(rules), molecule))
