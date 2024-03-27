import Permutations

Distances = Dict{SubString{String}, Dict{SubString{String}, Int}}

function parseinput(lines::Vector{String})
    distances = Distances()

    for line in lines
        loc1, loc2, distance = parseline(line)
        if !haskey(distances, loc1)
            distances[loc1] = Dict{SubString{String}, Int}()
        end
        distances[loc1][loc2] = distance
        if !haskey(distances, loc2)
            distances[loc2] = Dict{SubString{String}, Int}()
        end
        distances[loc2][loc1] = distance
    end

    return distances
end

function parseline(s::String)
    m = match(r"^(?<loc1>.*) to (?<loc2>.*) = (?<distance>.*)$", s)
    return m["loc1"], m["loc2"], parse(Int, m["distance"])
end

function get_total_distances(distances::Distances)
    totals = []
    locations = collect(keys(distances))
    for permutation in Permutations.PermGen(length(distances))
        total = 0
        for i in 1:(lastindex(locations) - 1)
            total += distances[locations[permutation[i]]][locations[permutation[i + 1]]]
        end
        append!(totals, total)
    end
    return totals
end


distances = parseinput(readlines())
total_distances = get_total_distances(distances)
println(min(total_distances...))
println(max(total_distances...))