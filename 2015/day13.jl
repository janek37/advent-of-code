using DataStructures
using Permutations

function parseline(s::String)
    m = match(r"^(.+) would (.+) (\d+) happiness units by sitting next to (.+).", s)
    guest1, sign, value, guest2 = m
    value = parse(Int, value)
    change = ifelse(sign == "gain", 1, -1) * value
    return guest1, guest2, change
end

Neighbors = Dict{AbstractString, Int}
Graph = DefaultDict{AbstractString, Neighbors}

function makegraph(lines::Vector{String})
    graph = Graph(Neighbors)
    for line in lines
        guest1, guest2, change = parseline(line)
        graph[guest1][guest2] = change
    end
    return graph
end

function totalchanges(graph::Graph; circular::Bool)
    guests = collect(keys(graph))
    totals = Vector{Int}()
    for permutation in PermGen(lastindex(guests))
        total = 0
        for i in firstindex(guests):(lastindex(guests) - 1 + circular)
            guest1 = guests[permutation[i]]
            i2 = ifelse(i == lastindex(guests), 1, i + 1)
            guest2 = guests[permutation[i2]]
            total += graph[guest1][guest2] + graph[guest2][guest1]
        end
        append!(totals, total)
    end
    return totals
end


graph = makegraph(readlines())
println(max(totalchanges(graph, circular=true)...))
println(max(totalchanges(graph, circular=false)...))
