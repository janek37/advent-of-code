import JSON

findnumbers(s::String) = map(m -> parse(Int, m.match), eachmatch(r"-?\d+", s))

find_numbers_without_red(data) = find_numbers_without_red!(data, Vector{Int}())

function find_numbers_without_red!(data, numbers::Vector{Int})
    if isa(data, AbstractDict)
        if "red" ∉ values(data)
            for value in values(data)
                find_numbers_without_red!(value, numbers)
            end
        end
    elseif isa(data, AbstractArray)
        for value in data
            find_numbers_without_red!(value, numbers)
        end
    elseif isa(data, Number)
        append!(numbers, data)
    end
    return numbers
end


input = readline()
println(sum(findnumbers(input)))
println(sum(find_numbers_without_red(JSON.parse(input))))
