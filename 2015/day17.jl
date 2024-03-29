function count_cointainer_combinations(containers::Vector{Int}, total::Int, count::Union{Int, Nothing} = nothing)
    if total < 0 || sum(containers) < total || (!isnothing(count) && count < 0)
        return 0
    end
    if total == 0
        return ifelse(isnothing(count), 1, count == 0)
    end
    container = containers[firstindex(containers)]
    rest = containers[(firstindex(containers)+1):lastindex(containers)]
    count_minus_one = isnothing(count) ? nothing : count - 1
    return count_cointainer_combinations(rest, total, count) + count_cointainer_combinations(rest, total - container, count_minus_one)
end

function count_minimal_cointainer_combinations(containers::Vector{Int}, total::Int)
    for count in 1:length(containers)
        c = count_cointainer_combinations(containers, total, count)
        if c > 0
            return c
        end
    end
end



containers = parse.(Int, readlines())
println(count_cointainer_combinations(containers, 150))
println(count_minimal_cointainer_combinations(containers, 150))
