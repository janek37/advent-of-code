using Combinatorics

function find_min_qe(weights::Vector{Int}, group_count::Int)
    s = sum(weights) ÷ group_count
    min_group_size = guess_min_group_size(weights, s)
    qe_values = map(
        prod,
        # not 100% general, but should work in normal cases
        filter(
            (group -> sum(group) == s),
            collect(combinations(weights, min_group_size)),
        ),
    )
    return min(qe_values...)
end

# not 100% general, but should work in normal cases
function guess_min_group_size(weights::Vector{Int}, s::Int)
    for k in 1:length(weights)
        l = k*2 - (s % 2)
        if sum(weights[lastindex(weights)-l+1:lastindex(weights)]) ≥ s
            return l
        end
    end
end

weights = parse.(Int, readlines())
println(find_min_qe(weights, 3))
println(find_min_qe(weights, 4))
