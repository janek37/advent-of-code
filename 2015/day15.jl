function parseline(s::String)
    m = match(r".*: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)", s)
    return map(x -> parse(Int, x), m)
end

function find_best_score(ingredients::Vector{Vector{Int}}, totalamount::Int = 100, acc::Vector{Int} = zeros(Int, 5); requiredcalories::Union{Nothing, Int} = nothing)
    ingredient = ingredients[firstindex(ingredients)]
    if length(ingredients) == 1
        values = ingredient .* totalamount .+ acc
        score = prod(max.(values, zeros(length(ingredient)))[1:4])
        calories = values[5]
        if !isnothing(requiredcalories) && requiredcalories != calories
            return 0
        end
        return score
    else
        scores = Vector{Int}()
        rest = ingredients[(firstindex(ingredients) + 1):lastindex(ingredients)]

        for amount in 0:totalamount
            new_acc = ingredient .* amount .+ acc
            append!(scores, find_best_score(rest, totalamount - amount, new_acc, requiredcalories=requiredcalories))
        end
        return max(scores...)
    end
end


ingredients = parseline.(readlines())
println(find_best_score(ingredients))
println(find_best_score(ingredients, requiredcalories=500))
