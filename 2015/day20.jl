function factorize(n::Int)
    m = n
    factors = Vector{Pair{Int, Int}}()
    for k in 2:Int(floor(sqrt(n)))
        r = 0
        while m % k == 0
            m = m ÷ k
            r += 1
        end
        if r > 0
            append!(factors, [k => r])
        end
    end
    return factors
end

function getallfactors(factors::Vector{Pair{Int, Int}})
    if length(factors) == 0
        return [1]
    end
    prime, power = factors[1]
    allfactors = Vector{Int}()
    for f in getallfactors(factors[2:lastindex(factors)])
        for pow in 0:power
            append!(allfactors, prime^pow * f)
        end
    end
    return allfactors
end

function find_smallest_int_with_sum_of_factors_at_least(n::Int)
    for k in 1:n
        if sum(getallfactors(factorize(k))) > n
            return k
        end
    end
end

get_special_factors(n::Int) = n .÷ filter(i -> n % i == 0, 1:50)

function find_smallest_int_with_sum_of_special_factors_at_least(n::Int)
    for k in 1:n
        if sum(get_special_factors(k)) > n
            return k
        end
    end
end


n = parse(Int, readline())
println(find_smallest_int_with_sum_of_factors_at_least(n ÷ 10))
println(find_smallest_int_with_sum_of_special_factors_at_least((n - 1) ÷ 11 + 1))
