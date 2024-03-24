function isnice(s::String)
    vowelscount = count(ch -> ch in "aeiou", s)
    if vowelscount < 3 return false end
    if !any(i -> s[i] == s[i+1], firstindex(s):(lastindex(s) - 1))
        return false
    end
    return !any(forbidden -> occursin(forbidden, s), ["ab", "cd", "pq", "xy"])
end

function isnice2(s::String)
    if !any(i -> occursin(s[i:i+1], s[i+2:lastindex(s)]), firstindex(s):(lastindex(s) - 3))
        return false
    end
    return any(i -> s[i] == s[i+2], firstindex(s):(lastindex(s) - 2))
end

strings = readlines()
println(count(isnice, strings))
println(count(isnice2, strings))
