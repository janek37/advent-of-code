function unescaped_length(s::String)
    s1 = s[firstindex(s)+1:lastindex(s)-1]
    return partial_unescaped_length(s1)
end

function partial_unescaped_length(s::AbstractString, from::Int = 0)
    if s == ""
        return 0
    end
    if s[1] != '\\'
        newoffset = 1
    elseif s[2] in ['\\', '"']
        newoffset = 2
    elseif s[2] == 'x'
        newoffset = 4
    else
        throw(ArgumentError)
    end
    return 1 + partial_unescaped_length(s[firstindex(s)+newoffset:lastindex(s)])
end

length_overhead(s::String) = length(s) - unescaped_length(s)

length_overoverhead(s::String) = count(ch -> ch in ['\\', '"'], s) + 2



strings = readlines()
println(sum(length_overhead(s) for s in strings))
println(sum(length_overoverhead(s) for s in strings))
