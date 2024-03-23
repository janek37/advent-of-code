function findfloor(s::String)
    upcount = count(ch -> (ch == '('),  s)
    downcount = count(ch -> (ch == ')'),  s)
    return upcount - downcount
end

function findbasement(s::String)
    current = 0
    for (i, ch) in enumerate(s)
        if ch == '('
            current += 1
        elseif ch == ')'
            current -= 1
        end
        if current < 0
            return i
        end
    end
end

instructions = readline()
println(findfloor(instructions))
println(findbasement(instructions))
