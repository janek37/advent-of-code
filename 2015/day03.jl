@enum Direction north south west east

function parsedirection(ch::Char)
    if ch == '^' return north end
    if ch == 'v' return south end
    if ch == '<' return west end
    if ch == '>' return east end
end

function getoffset(direction::Direction)
    if direction == north return (0, -1) end
    if direction == south return (0, 1) end
    if direction == west return (-1, 0) end
    if direction == east return (1, 0) end
end

function getplaces(directions)
    current = (0, 0)
    places = Set([current])
    for direction in directions
        current = current .+ getoffset(direction)
        push!(places, current)
    end
    return places
end

function getroboplaces(directions)
    santaplaces = getplaces(directions[firstindex(directions):2:lastindex(directions)])
    roboplaces = getplaces(directions[firstindex(directions)+1:2:lastindex(directions)])
    return santaplaces ∪ roboplaces
end

directions = map(parsedirection, collect(readline()))
println(length(getplaces(directions)))
println(length(getroboplaces(directions)))
