function parseinputline(line::String)
    m = match(r"^(?<command>.*) (?<xmin>[0-9]+),(?<ymin>[0-9]+) through (?<xmax>[0-9]+),(?<ymax>[0-9]+)$", line)
    return (
        command=m["command"],
        xrange=parse(Int, m["xmin"]):parse(Int, m["xmax"]),
        yrange=parse(Int, m["ymin"]):parse(Int, m["ymax"]),
    )
end

Ranges = Vector{UnitRange{Int}}

function getsubranges(ranges::Ranges)
    starts = Set([range.start for range in ranges])
    afterstops = Set([range.stop + 1 for range in ranges])
    rangestarts = starts ∪ afterstops
    sortedstarts = sort(collect(rangestarts))
    return [
        sortedstarts[i]:(sortedstarts[i+1]-1)
        for i in firstindex(sortedstarts):(lastindex(sortedstarts)-1)
    ]
end

# Instruction = NamedTuple{(:command, :xrange, :yrange), Tuple{SubString{String}, AbstractRange, AbstractRange}}

function countlights(instructions)
    turnedon = Set{Rectangle}()
    xsubranges = getsubranges(map(i -> i.xrange, instructions))
    ysubranges = getsubranges(map(i -> i.yrange, instructions))
    for instruction in instructions
        executeinstruction!(turnedon, instruction, xsubranges, ysubranges)
    end
    return sum(area.(turnedon))
end

Rectangle = Tuple{AbstractRange, AbstractRange}

area(rect::Rectangle) = length(rect[1]) * length(rect[2])

function executeinstruction!(
    turnedon::Set{Rectangle},
    instruction::Any,
    xsubranges::Ranges,
    ysubranges::Ranges,
)
    valid_xsubranges = filter(r -> r ⊆ instruction.xrange, xsubranges)
    valid_ysubranges = filter(r -> r ⊆ instruction.yrange, ysubranges)
    for xsubrange in valid_xsubranges
        for ysubrange in valid_ysubranges
            rect = (xsubrange, ysubrange)
            if rect ∈ turnedon && instruction.command in ["turn off", "toggle"]
                delete!(turnedon, rect)
            elseif rect ∉ turnedon && instruction.command in ["turn on", "toggle"]
                push!(turnedon, rect)
            end
        end
    end
end

function totalbrightness(instructions)
    brightness = Dict{Rectangle, Int}()
    xsubranges = getsubranges(map(i -> i.xrange, instructions))
    ysubranges = getsubranges(map(i -> i.yrange, instructions))
    for instruction in instructions
        executebrightnessinstruction!(brightness, instruction, xsubranges, ysubranges)
    end
    return sum(p -> area(p[1]) * p[2], pairs(brightness))
end

function executebrightnessinstruction!(
    brightness::Dict{Rectangle, Int},
    instruction::Any,
    xsubranges::Ranges,
    ysubranges::Ranges,
)
    valid_xsubranges = filter(r -> r ⊆ instruction.xrange, xsubranges)
    valid_ysubranges = filter(r -> r ⊆ instruction.yrange, ysubranges)
    for xsubrange in valid_xsubranges
        for ysubrange in valid_ysubranges
            rect = (xsubrange, ysubrange)
            if instruction.command == "turn off" && haskey(brightness, rect)
                brightness[rect] = max(brightness[rect] - 1, 0)
            elseif instruction.command in ["turn on", "toggle"]
                inc = ifelse(instruction.command == "turn on", 1, 2)
                if !haskey(brightness, rect)
                    brightness[rect] = 0
                end
                brightness[rect] += inc
            end
        end
    end
end


instructions = map(parseinputline, readlines())
println(countlights(instructions))
println(totalbrightness(instructions))
