using DataStructures

function parseline(s::String)
    m = match(r"^.* can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.$", s)
    speed, duration, rest = m
    return (speed=parse(Int, speed), duration=parse(Int, duration), rest=parse(Int, rest))
end

Reindeer = NamedTuple{(:speed, :duration, :rest), Tuple{Int, Int, Int}}

winning_distance(reindeers::Vector{Reindeer}, time::Int) = max(map(r -> distance(r, time), reindeers)...)

function distance(reindeer::Reindeer, time::Int)
    cycle = reindeer.duration + reindeer.rest
    fullcycles = time ÷ cycle
    remainder = time % cycle
    return fullcycles * reindeer.speed * reindeer.duration + min(reindeer.duration, remainder) * reindeer.speed
end

winner(reindeers::Vector{Reindeer}, time::Int) = max(map(r -> (distance(r, time), r), reindeers)...)[2]

function findleader(reindeers::Vector{Reindeer}, time::Int)
    points = DefaultDict{Reindeer, Int}(0)
    for t in 1:time
        points[winner(reindeers, t)] += 1
    end
    return max(values(points)...)
end


reindeers = parseline.(readlines())
println(winning_distance(reindeers, 2503))
println(findleader(reindeers, 2503))
