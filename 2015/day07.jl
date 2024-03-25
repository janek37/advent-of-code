@enum GateType direct and or not lshift rshift

Base.@kwdef struct Gate
    type::GateType
    arg1::Union{UInt16, AbstractString}
    arg2::Union{UInt16, AbstractString, Nothing} = nothing
end


function parseline(s::String)
    lhs, output = split(s, " -> ")
    if occursin(r"^([0-9]+|[a-z]+)$", lhs)
        return output, Gate(type=direct, arg1=maybeparse(lhs))
    end
    m = match(r"^(?<arg1>[0-9]+|[a-z]+) (?<op>AND|OR) (?<arg2>[0-9]+|[a-z]+)$", lhs)
    if !isnothing(m)
        type = m["op"] == "AND" ? and : or
        return output, Gate(type=type, arg1=maybeparse(m["arg1"]), arg2=maybeparse(m["arg2"]))
    end
    m = match(r"^NOT (?<arg>[0-9]+|[a-z]+)$", lhs)
    if !isnothing(m)
        return output, Gate(type=not, arg1=maybeparse(m["arg"]))
    end
    m = match(r"^(?<arg1>[0-9]+|[a-z]+) (?<dir>L|R)SHIFT (?<arg2>[0-9]+|[a-z]+)$", lhs)
    if !isnothing(m)
        type = m["dir"] == "L" ? lshift : rshift
        return output, Gate(type=type, arg1=maybeparse(m["arg1"]), arg2=maybeparse(m["arg2"]))
    end
    throw(ArgumentError(lhs))
end

maybeparse(s::AbstractString) = occursin(r"^[0-9]+$", s) ? parse(UInt16, s) : s

function makegates(lines::Vector{String})
    gates = Dict{String, Gate}()
    for line in lines
        output, gate = parseline(line)
        gates[output] = gate
    end
    return gates
end

function evaloutput(wire::Union{UInt16, AbstractString}, gates::Dict{String, Gate}, cache::Dict{String, UInt16} = Dict{String, UInt16}())
    if typeof(wire) == UInt16
        return wire
    end
    if haskey(cache, wire)
        return cache[wire]
    end

    gate = gates[wire]
    value1 = evaloutput(gate.arg1, gates, cache)
    if gate.type == direct
        return value1
    elseif gate.type == and
        value = value1 & evaloutput(gate.arg2, gates, cache)
    elseif gate.type == or
        value = value1 | evaloutput(gate.arg2, gates, cache)
    elseif gate.type == not
        value = ~value1
    elseif gate.type == lshift
        value = value1 << evaloutput(gate.arg2, gates, cache)
    elseif gate.type == rshift
        value = value1 >>> evaloutput(gate.arg2, gates, cache)
    end
    cache[wire] = value
    return value
end


gates = makegates(readlines())
signal = evaloutput("a", gates)
println(signal)
gates["b"] = Gate(type=direct, arg1=signal)
println(evaloutput("a", gates))
