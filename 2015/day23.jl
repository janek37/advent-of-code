Instruction = @NamedTuple{op::Symbol, register::Char, offset::Int}

function parseline(s::String)
    m = match(r"(hlf|tpl|inc) ([ab])", s)
    if !isnothing(m)
        op, reg = m
        return (op=Symbol(op), register=reg[1], offset=0)
    end
    m = match(r"jmp ([+-]\d+)", s)
    if !isnothing(m)
        return (op=:jmp, register='0', offset=parse(Int, m[1]))
    end
    m = match(r"(ji[oe]) ([ab]), ([+-]\d+)", s)
    if !isnothing(m)
        op, reg, offset = m
        return (op=Symbol(op), register=reg[1], offset=parse(Int, offset))
    end
end

State = @NamedTuple{registers::Dict{Char, BigInt}, instruction_pointer::Int}

function execute(instruction::Instruction, state::State)
    op = instruction.op
    if op == :hlf
        state.registers[instruction.register] ÷= 2
    elseif op == :tpl
        state.registers[instruction.register] *= 3
    elseif op == :inc
        state.registers[instruction.register] += 1
    elseif op == :jmp || (op == :jie && state.registers[instruction.register] % 2 == 0) || (op == :jio && state.registers[instruction.register] == 1)
        return (registers=state.registers, instruction_pointer=state.instruction_pointer + instruction.offset)
    end
    return (registers=state.registers, instruction_pointer=state.instruction_pointer + 1)
end

function run(instructions::Vector{Instruction}, state::State)
    while 1 ≤ state.instruction_pointer ≤ length(instructions)
        state = execute(instructions[state.instruction_pointer], state)
    end
    return state
end


instructions = parseline.(readlines())
println(run(instructions, (registers=Dict{Char, BigInt}('a' => 0, 'b' => 0), instruction_pointer=1)).registers['b'])
println(run(instructions, (registers=Dict{Char, BigInt}('a' => 1, 'b' => 0), instruction_pointer=1)).registers['b'])
