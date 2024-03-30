function parseinput(lines::Vector{String})
    width = length(lines[1])
    height = length(lines)
    board = BitArray(undef, (width, height))
    for (y, line) in enumerate(lines)
        for (x, char) in enumerate(line)
            board[x, y] = char == '#'
        end
    end
    return board
end

function nextstep(board::BitArray, stuck::Bool = false)
    width, height = size(board)
    nshift = [board[1:width, 2:height] zeros(Bool, width, 1)]
    sshift = [zeros(Bool, width, 1) board[1:width, 1:(height-1)]]
    wshift = [board[2:width, 1:height]; zeros(Bool, 1, height)]
    eshift = [zeros(Bool, 1, height); board[1:(width-1), 1:height]]
    nwshift = [nshift[2:width, 1:height]; zeros(Bool, 1, height)]
    neshift = [zeros(Bool, 1, height); nshift[1:(width-1), 1:height]]
    swshift = [sshift[2:width, 1:height]; zeros(Bool, 1, height)]
    seshift = [zeros(Bool, 1, height); sshift[1:(width-1), 1:height]]
    neighbors = @. nshift + sshift + wshift + eshift + nwshift + neshift + swshift + seshift
    newboard = nextstate.(board, neighbors)
    if stuck
        newboard[1, 1] = true
        newboard[1, height] = true
        newboard[width, 1] = true
        newboard[width, height] = true
    end
    return newboard
end

function nextstate(state::Bool, neighbors::Int)
    return neighbors == 3 || (state && neighbors == 2)
end

function runsteps(board::BitArray, steps::Int, stuck::Bool = false)
    for i in 1:steps
        board = nextstep(board, stuck)
    end
    return board
end


board = parseinput(readlines())
println(sum(runsteps(board, 100)))
println(sum(runsteps(board, 100, true)))
