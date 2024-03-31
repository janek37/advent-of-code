function parseinput(s::String)
    row, column = [parse(Int, m.match) for m in eachmatch(r"\d+", s)]
    return row, column
end

rowcol_to_index(row::Int, column::Int) = (row+column-2)*(row+column-1)÷2 + column

nth_code(n::Int) = (powermod(252533, n-1, 33554393) * 20151125) % 33554393

code_at(row::Int, column::Int) = nth_code(rowcol_to_index(row, column))


row, column = parseinput(readline())
println(code_at(row, column))
