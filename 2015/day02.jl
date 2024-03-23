parseinput() = map(parseline, readlines())

parseline(s::String) = map(i -> parse(Int, i), split(s, 'x'))


function wrappingpaperarea(dim)
    x, y, z = sort(dim)
    return 3*x*y + 2*x*z + 2*y*z
end


function ribbonlength(dim)
    x, y, z = sort(dim)
    return 2*x + 2*y + x*y*z
end


dims = parseinput()
println(sum(map(wrappingpaperarea, dims)))
println(sum(map(ribbonlength, dims)))
