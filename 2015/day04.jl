using MD5

function isgoodhash(prefix::String, n::Int, len::Int)
    s = "$prefix" * "$(n)"
    hash = bytes2hex(md5(s))
    return startswith(hash, '0'^len)
end

function findhash(prefix::String, len::Int)
    i = 0
    while !isgoodhash(prefix, i, len)
        i += 1
    end
    return i
end


prefix = readline()
println(findhash(prefix, 5))
println(findhash(prefix, 6))
