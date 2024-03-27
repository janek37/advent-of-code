# I've actually solved this one manually, the following is not general, but works on my input and on other inputs I've encountered

nextpassword(s::String) = s[1:3] * "xxyzz"

nextnextpassword(s::String) = s[1:2] * Char(Int(s[3]) + 1) * "aabcc"


oldpassword = readline()
println(nextpassword(oldpassword))
println(nextnextpassword(oldpassword))
