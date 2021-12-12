# I needed some more knowledge about bits before I could start day three.

somebit = "10101"
someNumber = parse(UInt8, somebit, base=2)
                                                        # comments were for Int not UInt!
println(somebit)                                        # 10101
println(someNumber)                                     # 21
notSomeNumber = ~someNumber                             

println(notSomeNumber)                                  # -22 === 8 mod 10
println(string(notSomeNumber, base=2))                  # -10110 
                                                        #   where 10110 <=> 22 === 2 mod 10 
                                                        #                => -2 === 8 mod 10

someotherbit = "01010"
someOtherNumber = parse(UInt8, someotherbit, base=2)
println(someotherbit)                                   # 01010
println(someOtherNumber)                                # 10


println(string(someNumber ⊻ parse(UInt8, "11111", base=2), base=2)) # yields 01010 <=> 1010


# Remind me of some conversion stuff by hand...
#
# 10101
# 2^0 * 1 + 2^1 * 0 + 2^2 * 1 + 2^3 * 0 + 2^4 * 1 = 1 + 4 + 16 = 21
# 2^0 * 0 + 2^1 * 1 + 2^2 * 0 + 2^3 * 1 + 2^4 * 0 = 2 + 8      = 10
# 
# 21 / 2    = 10 rest 1 
# 10 / 2    = 5  rest 0
# 5  / 2    = 2  rest 1 
# 2  / 2    = 1  rest 0 
# 1  / 2    = 0  rest 1
# 0  / 2    = 0  rest 0
# => 010101 <=> 10101