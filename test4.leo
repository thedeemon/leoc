type indata = {
 len : int
 str : byte[len]
 ints : int[10]
 par : int
 outstr : byte[len]
}
 
args : indata

print(args.len)
print(args.par)

for i in args.ints.range
 args.ints[i] <- i+1
end

for i in args.str.range
 args.outstr[i] <- args.str[|args.str| - 1 - i]
end

x = 20/5/4

