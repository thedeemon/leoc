type point = { x :int; y : int }

type indata = {
 len : int
 str : byte[len]
 ints : int[10]
 p : point
 outstr : byte[len]
}
 
args : indata

print(args.len)

for i in args.ints.range
 args.ints[i] <- i+1
end

for i in args.str.range
 args.outstr[i] <- args.str[|args.str| - 1 - i]
end

args.p.x <- 2000/5/4
