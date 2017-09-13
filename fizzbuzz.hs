lessThan20 :: Int -> String
lessThan20 n
	| n>0 && n<20 =
	let answers = ["one", "two", "three","four", "five", "six"]
	in answers!!(n-1)


tens :: Int -> String
tens n 
	| n>=2 && n<=9 =
		answers!!(n-2)
		where answers = words("twenty thirty forty fifty sixty seventy eighty ninety")

numbers :: Int -> String
numbers n 
	| n < 20 = lessThan20 n
	| n  < 100 && ((n ´mod´ 10)==0) = tens (n ´div´ 10)
	|
	|