-- testing pattern-matching

data OpWithArg a = Operation (a -> a) a
oApply :: OpWithArg a -> OpWithArg a
oApply (Operation f n) = Operation f (f n)
getArg :: OpWithArg a -> a
getArg (Operation _ n) = n

squareTwo = Operation (\x -> x * x) 2
squareFour = oApply squareTwo
squareSixteen = oApply $ oApply squareTwo

main :: IO ()
main = print $ getArg squareSixteen