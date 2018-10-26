import qualified Countdown as Countdown

main = do
  let solutions = Countdown.solutions [1, 3, 7, 10, 25, 50] 765
  print solutions
  print (length solutions)
