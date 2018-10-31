import qualified Countdown as Countdown
import qualified Data.List as List

main = do
  let solutions = List.sort $ Countdown.solutions [1, 3, 7, 10, 25, 50] 765
  print (length solutions)
  mapM_ print solutions
