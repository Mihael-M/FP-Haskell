import Data.List
type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present deriving (Show, Eq)
type StudentRecord = [Attendance]

maxConsecutiveLates :: StudentRecord -> Int
maxConsecutiveLates record = maximum (map length (filter (\x -> head x == Late) (group record)))

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (maxMisses, maxConsecLates) = \record ->
  let absences = length (filter (== Absent) record)
      maxLatesInARow = maxConsecutiveLates record
  in absences <= maxMisses && maxLatesInARow <= maxConsecLates

cP = canPass (1, 2)

main :: IO ()
main = do
  print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
  print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
  print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False
