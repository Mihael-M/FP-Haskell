import Data.List
canPass :: Criterion -> (StudentRecord -> Bool)
canPass (maxMisses, maxLates) = (\record -> (length (filter (== Absent) record) <= maxMisses) && (length (filter (== Late) record) <= maxLates))

type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)

data Attendance = Absent | Late | Present
 deriving (Eq)
type StudentRecord = [Attendance]
cP = canPass (1, 2)
main::IO()
main = do
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True -- трябва да е False...
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False