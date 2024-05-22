import Data.List

typical = ["African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher"]

filterTypical :: [String] -> [String]
filterTypical strings = nub $ filter (`notElem` typical) strings

main::IO()
main = do
    print $ filterTypical ["Mallard", "Hook Bill", "African", "Crested", "Pilgrim", "Toulouse", "Blue Swedish"] == ["Mallard", "Hook Bill", "Crested", "Blue Swedish"]
    print $ filterTypical ["Mallard", "Barbary", "Hook Bill", "Blue Swedish", "Crested"] == ["Mallard", "Barbary", "Hook Bill", "Blue Swedish", "Crested"]
    print $ filterTypical ["African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher"] == []
