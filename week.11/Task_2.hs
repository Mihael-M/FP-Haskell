type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

highestCapital :: [Country] -> Name
highestCapital [] = error "No countries provided"
highestCapital countries = snd $ foldr (\(Country countryName _ cities) acc@(maxElevation, _) -> 
  let highestCityElevation = foldr (\(City _ el _) prevMax -> max el prevMax) 0 cities
  in if highestCityElevation > maxElevation then (highestCityElevation, countryName) else acc) (initialMax, initialName) countries
 where
  initialMax = maximum [el | Country _ _ cities <- countries, City _ el _ <- cities]
  initialName = let (Country name _ _) = head countries in name


main :: IO()
main = do
  print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"
  print $ highestCapital [ Country "Italy" "Rome" [City "Milan" 120 15, City "Rome" 150 16, City "Naples" 10 17]
                    , Country "Spain" "Madrid" [City "Madrid" 600 18, City "Barcelona" 20 19, City "Seville" 50 20]
                    , Country "Greece" "Athens" [City "Athens" 250 21, City "Thessaloniki" 5 22, City "Patras" 10 20]
                    ] == "Italy" --mytest
