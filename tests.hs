data Time = Time{ 
  hour :: Int ,
  minute :: Int 
} deriving Show


timeConventor :: Time -> Double
timeConventor (Time h m) = fromIntegral h + (fromIntegral m)*0.01 

main :: IO()
main = do
    let time = Time 12 22
    print time
    print (timeConventor time)