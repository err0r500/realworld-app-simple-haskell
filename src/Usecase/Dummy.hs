{-# LANGUAGE ExistentialQuantification #-}

module Usecase.Dummy where

import           ClassyPrelude

class WithColor a where
    hasColor :: Text -> a -> Bool

data Car = Car
    { _id       :: Text
    , _carColor :: Text
    } deriving (Show)

instance WithColor Car where
    hasColor = carColor

carColor :: Text -> Car -> Bool
carColor color c = color == _carColor c

data Bus = Bus
    { _plate    :: Text
    , _busColor :: Text
    } deriving (Show)

instance WithColor Bus where
    hasColor = busColor

busColor :: Text -> Bus -> Bool
busColor color c = color == _busColor c

data Showable =
    forall a. Show a =>
              MkShowable a

pack' :: Show a => a -> Showable
pack' = MkShowable

checkThis :: IO ()
checkThis =
    let cars = [Car "1" "blue", Car "2" "red", Car "3" "blue"]
        coolBlueCars = map (\blueCar -> blueCar {_carColor = "coolBlue"}) $ filter (hasColor "blue") cars
        buses = [Bus "a" "blue", Bus "b" "red", Bus "c" "blue"]
        coolBlueBuses = map (\blueBus -> blueBus {_busColor = "coolBlue"}) $ filter (hasColor "blue") buses
     in do
        printList coolBlueBuses
        printList coolBlueCars

printList :: Show a => [a] -> IO ()
printList = mapM_ (putStrLn . tshow)
