module Main where

import Lib

newOr a b = if a then a else b

main :: IO ()
main = do
        let numbers = [1,2,3,4]
        let second = [5,6,7]
        let concatenated = numbers ++ second
        print (concatenated)
    

