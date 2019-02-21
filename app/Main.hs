module Main where

import Lib
import Data.List


--Reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverse xs ++ [x]

--Union of Two Lists
unionList :: (Eq a) => [a] -> [a] -> [a]
unionList [] [a] = [a]
unionList [a] [] = [a]

unionList xs ys = removeDuplicates(xs ++ ys)
removeDuplicates [] = []
removeDuplicates (x:xs) = if x `elem` xs then removeDuplicates xs
                          else x : removeDuplicates xs      

--Sorting a list
sortList :: Ord a => [a] -> [a]
sortList [] = []
sortList (p:xs) = (sortList lesser) ++ [p] ++ (sortList greater)
        where
          lesser  = filter (< p) xs
          greater = filter (>= p) xs

--Maximum Element
maxNum :: (Ord a) => [a] -> a
maxNum [] = error "List is Empty"
maxNum [x] = x
maxNum (x:xs) | (maxNum xs) > x = maxNum xs
               | otherwise  = x

--Minimum Element               
minNum :: (Ord a) => [a] -> a
minNum [] = error "List is Empty"
minNum [x] = x
minNum (x:xs) | (minNum xs) < x = minNum xs
              | otherwise = x  

--Permutation of a List
permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- permutation (delete x xs)]
                   
               

main :: IO ()
main = do
        print (minNum [1,2,3])
    

