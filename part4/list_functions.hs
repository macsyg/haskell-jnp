{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
main :: Program
type Program = IO ()

main = print test8

test1 = appendList listA listB
test2 = elemList elem1 listA
test3 = elemList elem1 listB
test4 = listLength listC
test5 = nth listC 5
test6 = mapList add_one listC
test7 = andList listBoolA
test8 = andList listBoolB

--appendList :: [a] -> [a] -> [a]
--appendList (h:t) list = h:(appendList t list)
--appendList [] list = list
  
listA = [1, 2, 3]
listB = [4, 5, 6]
listC = [7, 8, 9, 0, 1, 2, 3, 4]
listBoolA = [True, False, True, True, True]
listBoolB = [True, True, True, True, True]
add_one x = x + 1
elem1 = 2


foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun acc (h:t) = fun h (foldList fun acc t)
foldList fun acc [] = acc

-- appendList
addHead :: a -> [a] -> [a]
addHead h t = h:t

appendList :: [a] -> [a] -> [a]
appendList list1 list2 = foldList addHead list2 list1
-- appendList END

-- elemList
elemList :: Eq a => a -> [a] -> Bool
elemList element list = snd(foldList checkElem (element, False) list)

checkElem :: Eq a => a -> (a, Bool) -> (a, Bool)
checkElem new_elem (searched_elem, isSame)
  | isSame == True = (searched_elem, True)
  | new_elem == searched_elem = (searched_elem, True)
  | otherwise = (searched_elem, False)
-- elemList END

-- listLength
listLength :: [a] -> Integer
listLength list = foldList countElem 0 list

countElem :: a -> Integer -> Integer
countElem element acc = (acc + 1)
-- listLength END

-- nth 
nth :: [a] -> Integer -> a
nth list n = snd(foldList searchedElem ((listLength list - n), head list) list) 

searchedElem :: a -> (Integer, a) -> (Integer, a)
searchedElem element (n, old_elem)
  | n == 0 = ((n-1), element)
  | otherwise = ((n-1), old_elem)
-- nth END

-- mapList
mapList :: (a -> b) -> [a] -> [b]
mapList fun listA = snd(foldList mapElem (fun,[]) listA)

mapElem :: a -> ((a -> b), [b]) -> ((a -> b), [b])
mapElem element (fun, listB) = (fun, (fun element):listB)
-- mapList END

-- andList 
andList :: [Bool] -> Bool
andList boolList = foldList andElems True boolList

andElems :: Bool -> Bool -> Bool
andElems boolElem boolAcc = boolElem && boolAcc
-- andList END

-- allList 
allList :: (a -> Bool) -> [a] -> Bool
allList correctElem list = andList (mapList correctElem list)
-- andList END

























