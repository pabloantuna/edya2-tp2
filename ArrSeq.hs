module ArrSeq where

import Arr ((!))
import qualified Arr as A
import Par
import Seq

instance Seq A.Arr where
  emptyS = A.empty

  singletonS x = fromList [x]

  lengthS = A.length

  nthS = (!)

  tabulateS = A.tabulate

  mapS f seq = tabulateS (f . nthS seq) (lengthS seq)

  --filterS    :: (a -> Bool) -> s a -> s a

  appendS xs ys = tabulateS aux (largoXS + largoYS)
    where
      largoXS = lengthS xs
      largoYS = lengthS ys
      aux n
        | n < largoXS = nthS xs n
        | otherwise = nthS ys (n - largoXS)

  takeS seq n = A.subArray 0 (min n (lengthS seq)) seq

  dropS seq n
    | n > largo = emptyS
    | otherwise = A.subArray n (largo - n) seq
    where
      largo = lengthS seq

  showtS seq
    | largo == 0 = EMPTY
    | largo == 1 = ELT (nthS seq 0)
    | otherwise =
      let mitad = largo `div` 2
       in NODE (takeS seq mitad) (dropS seq mitad)
    where
      largo = lengthS seq

  showlS seq
    | largo == 0 = NIL
    | otherwise = CONS (nthS seq 0) (A.subArray 1 (largo - 1) seq)
    where
      largo = lengthS seq

  joinS = A.flatten

  --reduceS    :: (a -> a -> a) -> a -> s a -> a
  --scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)

  fromList = A.fromList
