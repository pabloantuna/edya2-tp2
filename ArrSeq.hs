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

  filterS f seq = joinS (mapS (\x -> if f x then singletonS x else emptyS) seq)

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
          (left, right) = takeS seq mitad ||| dropS seq mitad
       in NODE left right
    where
      largo = lengthS seq

  showlS seq
    | largo == 0 = NIL
    | otherwise = CONS (nthS seq 0) (dropS seq 1)
    where
      largo = lengthS seq

  joinS = A.flatten

  --reduceS
  reduceS f b seq
    | largo == 0 = b
    | otherwise = f b (reduceS' seq)
    where
      largo = lengthS seq
      reduceS' seq
        | largo == 1 = nthS seq 0
        | otherwise = reduceS' (contraer seq)
        where
          largo = lengthS seq
      contraer seq
        | largo < 2 = seq
        | otherwise = tabulateS (\x -> if (2 * x + 1) == largo then nthS seq (2 * x) else f (nthS seq (2 * x)) (nthS seq (2 * x + 1))) mitad
        where
          largo = lengthS seq
          mitad = ceiling (fromIntegral largo / 2)

  --scanS
  scanS f b seq
    | largo == 0 = (emptyS, b)
    | largo == 1 = (singletonS b, f b (nthS seq 0))
    where
      largo = lengthS seq

  fromList = A.fromList
