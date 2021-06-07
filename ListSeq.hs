module ListSeq where

import Par
import Seq

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS = length

  nthS = (!!)

  tabulateS f n = fhasta n
    where
      fhasta 0 = emptyS
      fhasta m =
        let (x, xs) = f (n - m) ||| fhasta (m -1)
         in x : xs

  mapS _ [] = emptyS
  mapS f (x : xs) =
    let (y, ys) = f x ||| mapS f xs
     in y : ys

  filterS _ [] = emptyS
  filterS f (x : xs) =
    let (y, ys) = f x ||| filterS f xs
     in if y then x : ys else ys

  appendS = (++)

  takeS xs n = take n xs

  dropS xs n = drop n xs

  showtS [] = EMPTY
  showtS [x] = ELT x
  showtS xs =
    let mitad = lengthS xs `div` 2
        (left, right) = takeS xs mitad ||| dropS xs mitad
     in NODE left right

  showlS [] = NIL
  showlS (x : xs) = CONS x xs

  joinS = concat

  --reduceS
  --scanS

  fromList = id