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
  reduceS f b [] = b
  reduceS f b s = f b (reduceS' s)
    where
      reduceS' [s1] = s1
      reduceS' s = reduceS' (contraer s)
      contraer [] = []
      contraer [x1] = [x1]
      contraer (x1:x2:xs) = let (y, ys) = f x1 x2 ||| contraer xs in y:ys

  --scanS
  -- scanS f b [] = ([b], b)
  -- scanS f b s = expandir s (scanS f b (contraer s))
  --   where
  --     contraer [] = []
  --     contraer [x1] = [x1]
  --     contraer (x1:x2:xs) = let (y, ys) = f x1 x2 ||| contraer xs in y:ys
  --     expandir s s'@([], t) = s'
  --     expandir s s'@([x], t) = s'
  --     expandir (s1:ss) (x1:x2:xs, t) = (x1 : f s1 x2 : fst expandir ss (xs, t), t)


  fromList = id