module ListSeq where
import Seq
import Par

instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS = length

    nthS = (!!)
    
    -- tabulateS f n = mapS f [0..n]
    tabulateS f n = fhasta n
                    where
                        fhasta 0 = emptyS 
                        fhasta m = let
                                    (x, xs) = f (n-m) ||| fhasta (m-1)
                                  in
                                    x:xs
                                    
    --- mapS = map -- idk tal vez no se como trabaja internamente map como para saber si optimiza ese pararelismo que quieren

    mapS _ [] = emptyS 
    mapS f (x:xs) = let
                        (y, ys) = f x ||| mapS f xs
                    in
                        y:ys

    --- filterS = filter same duda map
    filterS _ [] = emptyS 
    filterS f (x:xs) = let
                        (y, ys) = f x ||| filterS f xs
                      in
                        if y then x:ys else ys

    appendS = (++)

    takeS xs n = take n xs

    dropS xs n = drop n xs

    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS xs = let
                    mitad = lengthS xs `div` 2
                    (left, right) = takeS xs mitad ||| dropS xs mitad
                in 
                    NODE left right

    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS = concat

    --reduceS
    --scanS

    fromList = id