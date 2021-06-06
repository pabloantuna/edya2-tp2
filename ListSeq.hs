module ListSeq where
import Seq
import Par

instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS = length

    nthS = (!!)

    -- copiada de Damian medio chad que use la lambda function
    tabulateS f n = aux (\x -> f (n-x)) n
                    where
                        aux _ 0 = emptyS 
                        aux f n = let
                                    (x, xs) = f n ||| aux f (n-1)
                                  in
                                    x:xs

    -- no copiada pero resulta que aldana la tiene EXACTAMENTE igual y me da miedo que crean que lo copiamos
    -- tabulateS f n = aux f n 0 
    --                 where
    --                     aux _ 0 _ = emptyS
    --                     aux f n i = let 
    --                                   (x, xs) = f i ||| aux f (n-1) (i+1)
    --                                 in
    --                                     x:xs

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

    -- appendS = (++)
    appendS xs [] = xs
    appendS [] ys = ys
    appendS (x:xs) ys = x : appendS xs ys

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


    -- joinS = concat creo?
    joinS [] = []
    joinS (x:xs) = appendS x (joinS xs)

    --reduceS
    --scanS

    fromList = id