module ArrSeq where
import Seq
import Par
import qualified Arr as A
import Arr ((!))

instance Seq A.Arr where
    emptyS = A.empty

    singletonS x = fromList [x]

    lengthS = A.length

    nthS = (!)

    tabulateS = A.tabulate

    --mapS       :: (a -> b) -> s a -> s b 
    --filterS    :: (a -> Bool) -> s a -> s a 
    --appendS    :: s a -> s a -> s a
    takeS seq n = A.subArray 0 n seq

    dropS seq n = A.subArray n (A.length seq - n) seq

    --showtS     :: s a -> TreeView a (s a)
    --showlS     :: s a -> ListView a (s a)

    joinS = A.flatten
    --reduceS    :: (a -> a -> a) -> a -> s a -> a
    --scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
    fromList = A.fromList
