module Votes exposing (..)

-- 1 

compteVotes : List a -> List (Int,a)
compteVotes l =
    let 
        compte : a ->  List a -> Int 
        compte x l3 = List.length(List.filter (\y -> y == x) l3) 
        trouveCandidats : List a -> List a 
        trouveCandidats l2 =
            case l2 of 
                x::xs ->  x:: (List.filter(\y -> y/= x) (trouveCandidats xs))
                []    -> []
    in List.map ((\elt -> (compte elt l, elt))) ( trouveCandidats l )

-- 2

enleveSiVide : List (List a) -> List (List a)
enleveSiVide ll =  List.filter(\l -> List.length l /= 0) ll

elim : a -> List(List a ) -> List(List a) 
elim x ll = List.map(\l ->  (List.filter (\elt -> x/= elt)l) )ll

rang : List(List a) -> List a 
rang ll = List.concat (List.map (\l -> (List.take 1 l)) ll)


tri : List(Int , a ) -> List(Int,a)
tri l = List.sortBy (\x -> Tuple.first x) l

trouveTete : List(List a ) -> List a 
trouveTete ll = 
    let 
        cv = compteVotes (rang ll)
    in List.map (\elt -> (Tuple.second elt)) (tri cv)

--gagnant : List(List a) -> a 
gagnant g = 
    case (trouveTete (enleveSiVide g)) of 
        [elt] -> elt
        x::s -> gagnant (elim x g)
        [] -> ""

test =  gagnant [["R","G"],["G","R"]]
test1 =  gagnant [["G","R"],["R","G"]]
test2 = gagnant [["R","G"],["B"],["G","R","B"],["B","G","R"],["G"]]
