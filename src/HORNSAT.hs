module HORNSAT (Formula (..), Literal (..), Clause (..), hornSat) where

data Literal = Literal {sig::Bool, name::String} | BTM | TOP deriving (Eq)
data Clause = Clause {prec::[Literal], impl::Literal}
data Formula = Formula {clauses::[Clause]}
type Model = [Literal]

instance Show Literal where 
    show = showLiteral

instance Show Clause where 
    show (Clause p i) = (init (foldl (++) "" (map (\x -> (show x) ++ "∧") p))) ++ "->" ++ (show i)

instance Show Formula where 
    show = showFormula

showLiteral :: Literal -> String
showLiteral TOP = "⊤"
showLiteral BTM = "⊥"
showLiteral (Literal s n) = (if s then "" else "¬") ++ n

showFormula :: Formula -> String
showFormula f = init (foldl (++) "" (map(\x -> "(" ++ (show x) ++ ")∧") (clauses f)))

hornSat :: Formula -> String
hornSat f = case (hornSatH f 0 []) of 
    Just modell -> show modell
    Nothing -> "Not Satisfiable"

hornSatH :: Formula -> Int -> Model -> Maybe Model
hornSatH f n acc 
    | n == length (clauses f) = Just acc
    | null (prec ((clauses f) !! n)) = Nothing
    | impl ((clauses f) !! n) == TOP && (prec ((clauses f) !! n) !! 0) `notElem` acc = hornSatH f 0 (((prec ((clauses f) !! n)) !! 0):acc)
    | foldl (&&) True ((map (`elem` acc) (prec ((clauses f) !! n)))) && (impl ((clauses f) !! n)) == BTM = Nothing
    | foldl (&&) True ((map (`elem` acc) (prec ((clauses f) !! n)))) && (impl ((clauses f)!! n)) `notElem` [BTM, TOP] && (impl ((clauses f) !! n)) `notElem` acc = hornSatH f 0 ((impl ((clauses f) !! n)):acc)
    | otherwise = hornSatH f (n+1) acc
