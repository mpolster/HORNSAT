module HORNSAT (Formula (..), Literal (..), Clause (..), hornSat) where

data Literal = Literal {name::String} | BTM | TOP deriving (Eq)
data Clause = Clause {prec::[Literal], impl::Literal}
data Formula = Formula {clauses::[Clause]}
type Model = [Literal]

instance Show Literal where 
    show (Literal x) = x
    show x = if x == TOP then "⊤" else "⊥"

instance Show Clause where 
    show (Clause p i) = init (foldl (++) "" (map (\x -> show x ++ "∧") p)) ++ "->" ++ show i

instance Show Formula where 
    show (Formula c) = init (foldl (++) "" (map (\x -> "(" ++ show x ++ ")∧") c))

hornSat :: Formula -> (String, Model)
hornSat f = case hornSatH f 0 (map (\x-> prec x !! 0) (filter (\s -> impl s == TOP) (clauses f))) of 
    Just model -> if model == [BTM] then ("Invalid Formula!", []) else ("Satisfiable!", model)
    Nothing -> ("Not Satisfiable!", [])

hornSatH :: Formula -> Int -> Model -> Maybe Model
hornSatH f n acc 
    | length cls == n = Just acc
    | null (prec cls_n) = Just [BTM]
    | foldl (&&) True (map (`elem` acc) (prec cls_n)) && (impl cls_n) == BTM = Nothing
    | foldl (&&) True (map (`elem` acc) (prec cls_n)) && (impl cls_n) `notElem` [BTM, TOP] ++ acc = hornSatH f 0 (impl cls_n : acc)
    | otherwise = hornSatH f (n+1) acc
    where cls = clauses f
          cls_n = cls !! n
