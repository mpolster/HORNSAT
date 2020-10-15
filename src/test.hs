import HORNSAT

main = do
    putStrLn (hornSat (Formula [Clause [Literal True "A", Literal True "B"] BTM, Clause [Literal True "B"] TOP, Clause [Literal True "C", Literal True "B"] (Literal True "B")]))
    putStrLn (hornSat (Formula [Clause [Literal True "A2", Literal True "A4"] (Literal True "A3"), Clause [Literal True "A4", Literal True "A1"] (Literal True "A2"), Clause [Literal True "A4"] (Literal True "A1"), Clause [Literal True "A3"] BTM]))
    putStrLn (hornSat (Formula []))
    putStrLn $ show (Formula [Clause [Literal True "A", Literal True "B"] BTM, Clause [Literal True "B"] TOP, Clause [Literal True "C", Literal True "B"] (Literal True "B")])
