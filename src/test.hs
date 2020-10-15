import HORNSAT

main = do
    putStrLn $ show (hornSat (Formula [Clause [Literal "A", Literal "B"] BTM, Clause [Literal "B"] TOP, Clause [Literal "C", Literal "B"] (Literal "B")]))
    putStrLn $ show (hornSat (Formula [Clause [Literal "A2", Literal "A4"] (Literal "A3"), Clause [Literal "A4", Literal "A1"] (Literal "A2"), Clause [Literal "A4"] (Literal "A1"), Clause [Literal "A3"] BTM]))
    putStrLn $ show (hornSat (Formula [Clause [Literal "A1", Literal "A6"] (Literal "A3"), Clause [Literal "A2"] TOP, Clause [Literal "A3", Literal "A4"] (Literal "A2"), Clause [Literal "A2"] (Literal "A1"), Clause [Literal "A5", Literal "A1"] (Literal "A7"), Clause [Literal "A6"] TOP, Clause [Literal "A3", Literal "A4"] BTM]))
    putStrLn $ show (hornSat (Formula []))
    putStrLn $ show (hornSat (Formula [Clause [Literal "B"] BTM, Clause [Literal "B"] TOP]))
    putStrLn $ show (Formula [Clause [Literal "A", Literal "B"] BTM, Clause [Literal "B"] TOP, Clause [Literal "C", Literal "B"] (Literal "B")])
