import Data.List (sortBy)
import Data.Ord (comparing)

estudiantes :: [(String, Int)]
estudiantes = [("Ana",85), ("Luis",90), ("Carlos",85), ("Marta",95)]


ordenar :: [(String, Int)] -> [(String, Int)]
ordenar = sortBy (\(n1,c1) (n2,c2) -> compare c2 c1 <> compare n1 n2)

main :: IO ()
main = print (ordenar estudiantes)