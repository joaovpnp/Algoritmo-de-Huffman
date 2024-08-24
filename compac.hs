import System.IO

type Word' = String
type Quantity = Integer

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

main = do putStr("Digite o diretorio do arquivo: ")
          hFlush stdout
          arq <- getLine
          txt <- readFile arq
          let test = countWords txt
          printTree test

printTree Leaf = return ()
printTree (Node a left right) = do printTree left
                                   putStrLn ("\t" ++ show a)
                                   printTree right

wordsTree c Leaf = Node (c, 1) Leaf Leaf
wordsTree c (Node (letter, q) left right)
    | c == letter = Node (letter, q+1) left right
    | c < letter = Node (letter, q) (wordsTree c left) right
    | otherwise = Node (letter, q) left (wordsTree c right)

countWords cs = foldr wordsTree Leaf cs

