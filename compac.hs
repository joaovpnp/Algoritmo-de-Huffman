import System.IO

type Word' = String
type Quantity = Integer

data Tree a = Node a Tree Tree | Leaf deriving Show

main = do putStr("Digite o diretorio do arquivo: ")
          hFlush stdout
          arq <- getLine
          txt <- readFile


