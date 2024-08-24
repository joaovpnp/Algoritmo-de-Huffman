import System.IO

type Word' = String
type Quantity = Integer

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

main = do putStr("Digite o diretorio do arquivo: ")
          hFlush stdout
          arq <- getLine
          txt <- readFile arq
          let list = sortList( treeToList (countWords txt))
          encriptation list (huffmanTree (reverse list))

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

treeToList Leaf = []
treeToList (Node a left right) = treeToList left ++ [a] ++ treeToList right

auxSort [] _ = []
auxSort (x:xs) 1 = x : auxSort xs (length xs)
auxSort ((letter, q):b:xs) listLength
    | q <= snd b = auxSort ((letter,q):xs ++ [b]) (listLength-1)
    | otherwise = auxSort (b:(letter,q):xs) (length ((letter, q):b:xs))

sortList xs = auxSort xs (length xs)

huffmanTree [a,b] = Node (fst a, '1') (Node (fst a, '0') Leaf Leaf) (Node (fst b, '0') Leaf Leaf)
huffmanTree (a:b:xs) = Node (fst a, '1') (Node (fst a, '0') Leaf Leaf) (huffmanTree (b:xs))

code _ (Node a Leaf Leaf) = []
code x (Node a (Node b Leaf Leaf) right)
    | x == fst a = [snd b]
    | otherwise = snd a : code x right

encriptation [] _ = return ()
encriptation ((c,q):ds) tree = do putStrLn([c] ++ "=" ++ code c tree)
                                  encriptation ds tree
