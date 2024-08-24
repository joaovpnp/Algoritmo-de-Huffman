import System.IO

type Word' = String
type Quantity = Integer

data Tree = Node (Word', Quantity) Tree Tree | Leaf deriving Show