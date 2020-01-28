{- 
Author: Lukas Rønsholt
Study no: 20166653
Mail: lransh16@student.aau.dk

University: Aalborg University
Study: Software (Master)
Class: Programmin Paradigms

Huffman coding of text,
To just see the text compressed and decompressed again, run main.
To do the progress yourself follow the example:

```
text = "mississippi river"                  -- The text we will compress
chars = getDictFromString text              -- Get the uniqe characters and their occurences
tree = createBinaryTreeFromDict chars       -- Create a binary tree from the characters
prefix = getPrefixCodes tree                 -- Get the prefix codes for each character
compressed = compressString text prefix     -- Compress the text using the prefix codes

decompressed = decompressString compressed prefix   -- Decompress the text again

```

-}

import Data.List

type Amount = Int
type Pair = (Char,Amount)
type DoublePair = (Pair, Pair)
type Dictonary = [Pair]

data BinaryTree = Leaf (Char,Int) | Branch ((String, Int), BinaryTree, BinaryTree) deriving (Show, Read, Eq)  

-- Main: Easy compression of text provided though input
main :: IO ()
main = do
    putStr "Enter text to compress: "
    text <- getLine
    let prefix = getPrefixCodes $ createBinaryTreeFromDict $ getDictFromString text
    putStrLn "The compressed string:"
    let compressed = compressString text prefix
    putStrLn compressed
    putStrLn "And decompressed:"
    putStrLn $ decompressString compressed prefix

-- getDictFromString: Get the uniqe characters in a string and their occurences
getDictFromString :: String -> Dictonary
getDictFromString [] = []
getDictFromString string = fillDict string []
                                where
                                    inDict :: Char -> Dictonary -> Bool
                                    inDict _ [] = False
                                    inDict char dict = if char == fst (head dict) then True else inDict char (tail dict)
                                    fillDict :: String -> Dictonary -> Dictonary
                                    fillDict [] dict = dict
                                    fillDict (x:xs) dict = if inDict x dict then fillDict xs dict
                                                            else fillDict xs $ dict ++ [(x,amount)]
                                                                where
                                                                    amount = length $ filter (== x) (x:xs)

-- createBinaryTreeBranch: Create a new binary tree branch given two leafs and/or branches
createBinaryTreeBranch :: BinaryTree -> BinaryTree -> BinaryTree
createBinaryTreeBranch (Leaf (a1,a2)) (Leaf (b1,b2)) = Branch (([b1,a1],a2+b2), Leaf (b1,b2), Leaf (a1,a2))
createBinaryTreeBranch (Leaf (a1,a2)) (Branch ((b1,b2), c1, c2)) = Branch (([a1] ++ b1, a2 + b2), Leaf (a1,a2), Branch ((b1,b2), c1, c2))
createBinaryTreeBranch (Branch ((b1,b2), c1, c2)) (Leaf (a1,a2)) = Branch ((b1 ++ [a1], a2 + b2), Branch ((b1,b2), c1, c2), Leaf (a1,a2))
createBinaryTreeBranch (Branch ((a1,a2), b1, b2)) (Branch ((c1,c2), d1, d2)) = Branch ((c1 ++ a1, a2 + c2), Branch ((c1,c2), d1, d2), Branch ((a1,a2), b1, b2))

-- sortBinaryTreeList: Sort a list of binary trees
sortBinaryTreeList :: [BinaryTree] -> [BinaryTree]
sortBinaryTreeList list = sortBy amountChar list
                            where
                                amountChar :: BinaryTree -> BinaryTree -> Ordering
                                amountChar (Leaf (_,y1)) (Leaf (_,y2)) = compare y1 y2
                                amountChar (Leaf (x1,y1)) (Branch ((x2,y2), _, _)) = if y1 == y2 then compare [x1] x2 else compare y1 y2
                                amountChar (Branch ((x1,y1), _, _)) (Leaf (x2,y2)) = if y1 == y2 then compare x1 [x2] else compare y1 y2
                                amountChar (Branch ((x1,y1), _, _)) (Branch ((x2,y2), _, _)) = compare y1 y2

-- createBinaryTreeFromDict: convert a list of pairs (Char,Int) into a binary tree repersenting the frequensy of characters                                
createBinaryTreeFromDict :: Dictonary -> BinaryTree
createBinaryTreeFromDict dict = createBinaryTree $ map createLeaf dict
                                    where
                                        createLeaf :: Pair -> BinaryTree
                                        createLeaf (a,b) = Leaf (a, b)  
                                        createBinaryTree :: [BinaryTree] -> BinaryTree
                                        createBinaryTree trees = if length trees == 1 then head trees else createBinaryTree result
                                                                    where
                                                                        list = splitAt 2 $ sortBinaryTreeList trees
                                                                        result = snd list ++ [createBinaryTreeBranch (head (fst list)) (last (fst list))]

-- getPrefixCodes: Traverse a binary tree and calulate the prefix code all characters in the tree                                                                        
getPrefixCodes :: BinaryTree -> [(Char,String)]
getPrefixCodes tree = traverseTree tree []                                                                                          
                        where
                            traverseTree :: BinaryTree -> String -> [(Char,String)]
                            traverseTree (Branch ((_,_), left, right)) [] = traverseTree left "0" ++ traverseTree right "1"
                            traverseTree (Leaf (x,_)) [] = [(x,"0")]
                            traverseTree (Branch ((_,_), left, right)) code = traverseTree left (code ++ "0") ++ traverseTree right (code ++ "1")
                            traverseTree (Leaf (x,_)) code = [(x,code)]

-- compressString: Compress a string using the provided prefix codes                            
compressString :: String -> [(Char,String)] -> String
compressString string lookupTable = filter (/=' ') $ unwords $ map (encodeChar lookupTable) string
                                        where
                                            encodeChar :: [(Char,String)] -> (Char -> String)
                                            encodeChar list char = snd $ head $ filter (match char) list
                                                                        where
                                                                            match matcher element = fst element == matcher

-- decodeChar: look up a prefix and return the connected character                                                                            
decodeChar :: [(Char,String)] -> String -> Maybe Char
decodeChar list code = if result == [] then Nothing else Just $ fst $ head result
                        where
                            result = filter (match code) list
                            match matcher element = snd element == matcher

-- decompressString: decompress a string using the provied prefix codes, will throw an error if not possible to decode using the prefix codes                            
decompressString :: String -> [(Char,String)] -> String
decompressString string lookupTable = reverse $ decodeString (tail string) [(head string)] []
                                        where
                                            getValue :: Maybe Char -> Char
                                            getValue (Just a) = a
                                            decodeString :: String -> String -> String -> String
                                            decodeString [] current res = 
                                                if code == Nothing then error "String not able decompress with prefix" else res ++ [getValue code]
                                                where
                                                    code = decodeChar lookupTable current
                                            decodeString text current res = 
                                                if code == Nothing 
                                                    then decodeString (tail text) (current ++ [head text]) res 
                                                    else decodeString (tail text) ([head text]) res ++ [getValue code]
                                                where
                                                    code = decodeChar lookupTable current

-- create graph file for GraphVis to visulize the tree                                                                        
makeGraph :: BinaryTree -> IO ()                                                                        
makeGraph tree = writeFile "graph.gv" $ traverseTree tree []
                    where
                        removeSpace :: String -> String
                        removeSpace string = map replaceSpace string
                                                where replaceSpace char = if char == ' ' then '_' else char
                        traverseTree :: BinaryTree -> String -> String
                        traverseTree (Leaf (x,y)) [] = "digraph{\n" ++ removeSpace [x] ++ show y ++ "\n} "
                        traverseTree (Branch ((x,y), c, d)) [] = "digraph{\n" ++ traverseTree c (removeSpace x ++ show y) ++ traverseTree d (removeSpace x ++ show y) ++ "\n}"
                        traverseTree (Leaf (x,y)) parent = "\n" ++ parent ++ "->" ++ removeSpace [x] ++ show y ++ "\n"
                        traverseTree (Branch ((x,y), c, d)) parent = "\n" ++ parent ++ "->" ++ removeSpace x ++ show y ++ "\n" ++ traverseTree c (removeSpace x ++ show y) ++ traverseTree d (removeSpace x ++ show y) ++ "\n"
 
-- compressAndSaveToFile: compress a string and save the compressed string and prefix to a file                        
compressAndSaveToFile :: String -> IO ()
compressAndSaveToFile string = do
    let prefix = getPrefixCodes $ createBinaryTreeFromDict $ getDictFromString string
    let compressed = compressString string prefix
    writeFile "compressed" $ compressed ++ "\n" ++ show prefix

-- compressFile: read text from file and compress
compressFile :: FilePath -> IO ()
compressFile file = do
    text <- readFile file
    compressAndSaveToFile text

-- decompressFromFile: decompress text from a file containg a compressed text and prefix codes
decompressFromFile :: FilePath -> IO ()
decompressFromFile file = do
    content <- readFile file
    let flines = lines content
    let prefix = read (last flines) :: [(Char,String)]
    let compressed = head flines
    putStrLn "Decompressed text: "
    putStrLn $ decompressString compressed prefix
