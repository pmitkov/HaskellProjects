module Huffman where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL

data Bit = Zero | One deriving (Eq,Show)

type Bits = [Bit]

type CharMap a = M.Map Char a

data HuffmanTree = Leaf Char Int | Node HuffmanTree HuffmanTree [Char] Int deriving (Show)

instance Eq HuffmanTree where
    l == r = weight l == weight r

instance Ord HuffmanTree where
    compare l r = weight l `compare` weight r

symbols :: HuffmanTree -> [Char]
symbols (Leaf symbol _) = [symbol]
symbols (Node _ _ syms _) = syms

weight :: HuffmanTree -> Int
weight (Leaf _ w) = w
weight (Node _ _ _ w) = w

leftBranch :: HuffmanTree -> HuffmanTree
leftBranch (Node l _ _ _) = l

rightBranch :: HuffmanTree -> HuffmanTree
rightBranch (Node _ r _ _) = r

combineTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree
combineTrees l r = Node l r (symbols l ++ symbols r) (weight l + weight r)

createCharMap :: HuffmanTree -> CharMap Bits
createCharMap tree = M.fromList $ charPaths tree
    where charPaths (Leaf symbol _)   = [(symbol, [])]
          charPaths (Node l r syms _) = (map (\(s,p) -> (s, Zero:p)) $ charPaths l) ++ 
                                        (map (\(s,p) -> (s, One:p)) $ charPaths r)

insertSortedList :: (Ord a) => a -> [a] -> [a]
insertSortedList x [] = [x]
insertSortedList x (y:ys)
    | x < y     = x:y:ys
    | x == y    = x:y:ys
    | otherwise = y:insertSortedList x ys

getCharFrequencies :: String -> CharMap Int
getCharFrequencies [] = M.empty
getCharFrequencies (x:xs) = let frequencies = getCharFrequencies xs
                            in case M.lookup x frequencies of
                                 Just freq -> M.insert x (freq + 1) frequencies
                                 Nothing   -> M.insert x 1 frequencies

frequenciesToTreeList :: CharMap Int -> [HuffmanTree]
frequenciesToTreeList m = map (\(s,f) -> Leaf s f) $ sortBy (compare `on` snd) $ M.toList m

createTree :: [HuffmanTree] -> HuffmanTree
createTree [t] = t
createTree (t1:t2:ts) = createTree $ insertSortedList (combineTrees t1 t2) ts

encodeSequence :: String -> (HuffmanTree, Bits)
encodeSequence message = let messageToTree = createTree . frequenciesToTreeList . getCharFrequencies
                             tree    = messageToTree message
                             charMap = createCharMap tree
                         in (tree, concatMap (fromJust . ((flip M.lookup) charMap)) message)
                         
decodeSequence :: Bits -> HuffmanTree -> String
decodeSequence bits tree = traverseBitTree bits tree
    where traverseBitTree []   (Leaf symbol _)       = [symbol]
          traverseBitTree bits (Leaf symbol _)       = symbol:traverseBitTree bits tree
          traverseBitTree (Zero:bits) (Node l _ _ _) = traverseBitTree bits l
          traverseBitTree (One:bits) (Node _ r _ _)  = traverseBitTree bits r

encodeTree :: HuffmanTree -> Bits
encodeTree (Leaf symbol _) = One:charToBits symbol
encodeTree (Node l r _ _)  = Zero:(encodeTree l ++ encodeTree r)  

decodeTree :: Bits -> HuffmanTree
decodeTree bits = snd $ decode bits
    where decode (One:bits) = (drop 8 bits, Leaf (bitsToChar $ take 8 bits) 0)
          decode (Zero:bits) = let (leftBits, ltree)  = decode bits
                                   (rightBits, rtree) = decode leftBits
                                in (rightBits, Node ltree rtree [] 0)

byteStringToCharArray :: BL.ByteString -> [Char]
byteStringToCharArray = map (chr . fromIntegral) . BL.unpack

charArrayToByteString :: [Char] -> BL.ByteString
charArrayToByteString = BL.pack . map (fromIntegral . ord)

charToBits :: Char -> Bits
charToBits c = convert (ord c) 0 []
    where convert n count res
            | count == 8     = res
            | n `rem` 2 == 0 = convert (n `div` 2) (count + 1) (Zero:res)
            | otherwise      = convert (n `div` 2) (count + 1) (One:res)

bitsToChar :: Bits -> Char
bitsToChar bits = chr $ convert 0 bits
    where convert res [] = res
          convert res (bit:bits)
            | bit == One = convert (res * 2 + 1) bits
            | otherwise  = convert (res * 2) bits

intToBits :: Int -> Bits
intToBits n = convert n 0 []
    where convert n count res
            | count == 32    = res
            | n `rem` 2 == 0 = convert (n `div` 2) (count + 1) (Zero:res)
            | otherwise      = convert (n `div` 2) (count + 1) (One:res)

bitsToInt :: Bits -> Int
bitsToInt bits = convert 0 bits
    where convert res [] = res
          convert res (bit:bits)
            | bit == One = convert (res * 2 + 1) bits
            | otherwise  = convert (res * 2) bits

bitsToChars :: Bits -> [Char]
bitsToChars bits
    | length (take 8 bits) < 8 = [appendChar bits]
    | otherwise = bitsToChar (take 8 bits):bitsToChars (drop 8 bits)
    where appendChar bits = bitsToChar $ bits ++ replicate (8 - length bits) Zero