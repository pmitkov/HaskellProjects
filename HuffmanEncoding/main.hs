module Main where

import Huffman
import System.Environment
import System.Directory
import System.IO
import Data.Char
import qualified Data.ByteString.Lazy as BL

encodeFile :: FilePath -> FilePath -> IO ()
encodeFile inputFile outputFile = do
    contents <- BL.readFile inputFile
    let file = byteStringToCharArray contents
        (tree, sequence) = encodeSequence file
        encodedTree  = encodeTree tree
        encodedBits = charArrayToByteString $ bitsToChars $ (intToBits $ length encodedTree) ++ 
                                              (charToBits $ chr (length (sequence ++ encodedTree) `rem` 8)) ++ 
                                              encodedTree ++ 
                                              sequence
    BL.writeFile outputFile encodedBits
        
decodeFile :: FilePath -> FilePath -> IO ()
decodeFile inputFile outputFile = do
    contents <- BL.readFile inputFile
    let sequence = byteStringToCharArray contents
        bitSequence = concatMap charToBits sequence
        treeLength = bitsToInt $ take 32 bitSequence
        lastByteLength = bitsToInt $ drop 32 $ take 40 bitSequence
        tree = decodeTree $ take treeLength (drop 40 bitSequence)
    BL.writeFile outputFile $ charArrayToByteString $ 
                              decodeSequence (take (length bitSequence - 48 - treeLength + lastByteLength) $ 
                              drop (40 + treeLength) bitSequence) tree

checkInput :: [String] -> IO (Either String [String])
checkInput args = do
    if length args /= 3
        then return $ Left "Wrong number of arguments. Usage ./huffman [<encode>|<decode>] <inputFile> <outputFile>."
        else do
            existsInput <- doesFileExist $ args !! 1
            if existsInput == False
                then return $ Left $ "File does not exist."
                else if ((args !! 0) == "encode" || (args !! 0) == "decode")
                        then return $ Right args
                        else return $ Left "Wrong command. Usage ./huffman [<encode>|<decode>] <inputFile> <outputFile>."

main :: IO ()
main = do
    args <- getArgs
    check <- checkInput args
    case check of
        (Left error) -> putStrLn $ "Error: " ++ error
        (Right args) -> do
            if ((args !! 0) == "encode")
                then encodeFile (args !! 1) (args !! 2)
                else decodeFile (args !! 1) (args !! 2)