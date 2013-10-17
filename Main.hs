{-# LANGUAGE OverloadedStrings #-}

module Main
    (
        q1
        , q2
        , q3
        , q4
        , q5
        , q6
        , encrypt
        , encrypt1
        , main
        , input1
        , input2
        , key2
        , input3
        , bestPlainTexts
        , bestIcs
        , ic
        , target
        , ham
        , hamlet
        , bham
        , scorr
        , solve
        , txt2count
        , split
        , normDist
        , asciiToB64
        , asciiToB16
        , asciiFromB64
        , asciiFromB16
        , keySize2pt
    ) where



import System.IO hiding ()
import Control.Monad
import Data.Array as A
import Data.Char as C
import Data.Word
import Data.Maybe
import Data.Map as M  hiding (split)
import Data.List as L
import Data.Ord
import Text.Printf
import Data.Tuple (swap)
import Numeric
import qualified Data.ByteString as B  hiding (split) 
import qualified Data.ByteString.Lazy as BL  hiding (split) 
import qualified Data.ByteString.Base64 as B64S
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy.Char8 as C8 hiding (split)
import Data.Bits
import Analysis as A
import qualified Data.ByteString.Internal as BSI
import Data.Either
import Data.Bits
import Data.Int
import GHC.Exts
import Data.Byteable
import Crypto.Cipher.AES

input1::C8.ByteString
input1 = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

input2::C8.ByteString
input2 = "1c0111001f010100061a024b53535009181c"
key2::C8.ByteString
key2 = "686974207468652062756c6c277320657965"

input3::C8.ByteString
input3 = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- Answer should be SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t
-- Decode to ASCII
-- Encode to B64
q1 = asciiToB64 $ asciiFromB16 input1

-- Answer should be 746865206b696420646f6e277420706c6179
-- Decode input2 and key2 to ASCII
-- xor them together
-- Encode to B16
q2 = asciiToB16 $ encrypt key16 input16
    where
        input16 = asciiFromB16 input2
        key16 = asciiFromB16 key2
     
       
-- Answer is "Cooking MC's like a pound of bacon"
-- Decode input3 as B16
-- Work out best key - ie. lowest IC difference
-- encrypt with this key
q3 = (key, diff, encrypt1 key input16)
    where
        (key, diff) = L.head $ bestIcs 1 $ asciiFromB16 input3
        input16 = asciiFromB16 input3

--solve16::ByteString->(Int, Double, ByteString)
--solve16 input16 = (key, diff, encrypt1 key input)
--    where
--        (key, diff) = bestIc $ asciiFromB16 input16
--        input = asciiFromB16 input16

--solve64::ByteString->(Int, Double, ByteString)
--solve64 input64 = (key, diff, encrypt1 key input)
--    where
--        (key, diff) = bestIc64 input64
--        input = asciiFromB64 input64

-- Reads the Q4 file 
-- Works out the best IC for all the lines
-- Chooses the best of the best
-- Works out the index of the best and returns the plain text with this index
q4 :: IO ()
q4 = do 
    ls <- liftM C8.lines $ BL.readFile "Question4.txt"
    let ics = L.map (L.head.(bestIcs 1).asciiFromB16) ls -- works out the ic scores for each line
    let maxic = swap $ L.maximum $ L.map swap ics -- the line with the best ic
    let pts = L.map (L.head.(bestPlainTexts 1).asciiFromB16)  ls -- Works out all the best pts
    let ix = fromJust $ L.findIndex (\i->snd i == snd maxic) ics -- finds which line has the best ic
    System.IO.putStrLn $ "Question 4 answer (line, key, pt) is: " ++ (show (ix, fst $ ics!!ix, fst $ pts!!ix))
            
-- Converts the input string and key
-- Encrypts the string using the cycled key as the key
q5 :: IO ()
q5 = do
    let input5 = "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"
    let ptin = BL.pack $ L.map BSI.c2w input5
    let inputkey = "ICE"
    let key = BL.pack $ L.map BSI.c2w inputkey
    let ctin = encrypt ptin $ BL.cycle key
    let ct16 = B16.encode ctin
    let ct64 = B64.encode ctin
    let ptout = encrypt ctin $ BL.cycle key
    let ct = L.head $ rights [B64.decode ct64]
    let keySize=3::Int
    let pieces = BL.transpose $ L.map BL.pack $ Main.split (keySize) $ BL.unpack ct
    let keys = cprod $ L.map ((L.map fst).bestIcs 3) pieces
    let tpts = L.map (\key -> (L.zipWith encrypt1 key pieces)) keys
    let pts = L.map (\pt -> BL.pack $ BL.unpack $ BL.concat $ BL.transpose pt) tpts
    let ks = L.map (\pt -> kapa $ txt2count $ BL.pack $ BL.unpack $ BL.concat $ BL.transpose pt) tpts
    let (k,pt) = maximumBy (comparing fst) (L.zip ks pts)
    System.IO.putStrLn $ "Question 5, ct in ascii is: " ++ (show ctin)
    System.IO.putStrLn $ "Question 5, ct16 (ie. ct encoded into B16 - this is the answer): " ++ (show ct16)
    System.IO.putStrLn $ "Question 5, ct64 (encoded into B64 - this is in Q6 format): " ++ (show ct64)
    System.IO.putStrLn $ "Question 5, using the known key: " ++ (show ptout)
    System.IO.putStrLn $ "Now in Q6 we need to solve ct64 -> pt"
    System.IO.putStrLn $ "First ct64 -> ct: ct = " ++ (show ct)
    System.IO.putStrLn $ "Next we need to work out the key length ..."
    System.IO.putStrLn $ "Assuming it's 3:"
--    System.IO.putStrLn $ "sols are: " ++ (show sols)
--    System.IO.putStrLn $ "Plain text is: " ++ (show pt)

        
split::Int->[a]->[[a]]
split n as = if L.length as <n then [as] else L.take n as : (Main.split n $ L.drop n as)
        
keySize2pt ct64 keySize = pt       
    where
        chks1 = L.map BL.pack $ Main.split keySize $ BL.unpack ct64
        chks2 = L.reverse $ L.tail $ L.reverse chks1
        tchks = BL.transpose chks2
        sols = L.head $ L.map (solve 1) tchks
        pts = L.map (\s-> let (_,_,t)=s in t) sols
        pt = L.map BSI.w2c $ L.concat $ L.map BL.unpack $ BL.transpose pts
        
        
q6:: IO ()
q6 = do
    let s1 = "this is a test"
    let s2 = "wokka wokka!!!"
    let diff = ham s1 s2
    ls <- BL.readFile "Question6.txt"
    let ct64 = BL.pack $ L.map BSI.c2w $ L.concatMap C8.unpack $ C8.lines ls
    let ct = L.head $ rights [B64.decode ct64]
    let keySizes=L.take 3 $ sort $ L.map (\n->(normDist n ct,n)) [6..40]
    let keySize = (fromIntegral $ snd $ L.head keySizes)::Int
    let pieces = BL.transpose $ L.map BL.pack $ Main.split (keySize) $ BL.unpack ct
    let keys = cprod $ L.map ((L.map fst).(bestIcs 1)) pieces
    System.IO.putStrLn $ "Question 6: hamming of given strings is " ++ (show $ L.map chr $ L.head keys)

    let tpts = L.map (\key -> (L.zipWith encrypt1 key pieces)) keys
    let pts = L.map (\pt -> BL.pack $ BL.unpack $ BL.concat $ BL.transpose pt) tpts
    let ks = L.map (\pt -> kapa $ txt2count $ BL.pack $ BL.unpack $ BL.concat $ BL.transpose pt) tpts
    let (k,pt) = maximumBy (comparing fst) (L.zip ks pts)
    
    System.IO.putStrLn $ "Question 6: hamming of given strings is " ++ (show diff)
    System.IO.putStrLn $ "Question 6: Best keySizes are: " ++ (show keySizes)
    System.IO.putStrLn $ "Question 6: Best keySize is: " ++ (show keySize)
    System.IO.putStrLn $ "Question 6: keys are: " ++ (show $ L.map chr $ L.head keys)
    System.IO.putStrLn $ "Question 6: PLain text is: " ++ (show pt)
    System.IO.putStrLn $ "Question 6: ic score is: " ++ (show k)

q7:: IO ()
q7 = do
    ls <- B.readFile "Question7.txt"
    let ct64 = B.take 3840 $ ls
    let ct = L.head $ rights [B64S.decode ct64]
    let key = B.pack $ L.map BSI.c2w "YELLOW SUBMARINE"
    let aes = initAES key
    let pt = decryptECB aes ct
    System.IO.putStrLn $ "Question7 plain text is: " ++ (show pt)


main::IO ()
main = do	
    System.IO.putStrLn $ "Input1 decoded is:" ++ (show $ asciiFromB16 input1)
    System.IO.putStrLn $ "Question1 = " ++ (show q1) ++ " which is the correct answer."
    System.IO.putStrLn $ "Key2 decoded is: " ++ (show $ asciiFromB16 key2)
    System.IO.putStrLn $ "Input2 decoded is:" ++ (show $ asciiFromB16 input2)
    System.IO.putStrLn $ "Input2 decoded and encrypted with key2 decoded is" ++ (show $ encrypt (asciiFromB16 key2) (asciiFromB16 input2))
    System.IO.putStrLn $ "Question2 = " ++ (show q2) ++ " which is the correct answer."
    System.IO.putStrLn $ "Question3 best ic is: " ++ (show $ L.head $ (bestIcs 1) $ asciiFromB16 input3) ++ " with plain text: " ++ (show $ encrypt1 (fst $ L.head $ bestIcs 1 $ asciiFromB16 input3) (fst $ B16.decode input3))
    q4
    q5
    q6
    q7

