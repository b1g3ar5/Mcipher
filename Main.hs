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
        , decrypt
        , encrypt1
        , decrypt1
        , main
        , input1
        , input2
        , key2
        , input3
--        , bestPlainText
        , bestPlainText16
--        , bestPlainText64
--        , bestIc
        , bestIc16
--        , bestIc64
--        , ic
        , ic16
--        , ic64
        , target
        , hamming
        , corr
        , scorr
        , solve
        , txt2count
        , split
        , normDist
    ) where



import System.IO hiding ()
import Control.Monad
import Data.Array as A
import Data.Char as C
import Data.Word
import Data.Maybe
import Data.Map as M  hiding (split)
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
import Numeric
--import Data.ByteString as B hiding (putStrLn, ByteString)
import Data.ByteString.Lazy as B  hiding (split) -- (cycle, fromChunks, toChunks, zipWith, pack)
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Base16.Lazy as B16
import Data.ByteString.Lazy.Char8 as C8 hiding (ByteString, split)
import Data.Bits
import Analysis as A
import Data.ByteString.Internal (c2w, w2c)
import Data.Either
import Data.Bits
import Data.Int


input1::ByteString
input1 = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

input2::ByteString
input2 = "1c0111001f010100061a024b53535009181c"
key2::ByteString
key2 = "686974207468652062756c6c277320657965"

input3::ByteString
input3 = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- Answer should be SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t
-- Decode to ASCII
-- Encode to B64
q1 = B64.encode $ fst $ B16.decode input1


-- Answer should be 746865206b696420646f6e277420706c6179
-- Decode input2 and key2 to ASCII
-- xor them together
-- Encode to B16
q2 = B16.encode $ encrypt key16 input16
    where
        input16 = fst $ B16.decode input2
        key16 = fst $ B16.decode key2
     
       
-- Answer is "Cooking MC's like a pound of bacon"
-- Decode input3 as B16
-- Work out best key - ie. lowest IC difference
-- encrypt with this key

q3 = (key, diff, encrypt1 key input16)
    where
        (key, diff) = bestIc16 input3
        input16 = fst $ B16.decode input3

solve::ByteString->(Int, Double, ByteString)
solve input = (key, diff, encrypt1 key input)
    where
        (key, diff) = bestIc16 input
        input16 = fst $ B16.decode input


-- Reads the Q4 file 
-- Works out the best IC for all the lines
-- Chooses the best of the best
-- Works out the index of the best and returns the plain text with this index
q4 :: IO ()
q4 = do 
    ls <- liftM C8.lines $ B.readFile "Question4.txt"
    let ics = L.map bestIc16 ls
    let maxic = swap $ L.maximum $ L.map swap ics
    let pts = L.map bestPlainText16 ls
    let ix = fromJust $ L.findIndex (\i->snd i == snd maxic) ics
    System.IO.putStrLn $ "Question 4 answer is: " ++ (show (ix, fst $ ics!!ix, fst $ pts!!ix))
            
right::Either a b -> b
right e = L.head $ rights [e]    

-- Converts the input string and key
-- Encrypts the string using the cycled key as the key
q5 :: IO ()
q5 = do
    let input5 = "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"
    let cts = B.pack $ L.map c2w input5
    let inputkey = "ICE"
    let key = B.pack $ L.map c2w inputkey
    System.IO.putStrLn $ "Question 5: " ++ (show $ B16.encode $ encrypt cts $ B.cycle key)
    
        
split::Int->[a]->[[a]]
split n as = if L.length as <n then [as] else L.take n as : (Main.split n $ L.drop n as)
        
        
q6:: IO ()
q6 = do
    let s1 = "this is a test"
    let s2 = "wokka wokka!!!"
    let diff = ham s1 s2
    ls <- B.readFile "Question6.txt"
    let keySize = 4::Int64
    let diffs = [(normDist keySize ls, keySize) | keySize<-[4..40]]
    let minDiff = L.minimum diffs
    let keySize = (fromIntegral $ snd minDiff)::Int
    let chks1 = Main.split keySize $ B.unpack ls
    let chks2 = L.map B.pack $ L.reverse $ L.tail $ L.reverse chks1
    let tchks = B.transpose chks2
    let sols = L.map solve tchks
    System.IO.putStrLn $ "Question 6: hamming of given strings is " ++ (show diff)
    System.IO.putStrLn $ "Question 6: diffs over keySizes are: " ++ (show diffs)
    System.IO.putStrLn $ "Question 6: Best keySize is: " ++ (show minDiff)
    System.IO.putStrLn $ "Question 6: chks2: " ++ (show chks2)
    System.IO.putStrLn $ "Question 6: tchks: " ++ (show tchks)
    System.IO.putStrLn $ "Question 6: Solution is: " ++ (show sols)

-- Takes a keySize and a string
-- Gets 4 strings of size keySize from the input string
-- Works out the hamming distance (by doing an average with 4 strings)
-- Normalises by dividing by the keySize and returns that result

normDist::Int64->ByteString->Double
normDist keySize str = 0.25/(fromIntegral keySize) * fromIntegral (diff1+diff2+diff3+diff4)
    where
        diff1 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*0) str) $ B.take keySize $ B.drop (keySize*1) str
        diff2 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*1) str) $ B.take keySize $ B.drop (keySize*2) str
        diff3 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*2) str) $ B.take keySize $ B.drop (keySize*3) str
        diff4 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*3) str) $ B.take keySize $ B.drop (keySize*4) str

ham::[Char]->[Char]->Int
ham s1 s2 = L.sum $ L.zipWith (\c1 c2 -> hamlet (c2w c1) (c2w c2)) s1 s2

bham::ByteString->ByteString->Int
bham s1 s2 = L.sum $ B.zipWith hamlet s1 s2

hamlet::Word8->Word8->Int
hamlet w1 w2 = popCount $ w1 `xor` w2

-- encrpyts 2 equal length buffers        
encrypt::ByteString->ByteString->ByteString
encrypt key = B.pack . B.zipWith xor key
decrypt key = encrypt
 
-- encrypts a buffer with a a sinle character key
encrypt1::Int->ByteString->ByteString
encrypt1 key =  B.map (xor $ fromIntegral $ key) 
decrypt1 key = encrypt1
  
-- Encrypts with a repeating key (ie. Vignere cipher)
encrypt2::ByteString->ByteString->ByteString  
encrypt2 ks cts = encrypt cts $ B.cycle ks
  
ic16::Int->ByteString->Double
ic16 n bs = scorr $ L.map w2c $ B.unpack $ encrypt1 n $ fst $ B16.decode bs
  
ualphabet::String
ualphabet = L.map (\i -> C.toUpper $ w2c $ nchr i) [0..63]
 
alphabet::String
alphabet = L.map (\i -> w2c $ nchr i) [0..63]
        
bestIc16 ct = minDiff
    where
        ics = L.map (\n-> (n, ic16 n ct) ) [0..127]
        --diffs = L.map (\nic -> (fst nic, abs ((snd nic) - target) )) ics
        minDiff = L.foldl (\a nic -> if (snd nic > snd a) then nic else a) (-1,-1000.0) ics
              
main::IO ()
main = do	
    System.IO.putStrLn $ "Input1 decoded is:" ++ (show $ fst $ B16.decode input1)
    System.IO.putStrLn $ "Question1 = " ++ (show q1) ++ " which is the correct answer."
    System.IO.putStrLn $ "Key2 decoded is: " ++ (show $ fst $ B16.decode key2)
    System.IO.putStrLn $ "Input2 decoded is:" ++ (show $ fst $ B16.decode input2)
    System.IO.putStrLn $ "Input2 decoded and encrypted with key2 decoded is" ++ (show $ encrypt (fst $ B16.decode key2) (fst $ B16.decode input2))
    System.IO.putStrLn $ "Question2 = " ++ (show q2) ++ " which is the correct answer."
    System.IO.putStrLn $ "Question3 best ic is: " ++ (show $ bestIc16 input3) ++ " with plain text: " ++ (show $ encrypt1 (fst $ bestIc16 input3) (fst $ B16.decode input3))
    q4
    q5
    q6

bestPlainText16::ByteString->(ByteString, Double)
bestPlainText16 ct = (encrypt1 (fst $ bestIc16 ct) (fst $ B16.decode ct), snd $ bestIc16 ct)

	
