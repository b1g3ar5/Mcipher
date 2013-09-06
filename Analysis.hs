module Analysis
    (
	--cShift
	nchr
	, nord
	, nAlphabet
--	, alphabet
--	, clean
	, count2freq
	, txt2count
	, kapa
	, corr
	, scorr
	, target
	, Analysis.toUpper
	, Analysis.toLower
	, engFreq
	, engMap
	, hamming
--	, splitIC
--	, ixOfMin
--	, ixOfMax
--	, splitText
--	, corr
--	, isUpperChar
--	, affineShift
--	, affineDeshift
--	, modinv
    ) where

import System.IO
import Control.Monad
import Data.Array as A
import Data.Char as C
import Data.Word
import Data.Maybe
import Data.Monoid
import Data.Map as M
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
import Data.ByteString as B

import Data.ByteString.Internal (c2w, w2c)
--import Data.ByteString.Char8 (pack)
--import GHC.Word (Word8)


hamming :: String -> String -> Int
hamming xs ys = L.length (L.filter not (L.zipWith (==) xs ys))

bhamming :: ByteString -> ByteString -> Int
bhamming = undefined
--bhamming xs ys = B.length (B.filter not (B.zipWith (==) xs ys))

-- Counts of the letters in a text
data  ICount = ICount (Map Word8 Int) deriving (Show)
data  DCount = DCount (Map Word8 Double) deriving (Show)

zeroCount::ICount
zeroCount = ICount $ fromList $ Prelude.map (\x->(nchr x,0)) [0..nAlphabet]

-- A monoid instance where zero is alphabet with 0 frequencies
instance Monoid ICount where
    mempty = zeroCount
    mappend (ICount c) (ICount d) = ICount $ M.union c d

-- insert a character into a map of characters
-- Increment the count by 1
cInsert::ICount->Word8->ICount
cInsert (ICount m) c = ICount $ insertWith (+) c 1 m
--cInsert (ICount m) c = if (ord $ w2c c)>64 && (ord $ w2c c)<91 then ICount $ insertWith (+) c 1 m else ICount $ insertWith (+) (c2w '~') 1 m
--cInsert (ICount m) c = if (ord $ w2c c)>64 && (ord $ w2c c)<91 then ICount $ insertWith (+) c 1 m else ICount m

-- counts the occurences of each character in a string
txt2count::ByteString->ICount
txt2count txt = B.foldl cInsert zeroCount txt

-- converts the count into a frequency
count2freq::ICount->DCount
count2freq (ICount c) = DCount $ M.map (\v-> fromIntegral v / fromIntegral tot ) c
        where
            tot = M.foldl (+) 0 c

-- Works out the incidence of coincidence
-- IC = sum{ f_i*(f_i-1)}/N/(N-1)*n
-- where N = length of the text (= sum of the f_i)
-- n = the size of the alphabet
-- f_i = the frequency of letter i
kapa::ICount->Double
kapa (ICount fs) = (M.foldlWithKey (\a k f -> (fromIntegral $ f*(f-1))+a) (0.0) fs) / (fromIntegral $ tot * (tot - 1)) * fromIntegral nAlphabet
        where
            tot = M.foldl (+) 0 fs

corr::ICount->Double
corr (ICount fs) = M.foldl (+) 0.0 $ M.intersectionWith (\f l -> (fromIntegral f)*l) fs engMap

-- Calculates the letter where 0 gives 'A'
nchr::Int->Word8
nchr i = c2w $ chr $ ord 'A' + i

-- Calculates ord where ord 'A'=0 etc.
nord::Word8->Int
nord c = ord (w2c c) - ord 'A'

nAlphabet = 26::Int


toLower :: ByteString -> ByteString
toLower bs = B.map (\w -> c2w $ C.toLower $ w2c w) bs

toUpper :: ByteString -> ByteString
toUpper bs = B.map (\w -> c2w $ C.toUpper $ w2c w) bs


{-
-- Cleans cipher text of everything except alphabetic characters
clean::(Word8->Bool)->[Word8]->[Word8]
clean _ [] = []
clean f (x:xs) = case f x of
			True -> x:(clean f xs)
			False-> clean f xs

isUpperChar::Word8->Bool
isUpperChar x = (nord x >= 0) && (nord x <26)



-- The number of letters in the alphabet
-- alphabet = "ABCDEFGHIJLMNOPQRSTUVWXYZ"

-- Shifts right according to an alphabet with nAlphabet Chars
cShift::Word8->Word8->Word8
cShift k c = nchr $ mod (nord k + nord c) nAlphabet

  
-- Switches a->z, b->y etc (when n=26)
reverseTxt::ByteString->ByteString       
-- reverseTxt txt = L.map (\c-> chr $ nAlphabet - ord c + 65) txt     
reverseTxt txt = B.pack $ L.map (\c-> c2w $ chr $ nAlphabet - 1 + 2*65 - (ord $ w2c c)) $ B.unpack txt     
            
-- works out the ic of a string split into n pieces (ie. autocorrelation)
splitIC::Int->ByteString->Double
splitIC n txt = (sum $ L.map (ic.txt2count) spl)/(fromIntegral n)
        where
            spl = splitText n txt
            
-- split a list into n bits alternating which bit to put the next item in
splitText::Int->[a]->[[a]]
splitText _ [] = []
splitText 1 xs = [xs]
splitText n xs = A.elems $ split' 0 n init xs
            where
                init::Array Int [a]
                init = listArray (0,n-1) $ B.replicate n ([])

split'::Int->Int->Array Int [a]->[a]->Array Int [a]
split' i n acc [] = acc
split' i n acc (x:xs) = split' (mod (i+1) n) n (accum (++) acc [(i, [x])]) xs

-- Index of the minimum starting with 0
ixOfMin::Ord a=>[a]->Int
ixOfMin xs = fromJust $ B.elemIndex (B.minimum xs) xs

-- Index of the minimum starting with 0
ixOfMax::Ord a=>[a]->Int
ixOfMax xs = fromJust $ B.elemIndex (B.maximum xs) xs

-- Shift a character by an int
cshift::Char->Char->Char
cshift k ' ' = ' '
cshift k c = chr $ (ord 'A') + (mod (ord k + ord c - ord 'A' - ord 'A') nAlphabet)

-- Shift a char x to a*x+b mod 26
affineShift::Int->Int->Char->Char
affineShift a b x = nchr $ mod ((a*nord x) + b) nAlphabet

-- Shift a char x to a*x+b mod 26
affineDeshift::Int->Int->Char->Char
affineDeshift a b x = nchr $ mod (inva*(nord x - b)) nAlphabet
	where
		inva = modinv nAlphabet a
-}

-- Works out the correlation of an offset of n of a string with standard
-- english text frequencies
scorr::String->Double
scorr pt =  L.foldl (\acc l -> acc + if isJust $ M.lookup l asciiMap then fromJust $ M.lookup l asciiMap else 0.0) 0.0 pt
        
  
-- These are the frequencies of English text by letter        
engFreq::[(Word8, Double)]
engFreq = [ (c2w 'A', 0.08167)
        ,(c2w 'B',0.01492)
        ,(c2w 'C',0.02782)
        ,(c2w 'D',0.04253)
        ,(c2w 'E',0.12702)
        ,(c2w 'F',0.02228)
        ,(c2w 'G',0.02015)
        ,(c2w 'H',0.06094)
        ,(c2w 'I',0.06966)	
        ,(c2w 'J',0.00153)
        ,(c2w 'K',0.00772)	
        ,(c2w 'L',0.04025)	
        ,(c2w 'M',0.02406)	
        ,(c2w 'N',0.06749)	
        ,(c2w 'O',0.07507)	
        ,(c2w 'P',0.01929)	
        ,(c2w 'Q',0.00095)	
        ,(c2w 'R',0.05987)	
        ,(c2w 'S',0.06327)	
        ,(c2w 'T',0.09056)	
        ,(c2w 'U',0.02758)	
        ,(c2w 'V',0.00978)	
        ,(c2w 'W',0.02360)	
        ,(c2w 'X',0.00150)	
        ,(c2w 'Y',0.01974)	 
        ,(c2w 'Z',0.00074)]
                  
engMap = L.foldl (\m f -> M.insert (fst f) (snd f) m) M.empty engFreq                  
                  
target::Double
target = (L.sum $ L.map (\x->(snd x)*(snd x)) engFreq)*(fromIntegral $ L.length engFreq)

bigramFreq = [("TH",0.03880),
	("HE",0.03680),
	("IN",0.02280),
	("ER",0.02180),
	("AN",0.02140),
	("RE",0.01750),
	("ND",0.01570),
	("AT",0.01420),
	("ON",0.01380),
	("NT",0.01340),
	("HA",0.01290),
	("ES",0.01280),
	("ST",0.01270),
	("EN",0.01170),
	("ED",0.01150),
	("TO",0.01135),
	("IT",0.01110),
	("OU",0.01090),
	("EA",0.01090),
	("HI",0.01050)]

trigramFreq = [("THE",0.03508232)
	, ("AND",0.01593878)
	, ("ING",0.01147042)
	, ("HER",0.00822444)
	, ("HAT",0.00650715)
	, ("HIS",0.00596748)
	, ("THA",0.00593593)
	, ("ERE",0.00560594)
	, ("FOR",0.00555372)
	, ("ENT",0.00530771)
	, ("ION",0.00506454)
	, ("TER",0.00461099)
	, ("WAS",0.00460487)
	, ("YOU",0.00437213)
	, ("ITH",0.00431250)
	, ("VER",0.00430732)
	, ("ALL",0.00422758)
	, ("WIT",0.00397290)
	, ("THI",0.00394796)
	, ("TIO",0.00378058)]


quadrigramFreq = [("THAT",0.00761242)
	,("THER",0.00604501)
	,("WITH",0.00573866)
	,("TION",0.00551919)
	,("HERE",0.00374549)
	,("OULD",0.00369920)
	,("IGHT",0.00309440)
	,("HAVE",0.00290544)
	,("HICH",0.00284292)
	,("WHIC",0.00283826)
	,("THIS",0.00276333)
	,("THIN",0.00270413)
	,("THEY",0.00262421)
	,("ATIO",0.00262386)
	,("EVER",0.00260695)
	,("FROM",0.00258580)
	,("OUGH",0.00253447)
	,("WERE",0.00231089)
	,("HING",0.00229944)
	,("MENT",0.00223347)]



asciiFreq::[(Char, Double)]
asciiFreq = [
         (' ', 0.171662) 
        ,('!', 0.000072) 
        ,('"', 0.002442) 
        ,('#', 0.000179) 
        ,('$', 0.000561) 
        ,('%', 0.000160) 
        ,('&', 0.000226) 
        , ('\'', 0.002447) 
        ,('(', 0.002178) 
        ,(')', 0.002233) 
        ,('*', 0.000628) 
        ,('+', 0.000215) 
        ,(',', 0.007384) 
        ,('-', 0.013734) 
        ,('.', 0.015124) 
        ,('/', 0.001549) 
        ,('0', 0.005516) 
        ,('1', 0.004594) 
        ,('2', 0.003322) 
        ,('3', 0.001847) 
        ,('4', 0.001348) 
        ,('5', 0.001663) 
        ,('6', 0.001153) 
        ,('7', 0.001030) 
        ,('8', 0.001054) 
        ,('9', 0.001024) 
        ,(':', 0.004354) 
        ,(';', 0.001214) 
        ,('<', 0.001225) 
        ,('=', 0.000227) 
        ,('>', 0.001242) 
        ,('?', 0.001474) 
        ,('@', 0.000073) 
        ,('A', 0.003132) 
        ,('B', 0.002163) 
        ,('C', 0.003906) 
        ,('D', 0.003151) 
        ,('E', 0.002673) 
        ,('F', 0.001416) 
        ,('G', 0.001876) 
        ,('H', 0.002321) 
        ,('I', 0.003211) 
        ,('J', 0.001726) 
        ,('K', 0.000687) 
        ,('L', 0.001884) 
        ,('M', 0.003529) 
        ,('N', 0.002085) 
        ,('O', 0.001842) 
        ,('P', 0.002614) 
        ,('Q', 0.000316) 
        ,('R', 0.002519) 
        ,('S', 0.004003) 
        ,('T', 0.003322) 
        ,('U', 0.000814) 
        ,('V', 0.000892) 
        ,('W', 0.002527) 
        ,('X', 0.000343) 
        ,('Y', 0.000304) 
        ,('Z', 0.000076) 
        ,('[', 0.000086) 
        ,('\'', 0.000016) 
        ,(']', 0.000088) 
        ,('_', 0.001159) 
        ,('a', 0.051880) 
        ,('b', 0.010195) 
        ,('c', 0.021129) 
        ,('d', 0.025071) 
        ,('e', 0.085771) 
        ,('f', 0.013725) 
        ,('g', 0.015597) 
        ,('h', 0.027444) 
        ,('i', 0.049019) 
        ,('j', 0.000867) 
        ,('k', 0.006753) 
        ,('l', 0.031750) 
        ,('m', 0.016437) 
        ,('n', 0.049701) 
        ,('o', 0.057701) 
        ,('p', 0.015482) 
        ,('q', 0.000747) 
        ,('r', 0.042586) 
        ,('s', 0.043686) 
        ,('t', 0.063700) 
        ,('u', 0.020999) 
        ,('v', 0.008462) 
        ,('w', 0.013034) 
        ,('x', 0.001950) 
        ,('y', 0.011330) 
        ,('z', 0.000596) 
        ,('•', 0.006410) ]

asciiMap::Map Char Double
asciiMap = L.foldl (\m f -> M.insert (fst f) (snd f) m) M.empty asciiFreq                  






