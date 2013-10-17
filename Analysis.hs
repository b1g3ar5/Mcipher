module Analysis
    (
	--cShift
	cprod
	, nchr
	, asciiFromB16
	, asciiFromB64
	, asciiToB16
	, asciiToB64
	, nord
	, nAlphabet
	, count2freq
	, txt2count
	, kapa
	, scorr
	, totalFreq
	, totalFreqUpper
	, target
	, Analysis.toUpper
	, Analysis.toLower
	, cengFreq
	, wengFreq
	, cengMap
	, wengMap
	, asciiMap
	, ham
	, bham
	, hamlet
	, solve
	, encrypt
	, encrypt1
	, encrypt2
	, ic
	, icUpper
	, bestIcs
	, bestIcUppers
	, bestPlainTexts
	, normDist
    ) where
    

import System.IO
import Control.Monad
import Data.Array as A
import Data.Char as C
import Data.Word
import Data.Maybe
import Data.Either
import Data.Monoid
import Data.Map as M
import Data.List as L
import Text.Printf
import Data.Tuple (swap)
--import Data.ByteString as B
import Data.ByteString.Lazy as B  hiding (split) 
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Base16.Lazy as B16
import GHC.Exts
import Data.ByteString.Internal (c2w, w2c)
import Data.Bits
import Data.Int

--import Data.ByteString.Char8 (pack)
--import GHC.Word (Word8)

-- This is the cartesion product of a list of lists
cprod = L.foldr f [[]]
    where f xs yss = L.foldr g [] xs
           where g x zss = L.foldr h zss yss
                  where h ys uss = (x:ys):uss

right::Either a b -> b
right e = L.head $ rights [e]    

--asciiFromB16::ByteString->ByteString
asciiFromB16 = fst . B16.decode
asciiFromB64 = right . B64.decode
asciiToB64 = B64.encode
asciiToB16 = B16.encode

ham::[Char]->[Char]->Int
ham s1 s2 = L.sum $ L.zipWith (\c1 c2 -> hamlet (c2w c1) (c2w c2)) s1 s2

bham::ByteString->ByteString->Int
bham s1 s2 = L.sum $ B.zipWith hamlet s1 s2

-- Adds up the ones in the binary xor
hamlet::Word8->Word8->Int
hamlet w1 w2 = popCount $ w1 `xor` w2

solve::Int->ByteString->[(Int, Double, ByteString)]
solve n input = L.map (\kd -> let (key, diff)=kd in (key, diff, encrypt1 key input)) kds
    where
        kds = bestIcs n input

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

icUpper::Int->ByteString->Double
icUpper n bs = totalFreqUpper $ L.map (C.toUpper . w2c) $ B.unpack $ encrypt1 n bs
  
ic::Int->ByteString->Double
ic n bs = totalFreq $ L.map w2c $ B.unpack $ encrypt1 n bs
  
bestIcs::Int->ByteString->[(Int, Double)]
bestIcs n ct = L.map swap $ L.take n $ L.reverse $ sort $ L.map (\n-> (ic n ct, n)) [0..127]

bestIcUppers::Int->ByteString->[(Int, Double)]
bestIcUppers n ct = L.map swap $ L.take n $ L.reverse $ sort $ L.map (\n-> (icUpper n ct, n)) [65..90]

--    where
--        ics = L.map (\n-> (n, ic n ct) ) [0..127]
--        minDiff = L.foldl (\a nic -> if (snd nic > snd a) then nic else a) (-1,-1000.0) ics
--        next1Diff = L.foldl (\a nic -> if (snd nic > snd a) then nic else a) (-1,-1000.0) $ L.filter (\nic -> snd nic<snd minDiff) ics
--        next2Diff = L.foldl (\a nic -> if (snd nic > snd a) then nic else a) (-1,-1000.0) $ L.filter (\nic -> snd nic<snd next1Diff) ics
              
bestPlainTexts::Int->ByteString->[(ByteString, Double)]
bestPlainTexts n ct = L.map (\bd -> let (bs, d)=bd in (encrypt1 bs ct, d)) bds
    where
        bds = bestIcs n ct

-- Takes a keySize and a string
-- Gets 4 strings of size keySize from the input string
-- Works out the hamming distance (by doing an average with 4 strings)
-- Normalises by dividing by the keySize and returns that result
normDist::Int64->ByteString->Double
normDist keySize ct = 0.25/(fromIntegral keySize) * fromIntegral (diff1+diff2+diff3+diff4)
    where
        diff1 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*0) ct) $ B.take keySize $ B.drop (keySize*1) ct
        diff2 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*1) ct) $ B.take keySize $ B.drop (keySize*2) ct
        diff3 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*2) ct) $ B.take keySize $ B.drop (keySize*3) ct
        diff4 =  L.sum $ B.zipWith hamlet (B.take keySize $ B.drop (keySize*3) ct) $ B.take keySize $ B.drop (keySize*4) ct
                
ualphabet::String
ualphabet = L.map (\i -> C.toUpper $ w2c $ nchr i) [0..63]
 
alphabet::String
alphabet = L.map (\i -> w2c $ nchr i) [0..63]

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

--corr::ICount->Double
--corr (ICount fs) = M.foldl (+) 0.0 $ M.intersectionWith (\f l -> (fromIntegral f)*l) fs wengMap

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



-- Works out the correlation of an offset of n of a string with standard
-- english text frequencies
scorr::String->Double
scorr pt =  L.foldl (\acc l -> acc + if isJust $ M.lookup l asciiMap then fromJust $ M.lookup l asciiMap else 0.0) 0.0 pt
        
-- Adds up the expected frequencies of each UPPERCASE char in the pt
totalFreqUpper::String->Double
totalFreqUpper pt =  L.foldl (\acc l -> acc + if isJust $ M.lookup (C.toUpper l) cengMap then fromJust $ M.lookup (C.toUpper l) cengMap else 0.0) 0.0 pt
        
totalFreq::String->Double
totalFreq pt =  L.foldl (\acc l -> acc + if isJust $ M.lookup l asciiMap then fromJust $ M.lookup l asciiMap else 0.0) 0.0 pt
        
-- A maps of the frequencies, with Char ans Word8
wengMap = L.foldl (\m f -> M.insert (fst f) (snd f) m) M.empty wengFreq                  
cengMap = L.foldl (\m f -> M.insert (fst f) (snd f) m) M.empty cengFreq                  

-- Freqencies of letters, with Word8
wengFreq::[(Word8, Double)]
wengFreq = L.map (\s-> (c2w $ fst s, snd s)) cengFreq 
                  
-- A map of all acii characters
asciiMap::Map Char Double
asciiMap = L.foldl (\m f -> M.insert (fst f) (snd f) m) M.empty asciiFreq                  

target::Double
target = (L.sum $ L.map (\x->(snd x)*(snd x)) wengFreq)*(fromIntegral $ L.length wengFreq)


-- These are the frequencies of English text by letter        
cengFreq::[(Char, Double)]
cengFreq = [ ('A', 0.08167)
        ,('B',0.01492)
        ,('C',0.02782)
        ,('D',0.04253)
        ,('E',0.12702)
        ,('F',0.02228)
        ,('G',0.02015)
        ,('H',0.06094)
        ,('I',0.06966)	
        ,('J',0.00153)
        ,('K',0.00772)	
        ,('L',0.04025)	
        ,('M',0.02406)	
        ,('N',0.06749)	
        ,('O',0.07507)	
        ,('P',0.01929)	
        ,('Q',0.00095)	
        ,('R',0.05987)	
        ,('S',0.06327)	
        ,('T',0.09056)	
        ,('U',0.02758)	
        ,('V',0.00978)	
        ,('W',0.02360)	
        ,('X',0.00150)	
        ,('Y',0.01974)	 
        ,('Z',0.00074)]
                  

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


-- Frequencies of letters
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






