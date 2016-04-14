-- Program accepts a SAM record as input, sorted by name, and outputs
-- corresponding FASTA and BED files for use in Galaxy CRISPR guide designer
-- pipeline

import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List
import Data.Maybe
import System.Environment
import Text.Regex.Posix

--
-- DATA TYPES
--

-- Structure of SAM record. Only relevant properties recorded
data Sam = Sam { qname :: B.ByteString
               , rname :: B.ByteString
               , start :: Int
               , stop  :: Int
               , bases :: B.ByteString 
               , strnd :: Strand }

-- Defines equality of SAM records for the "unique" function.
-- Two SAM records are equal is they originate from the SAM read,
-- regardless of where they align
instance Eq Sam where
  x == y = (qname x) == (qname y)

-- Simple algebraic sum type for strandedness of reads
data Strand = POS | NEG deriving (Eq)

instance Show Strand where
  show POS = "+"
  show NEG = "-"

type Cigar = String
type Pam = String

-- Converts a list of SAM fields into a SAM record object
fieldsToSam :: [B.ByteString] -> Sam
fieldsToSam (f1:f2:f3:f4:_:f6:_:_:_:f10:fs) = 
  Sam { qname = f1, rname = f3, start = x, stop = y, strnd = z, bases = f10 }
  where
    x = fst $ fromJust $ B.readInt f4
    y = x + (cigarToLength $ B.unpack f6)
    z = if (testBit (fst $ fromJust $ B.readInt f2) 4) then NEG else POS

-- Converts a SAM object into a FASTA record ByteString for appending
makeFasta :: Sam -> B.ByteString
makeFasta f = B.concat [ B.pack ">", qname f, B.pack "\n"
                       , bases f, B.pack "\n"] 

-- Converts a SAM object into a BED record ByteString for appending
makeBed :: Sam -> B.ByteString
makeBed f = B.intercalate (B.pack "\t") list where 
  list = [ rname f
         , B.pack $ show $ start f
         , B.pack $ show $ stop f
         , qname f
         , B.pack "0"
         , B.pack $ (show $ strnd f) ++ "\n" ]

-- Removes SAM records without the correct PAM sequence, then removes
-- multimappers. Assumes SAM records are in order by name
removeMultimappers :: Pam -> [Sam] -> [Sam]
removeMultimappers pam = (filter posIsConsistent) . 
                         unique . 
                         (filter (`hasPam` pam))

-- check if a SAM record has a given PAM sequence
hasPam :: Sam -> Pam -> Bool
hasPam sam pam = if (strnd sam) == POS
  then (B.pack pam) `B.isSuffixOf` bases sam
  else (revComp $ B.pack pam) `B.isPrefixOf` bases sam

-- Removes elements from a list that are not unique
-- Assumes that the list is sorted
unique :: Eq a => [a] -> [a]
unique = concat . filter isSingle . group
  where isSingle x = length x == 1

-- Reverse-complements a ByteString
revComp :: B.ByteString -> B.ByteString
revComp b = B.reverse $ B.map baseComp b

-- Complements a base
baseComp :: Char -> Char
baseComp c
  | c == 'A' = 'T'
  | c == 'C' = 'G'
  | c == 'G' = 'C'
  | c == 'T' = 'A'
  | c == 'N' = 'N'
  | otherwise = error "Unrecognized base"

-- Returns the length of the stretch of reference that a read spans, inferred
-- from the read's CIGAR string
cigarToLength :: Cigar -> Int
cigarToLength cigar = sum $ map (read . init) a where
  a = getAllTextMatches $ cigar =~ "[0-9]*[DMS]" :: [String]

-- Confirms that the start position in the name of the read corresponds
-- to the start position in the SAM record. A read is inconsistent if
-- 1. the chromosomes don't match
-- 2. the start coordinates don't match (within an epsilon < 4)
-- 3. the orientations don't match
posIsConsistent :: Sam -> Bool
posIsConsistent sam = (rname sam == rname') && 
                      (abs (start sam - start') < eps) && 
                      ((show $ strnd sam) == strnd') where
                        rname' = head $ B.split ':' (qname sam)
                        start' = fst $ fromJust $ B.readInt $ head $ 
                                 B.split '-' ((B.split ':' (qname sam)) !! 1)
                        strnd' = B.unpack $ last $ B.split ':' (qname sam)
                        eps    = 4

--
-- impure functions
--

-- Write a list of SAM records as a FASTA file and a BED file
toFiles :: (FilePath, FilePath) -> [Sam] -> IO ()
toFiles _ [] = return ()
toFiles (fastaPath, bedPath) (x:xs) = do
  B.appendFile fastaPath (makeFasta x)
  B.appendFile bedPath (makeBed x)
  toFiles (fastaPath, bedPath) xs

main :: IO ()
main = do
       [pam, samPath, fastaPath, bedPath] <- getArgs
       samData <- B.readFile samPath
       let filteredSamData = (removeMultimappers pam) $ 
                             (map fieldsToSam) $
                             (map (B.split '\t')) $
                             B.lines samData
       toFiles (fastaPath, bedPath) filteredSamData
