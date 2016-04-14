-- Program accepts a FASTA file and two overhang sequences.
-- Outputs a text file, each line containing a guide sequence (or its
-- reverse complement) with the overhang sequence prepended at the 5' end.

import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment

type Overhang = String
guideLength = 20

process :: Overhang -> Overhang -> B.ByteString -> B.ByteString
process o1 o2 = B.unlines . (attachOverhangs o1 o2) . B.lines where

attachOverhangs :: Overhang -> Overhang -> [B.ByteString] -> [B.ByteString]
attachOverhangs o1 o2 [] = []
attachOverhangs o1 o2 (line1:bases:rest) =
  case (B.uncons line1, B.last line1) of
    (Just ('>', name), '+')
      -> (B.concat [name, B.pack "\t", B.pack o1, guide, B.pack "\n", (B.init name) `B.snoc` '-', B.pack "\t", B.pack o2, revComp guide]) : (attachOverhangs o1 o2 rest) where guide = removePam bases
    (Just ('>', name), '-')
      -> (B.concat [(B.init name) `B.snoc` '+', B.pack "\t", B.pack o1, revComp guide, B.pack "\n", name, B.pack "\t", B.pack o2, guide]) : (attachOverhangs o1 o2 rest) where guide = (B.reverse . removePam . B.reverse) bases
    _ -> error "FASTA record names must begin with '>' and end in either '-' or '+' for this utility"

-- remove the PAM sequence
removePam :: B.ByteString -> B.ByteString
removePam = B.take guideLength

-- reverse complement a ByteString
revComp :: B.ByteString -> B.ByteString
revComp b = B.reverse $ B.map baseComp b

-- complement a base
baseComp :: Char -> Char
baseComp c
  | c == 'A' = 'T'
  | c == 'C' = 'G'
  | c == 'G' = 'C'
  | c == 'T' = 'A'
  | c == 'N' = 'N'
  | otherwise = error "Unrecognized base"

main :: IO ()
main = do
       [overhang1, overhang2, fastaPath] <- getArgs
       fastaData <- B.readFile fastaPath
       B.putStr $ process overhang1 overhang2 fastaData
