import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Control.Arrow
import System.Environment

-- convert a bunch of TSV fields to a FASTA
-- TODO: rewrite without (!!) operator!
getFasta :: ([B.ByteString], [B.ByteString]) -> B.ByteString
getFasta (tsvFields, bedFields) = let chr    = bedFields !! 0
                                      start  = bedFields !! 1
                                      stop   = bedFields !! 2
                                      bases  = tsvFields !! 5
                                      strand = bedFields !! 5
  in B.concat [B.pack ">", chr, B.pack ":", start, B.pack "-",  stop, B.pack ":", strand, B.pack "\n", bases]

processData :: B.ByteString -> B.ByteString -> B.ByteString
processData tsv bed = B.unlines $ (map getFasta) fieldTuples
  where fieldTuples = map (mapTuple $ B.split '\t') (zip (tail $ B.lines tsv) (B.lines bed))
        mapTuple = join (***)

main :: IO ()
main = do
  [tsvFile, bedFile] <- getArgs
  tsv <- B.readFile tsvFile
  bed <- B.readFile bedFile
  B.putStrLn $ processData tsv bed
