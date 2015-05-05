import Data.Array

-- I'm too lazy to do cycle detection, so let's make it at least logarithmic in c.
main :: IO ()
main = do
  lcn:rest <- fmap lines getContents
  let [l,c,n] = map read (words lcn)
      ps      = map read $ take n rest
      entry i =
        let (ps1,ps2) = splitAt i ps
            psums = takeWhile (<= l) $ scanl1 (+) (ps2++ps1)
        in  ((i + length psums) `mod` n,last psums)
      bnds    = (0,n-1)
      table   = listArray bnds $ map entry (range bnds)
      tableC  = foldl1 (<#>) . map fst . filter snd $ zip (iterate (\t -> t <#> t) table) (bits c)
  print $ snd (tableC ! 0)

type RCTable = Array Int (Int,Int)

(<#>) :: RCTable -> RCTable -> RCTable
table1 <#> table2 =
  listArray (bounds table1) $ map follow (assocs table1)
  where
    follow (i,(j,c)) =
      let (j',c') = table2 ! j
      in  (j',c+c')

bits :: Int -> [Bool]
bits 0 = []
bits n = (r == 1):bits q
  where (q,r) = n `divMod` 2
