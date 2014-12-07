module Data.Suffix where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

data SuffixArray

toSufArray :: BS.ByteString -> SuffixArray
toSufArray = undefined

data Typ = STyp
         | LTyp
         deriving (Eq, Show)

instance Ord Typ where
    compare STyp LTyp = LT
    compare LTyp STyp = GT
    compare _ _       = EQ

data IsLMS = LMS Int
           | NotLMS
           deriving (Eq, Show)

-- | Classify characters as L- or S-types.
classifyLS :: V.Vector Char -> V.Vector Typ
classifyLS str = V.fromList . init . snd $ V.foldr go (maxBound, [STyp]) str
    where go next (prev, t) | next < prev = (next, STyp:t)
                            | next == prev && head t == STyp = (next, STyp:t)
                            | otherwise = (next, LTyp:t)

-- | Get all LMS substrings.
lmsSubs :: V.Vector Typ -> V.Vector IsLMS
lmsSubs t = V.zipWith3 go enums (V.init t') (V.tail t')
    where
      go idx LTyp STyp = LMS idx
      go _ _ _         = NotLMS
      t' = V.cons STyp t
      enums = V.fromList [0..V.length t]

-- |
induceSort :: V.Vector Char -> V.Vector IsLMS -> (V.Vector Int, [Int])
induceSort chars lms = (toVInts bkts, scanl (+) 0 $ Seq.length <$> Map.elems bkts)
    where
      go bkt (c, LMS idx) = addToBucket c idx bkt
      go bkt (c, NotLMS ) = addToBucket c (negate 1) bkt
      toVInts = V.fromList . concat . Map.elems . fmap toList
      bkts = V.foldl go Map.empty $ V.zip chars lms

type Buckets = Map.Map Char (Seq.Seq Int)

addToBucket :: Char -> Int -> Buckets -> Buckets
addToBucket c (- 1) bkt = case Map.lookup c bkt of
                        Nothing -> Map.insert c (Seq.singleton (- 1)) bkt
                        Just _ -> Map.adjust ((-1) Seq.<|) c bkt
addToBucket c i bkt = case Map.lookup c bkt of
                        Nothing -> Map.insert c (Seq.singleton i) bkt
                        Just _ -> Map.adjust (Seq.|> i) c bkt


      {-cmpSub (s,e) (s',e')-}
        {-| s < e && s >= e = LT-}
        {-| s < e           = EQ-}
        {-| otherwise = case compare (str `index` s) (str `index` s') of-}
              {-EQ -> case compare (typ V.! s) (typ V.! s) of-}
                  {-EQ -> cmpSub (s+1,e) (s'+1,e')-}
                  {-x  -> x-}
              {-y -> y-}

eg :: V.Vector Char
eg = V.fromList "mmiissiissiippii\NUL"
