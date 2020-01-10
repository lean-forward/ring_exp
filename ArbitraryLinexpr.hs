{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import System.IO.Extra
import System.Process.Typed
import Test.QuickCheck

data Var = A | B | C | D | E | F | G | H | I | J
  deriving (Bounded, Enum, Eq, Show)
instance (Arbitrary Var) where
  arbitrary = arbitraryBoundedEnum
newtype Linexpr = MkLinexpr {ofLinexpr :: [(Int, Either Var Linexpr)]}

instance (Arbitrary Linexpr) where
  arbitrary = do
    size <- getSize
    len <- choose (0, size)
    MkLinexpr <$> vectorOf len (resize (size - len) arbitrary)

instance (Show Linexpr) where
  show (MkLinexpr xs) = "(" ++ foldr (\x xs -> show (fst x) ++ " * " ++ showTerm (snd x) ++ " + " ++ xs) "0" xs ++ ")"
    where
    showTerm :: Either Var Linexpr -> String
    showTerm (Left x) = show x
    showTerm (Right x) = show x

totalize :: Var -> Linexpr -> Int
totalize _ (MkLinexpr []) = 0
totalize v (MkLinexpr ((n, Left v') : xs)) = (if v == v' then n else 0) + totalize v (MkLinexpr xs)
totalize v (MkLinexpr ((n, Right x) : xs)) = n * totalize v x + totalize v (MkLinexpr xs)

normalize :: Linexpr -> Linexpr
normalize xs = MkLinexpr $ map (\v -> (totalize v xs, Left v)) $ enumFromTo minBound maxBound

formatExample :: String -> Linexpr -> String
formatExample tactic xs = "example (A B C D E F G H I J : ℚ) : " ++ show xs ++ " = " ++ show (normalize xs) ++ " := by " ++ tactic

testcaseFile :: Linexpr -> String
testcaseFile xs = unlines
  [ "import tactic.ring tactic.ring_exp"
  , "set_option profiler true"
  , formatExample "ring" xs
  , formatExample "ring_exp" xs
  ]

runTest :: Int -> Linexpr -> IO ()
runTest size testcase = withTempFile $ \f -> do
  writeFile f $ testcaseFile testcase
  (out, err) <- readProcess_ $ proc "lean" [f]
  let lines = Text.lines (decodeUtf8 out)
  let times = catMaybes $ flip map lines $ Text.stripPrefix "elaboration: tactic execution took " >=> Text.stripSuffix "ms"
  case times of
    (timeRing : timeRingExp : []) -> putStrLn $ show size ++ "," ++ Text.unpack timeRing ++ "," ++ Text.unpack timeRingExp
    _ -> pure () -- Skip timeouts

main :: IO ()
main = flip mapM_ (cycle [20 .. 40]) $ \size -> do
  testcase <- generate (resize size arbitrary :: Gen Linexpr)
  runTest size testcase
