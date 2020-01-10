{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import GHC.IO.Exception
import System.IO.Extra
import System.Process.Typed
import Test.QuickCheck

data Var = A | B | C | D | E | F | G | H | I | J
  deriving (Bounded, Enum, Eq, Show)
instance (Arbitrary Var) where
  arbitrary = arbitraryBoundedEnum
data Expr
  = Var Var
  | Num Int
  | Add Expr Expr
  | Mul Expr Expr
  | Pow Expr (NonNegative Int)

instance (Arbitrary Expr) where
  arbitrary = do
    size <- getSize
    case size of
      0 -> oneof [Var <$> arbitrary, Num <$> arbitrary]
      _ -> do
        left <- choose (0, size)
        let right = size - left
        oneof
          [ Num <$> arbitrary
          , Add <$> resize left arbitrary <*> resize right arbitrary
          , Mul <$> resize left arbitrary <*> resize right arbitrary
          , Pow <$> resize left arbitrary <*> resize right arbitrary
          ]

instance (Show Expr) where
  show (Var v) = show v
  show (Num n) = "(" ++ show n ++ " : ℚ)"
  show (Add l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Mul l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
  show (Pow l (NonNegative r)) = "(" ++ show l ++ " ^ " ++ show r ++ ")"

formatExample :: String -> Expr -> String
formatExample tactic xs = "example (A B C D E F G H I J : ℚ) : " ++ show xs ++ " = " ++ show xs ++ " := by " ++ tactic

testcaseFile :: Expr -> String
testcaseFile xs = unlines
  [ "import tactic.ring tactic.ring_exp"
  , "set_option profiler true"
  , formatExample "ring" xs
  , formatExample "ring_exp" xs
  ]

unifyTime :: Text -> Maybe Float
unifyTime str = case Text.stripSuffix "ms" str of
  (Just ms) -> Just (read (Text.unpack ms) :: Float)
  Nothing -> case Text.stripSuffix "s" str of
    (Just sec) -> Just (1000 * read (Text.unpack sec) :: Float)
    Nothing -> Nothing

runTest :: Int -> Expr -> IO Bool
runTest size testcase = withTempFile $ \f -> do
  writeFile f $ testcaseFile testcase
  (code, out, err) <- readProcess $ proc "lean" [f]
  if code /= ExitSuccess then pure False else do
    let lines = Text.lines (decodeUtf8 out)
    let times = catMaybes $ flip map lines $ Text.stripPrefix "elaboration: tactic execution took " >=> unifyTime
    case times of
      (timeRing : timeRingExp : []) -> do
        putStrLn $ "# " ++ show testcase
        putStrLn $ show size ++ "," ++ show timeRing ++ "," ++ show timeRingExp
        pure True
      _ -> error (show lines)

main :: IO ()
main = flip mapM_ (cycle [20 .. 40]) $ \size -> do
  testcase <- generate (resize size arbitrary :: Gen Expr)
  runTest size testcase
