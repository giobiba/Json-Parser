{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}
module JSONParser where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt)
import Data.Functor(($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Positive, Negative)

data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: [Int], exponent :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Generic)

-- instance of show for the JSON file

instance Show JValue where
    show value = case value of
        JNull           -> "null"
        JBool True      -> "true"
        JBool False     -> "false"
        JString s       -> showJSONString s
        JNumber s [] 0  -> show s
        JNumber s f  0  -> show s ++ "." ++ concatMap show f
        JNumber s [] e  -> show s ++ "e" ++ show e
        JNumber s f  e  -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
        JArray a        -> "[" ++ intercalate ", " (show <$> a) ++ "]"
        JObject o       -> "{" ++ intercalate ", " (showPair <$> o) ++ "}"
        where
            showPair (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0'..'\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
        '\'' -> "'"
        '\"' -> "\\\""
        '\\' -> "\\\\"
        '/'  -> "\\/"
        '\b' -> "\\b"
        '\f' -> "\\f"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
        _ -> [c]
        where
            showJSONNonASCIIChar c =
                let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a

-- creating generators for the JSON

jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary


-- for the string generator we need to include both unescaped and escaped characters
stringGen :: Gen String
stringGen =
    concat <$> listOf (oneof [vectorOf 1 arbitraryUnicodeChar
                              , escapedUnicodeChar])
    where
        escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
        hexDigitLetters    = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> stringGen

-- generating the complex types
jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
    where
        objKV n = (,) <$> stringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n =
    frequency [(4, frequency scalarGens), (1, oneof (compositeGens n))]
    where
        scalarGens      = [(1,jNullGen), (1,jBoolGen), (2, jNumberGen), (2, jStringGen)]
        compositeGens n =  [jArrayGen n, jObjectGen n]

instance Arbitrary JValue where
    arbitrary = sized jValueGen
    shrink    = genericShrink

jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
    scale (round . sqrt . fromIntegral)
    . listOf
    . elements
    $ [' ' , '\n' , '\r' , '\t']
