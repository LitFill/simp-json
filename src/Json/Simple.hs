{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

module Json.Simple where

import Data.Kind           (Type)
import Data.Map.Strict     (Map)
import Fmt
import Control.Applicative (Alternative (..))
import Data.Char           (isDigit, isSpace)
import Text.Read           (readEither)
import GHC.Generics        (Generic)

import Data.Map.Strict qualified as Map

data JsonValue :: Type where
    JsonNull   :: JsonValue
    JsonBool   :: Bool        -> JsonValue
    JsonNumber :: Integer     -> JsonValue
    JsonString :: String      -> JsonValue
    JsonArray  :: [JsonValue] -> JsonValue
    JsonObject :: Map String JsonValue -> JsonValue
    deriving stock (Show, Eq, Generic)

instance Buildable JsonValue where
    build JsonNull = "null"
    build (JsonBool True) = "true"
    build (JsonBool False) = "false"
    build (JsonNumber num) = build num
    build (JsonString str) = build str
    build (JsonArray list) = listF list
    build (JsonObject obj) = listF $ map buildPair (Map.toList obj)
    {-# INLINE build #-}

buildPair
    :: (Buildable a, Buildable b)
    => (a, b) -> Builder
buildPair (a, b) = "("+| a |+", "+| b |+")"

newtype Result :: Type -> Type where
    Result :: { getEither :: Either String a } -> Result a
    deriving (Generic)

instance {-# Overlappable #-} Buildable a => Show (Result a) where
    show = fmt . genericF

instance (Buildable a, Buildable b) => Show (Result (a, b)) where
    show r = case getEither r of
        Right pair -> show (Result (Right (buildPair pair)))
        Left err -> err

newtype Parser :: Type -> Type where
    Parser ::
        { runParser :: String -> Result (a, String)
        } -> Parser a

instance Functor Parser where
    fmap f (Parser p1) =
        Parser $ \input -> Result $ do
            (a, input') <- getEither $ p1 input
            pure (f a, input')

instance Applicative Parser where
    pure a = Parser $ \input -> Result $ pure (a, input)

    (Parser f) <*> (Parser a) =
        Parser $ \input -> Result $ do
            (f1, input1) <- getEither $ f input
            (a1, input2) <- getEither $ a input1
            pure (f1 a1, input2)

instance Functor Result where
    fmap f (Result ei) = Result $ fmap f ei

instance Applicative Result where
    pure = Result . pure
    Result f <*> Result ei = Result $ f <*> ei

instance Alternative Result where
    empty = Result . Left $ "empty"

    Result (Right a) <|> _ = Result (Right a)
    Result (Left  _) <|> b = b

instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  Result e1 >>= f = Result $ e1 >>= getEither . f

failR :: String -> Result a
failR = Result . Left

instance Alternative Parser where
    empty = Parser $ \_ -> Result $ Left "empty parser"

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

jsonNullP :: Parser JsonValue
jsonNullP = (\_ -> JsonNull) <$> stringP "null"

stringP :: String -> Parser String
stringP = sequenceA . map charP

charP :: Char -> Parser Char
charP c = Parser go
  where
    go (x : xs) | x == c = pure (c, xs)
    go (s : _ )          = failR $ "The char '"+| s |+"' is not '"+| c |+"'"
    go []                = failR "Empty input"

jsonBoolP :: Parser JsonValue
jsonBoolP = go <$> (stringP "true" <|> stringP "false")
  where
    go "true"  = JsonBool True
    go "false" = JsonBool False
    go _       = error "unreachable"

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ pure . span f

readResult :: Read a => String -> Result a
readResult = Result . readEither

jsonNumberP :: Parser JsonValue
jsonNumberP = Parser $ \input ->  do
    (numStr, input') <- runParser (spanP isDigit) input
    num <- readResult numStr
    pure (JsonNumber num, input')

jsonStringP :: Parser JsonValue
jsonStringP = JsonString <$> go
  where
    go = charP '"' *> spanP (/= '"') <* charP '"'
    -- TODO: add handling of escape char

wsP :: Parser String
wsP = spanP isSpace

commaP :: Parser Char
commaP = charP ','

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = go <|> pure []
  where
    go = fmap (:) element <*> many (sep *> element)

elemP :: Parser [JsonValue]
elemP = sepBy (wsP *> commaP <* wsP) jsonValueP

jsonArrayP :: Parser JsonValue
jsonArrayP = JsonArray <$> go
  where
    go = charP '[' *> wsP *> elemP <* wsP <* charP ']'

jsonValueP :: Parser JsonValue
jsonValueP =
    jsonNullP
    <|> jsonBoolP
    <|> jsonNumberP
    <|> jsonStringP
    <|> jsonArrayP
