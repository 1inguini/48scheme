module Main where

-- import           Lib

import qualified Control.Exception.Safe as Exception
import Data.Complex (Complex (..))
import qualified Data.Complex as Complex
import Data.Foldable (foldlM)
import Data.Functor (($>))
import qualified Data.Generics.Sum as GSum
import qualified Data.List as List
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Ratio ((%))
import qualified Data.Ratio as Ratio
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Optics as Op
import System.Environment (getArgs)

-- import Text.Megaparsec
-- import qualified Text.Megaparsec.Char as MChar
-- import qualified Text.Megaparsec.Char.Lexer as L

data LispVal
  = Atom Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number LispNumber
  | Float Float
  | String Text
  | Bool Bool
  deriving (Generic, Eq, Show)

data LispNumber
  = Complex LispComplex
  | Real Double
  | Rational Rational
  | Integer Integer
  deriving (Generic, Eq, Show)

type LispComplex = Complex Double

-- lispNumComplex = Op.iso toComplex toLispNumber

lispNumToComplex :: LispNumber -> LispComplex
lispNumToComplex (Complex x) = x
lispNumToComplex (Real x) = x :+ 0
lispNumToComplex (Rational x) = fromRational x :+ 0
lispNumToComplex (Integer x) = fromInteger x :+ 0

lispNumToDouble :: Exception.MonadThrow m => LispNumber -> m Double
lispNumToDouble x@(Complex _) = Exception.throw [TypeMismatch "Real" $ Number x]
lispNumToDouble (Real x) = pure x
lispNumToDouble (Rational x) = pure $ fromRational x
lispNumToDouble (Integer x) = pure $ fromInteger x

lispNumToRational :: Exception.MonadThrow m => LispNumber -> m Rational
lispNumToRational x@(Complex _) = Exception.throw [TypeMismatch "Rational" $ Number x]
lispNumToRational x@(Real _) = Exception.throw [TypeMismatch "Rational" $ Number x]
lispNumToRational (Rational x) = pure x
lispNumToRational (Integer x) = pure $ fromInteger x

lispNumToInteger :: Exception.MonadThrow m => LispNumber -> m Integer
lispNumToInteger x@(Complex _) = Exception.throw [TypeMismatch "Integer" $ Number x]
lispNumToInteger x@(Real _) = Exception.throw [TypeMismatch "Integer" $ Number x]
lispNumToInteger x@(Rational _) = Exception.throw [TypeMismatch "Integer" $ Number x]
lispNumToInteger (Integer x) = pure x

-- toLispNumber :: LispComplex -> LispNumber
-- toLispNumber (r :+ 0)
--   | Ratio.denominator r == 0 = Integer $ Ratio.numerator r
--   | otherwise = Rational r
-- toLispNumber c = Complex c

-- apNumOpToLispNumber ::
--   (Complex Rational -> Complex Rational -> Complex Rational) ->
--   (LispNumber -> LispNumber -> LispNumber)
-- apNumOpToLispNumber op x y = toLispNumber $ toComplex x `op` toComplex y

-- instance Num LispNumber where
--   (+) = apNumOpToLispNumber (+)
--   (-) = apNumOpToLispNumber (-)
--   (*) = apNumOpToLispNumber (*)
--   abs = Op.over lispNumComplex abs
--   signum = Op.over lispNumComplex signum
--   fromInteger = toLispNumber . fromInteger

-- instance Fractional LispNumber where
--   fromRational = toLispNumber . fromRational
--   recip = Op.over lispNumComplex recip

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch Text LispVal
  | --   | Parser ParseError
    BadSpecialForm Text LispVal
  | -- | NotFunction T.Text T.Text
    UnboundVar Text
  | Default Text
  deriving (Generic, Eq, Show)

instance Exception.Exception [LispError]

type LispThrowErr = Either [LispError]

-- type Parser = Parsec Void T.Text

-- symbolChar :: Parser Char
-- symbolChar = oneOf "!#$%&|*+-/:<=>?@^_~"

-- spaceConsumer :: Parser ()
-- spaceConsumer = L.space MChar.space1 empty empty

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme spaceConsumer

-- symbol :: T.Text -> Parser T.Text
-- symbol = L.symbol' spaceConsumer

-- -- readExpr :: T.Text -> T.Text
-- -- readExpr input =
-- --   case parse (space >> symbolChar) "lisp" input of
-- --     Left err -> "No match: " <> T.pack (errorBundlePretty err)
-- --     Right val -> "Found value: " <> val

-- parseExpr :: Parser LispVal
-- parseExpr = lexeme $ Atom . T.singleton <$> symbolChar

-- parseString :: Parser LispVal
-- parseString = do
--   x <- between (symbol "\"") (symbol "\"") $ takeWhileP Nothing (/= '\'')
--   return $ String x

-- parseAtom :: Parser LispVal
-- parseAtom = do
--   first <- MChar.letterChar <|> symbolChar
--   rest <- many (letter <|> digit <|> symbol)
--   let atom = first : rest
--   return $ case atom of
--     "#t" -> Bool True
--     "#f" -> Bool False
--     _ -> Atom atom

-- parseNumber :: Parser LispVal
-- parseNumber = Number . read <$> many1 digit

foldNumOp :: Exception.MonadThrow m => (LispNumber -> LispNumber -> m LispNumber) -> LispNumber -> [LispVal] -> m LispVal
foldNumOp op identity ls =
  let mNumbers = Op.preview (GSum._As @"Number") <$> ls
   in case List.elemIndices Nothing mNumbers of
        [] -> Number <$> foldlM op identity (Maybe.catMaybes mNumbers)
        indices -> Exception.throw $ TypeMismatch "Number" . (ls !!) <$> indices

binNumOp :: Exception.MonadThrow m => (LispNumber -> LispNumber -> m LispNumber) -> [LispVal] -> m LispVal
binNumOp op [Number n, Number m] = Number <$> op n m
binNumOp op args@[_, _] =
  Exception.throw $
    TypeMismatch "Number"
      <$> List.filter (\case Number _ -> True; _ -> False) args
binNumOp op args = Exception.throw [NumArgs 2 args]

numOpToLispNumOp ::
  Exception.MonadThrow m =>
  (forall a. Num a => a -> a -> a) ->
  (LispNumber -> LispNumber -> m LispNumber)
numOpToLispNumOp op (Integer x) (Integer y) = pure $ Integer $ op x y
numOpToLispNumOp op (Integer x) (Rational y) = pure $ Rational $ op (fromIntegral x) y
numOpToLispNumOp op (Integer x) (Real y) = pure $ Real $ op (fromIntegral x) y
numOpToLispNumOp op (Rational x) (Integer y) = pure $ Rational $ op x $ fromIntegral y
numOpToLispNumOp op (Rational x) (Rational y) = pure $ Rational $ op x y
numOpToLispNumOp op (Rational x) (Real y) = pure $ Real $ op (fromRational x) y
numOpToLispNumOp op (Real x) (Integer y) = pure $ Real $ op x $ fromIntegral y
numOpToLispNumOp op (Real x) (Rational y) = pure $ Real $ op x $ fromRational y
numOpToLispNumOp op (Real x) (Real y) = pure $ Real $ op x y
numOpToLispNumOp op x y = pure $ Complex $ op (lispNumToComplex x) (lispNumToComplex y)

fractionalOpToLispNumOp ::
  Exception.MonadThrow m =>
  (forall a. Fractional a => a -> a -> a) ->
  (LispNumber -> LispNumber -> m LispNumber)
fractionalOpToLispNumOp op (Integer x) (Integer y) = pure $ Rational $ op (fromIntegral x) (fromIntegral y)
fractionalOpToLispNumOp op (Integer x) (Rational y) = pure $ Rational $ op (fromIntegral x) y
fractionalOpToLispNumOp op (Integer x) (Real y) = pure $ Real $ op (fromIntegral x) y
fractionalOpToLispNumOp op (Rational x) (Integer y) = pure $ Rational $ op x $ fromIntegral y
fractionalOpToLispNumOp op (Rational x) (Rational y) = pure $ Rational $ op x y
fractionalOpToLispNumOp op (Rational x) (Real y) = pure $ Real $ op (fromRational x) y
fractionalOpToLispNumOp op (Real x) (Integer y) = pure $ Real $ op x $ fromIntegral y
fractionalOpToLispNumOp op (Real x) (Rational y) = pure $ Real $ op x $ fromRational y
fractionalOpToLispNumOp op (Real x) (Real y) = pure $ Real $ op x y
fractionalOpToLispNumOp op x y = pure $ Complex $ op (lispNumToComplex x) (lispNumToComplex y)

integerOpToLispNumOp ::
  Exception.MonadThrow m =>
  (forall a. Integral a => a -> a -> a) ->
  (LispNumber -> LispNumber -> m LispNumber)
integerOpToLispNumOp op (Integer x) (Integer y) = pure $ Integer $ op x y
integerOpToLispNumOp _ x y = Exception.throw $ TypeMismatch "Integer" . Number <$> [x, y]

primitivesEnv :: Exception.MonadThrow m => Map.Map Text ([LispVal] -> m LispVal)
primitivesEnv =
  Map.fromList
    [ ("+", foldNumOp (numOpToLispNumOp (+)) $ Integer 0),
      ("-", foldNumOp (numOpToLispNumOp (-)) $ Integer 0),
      ("*", foldNumOp (numOpToLispNumOp (*)) $ Integer 1),
      ("/", foldNumOp (fractionalOpToLispNumOp (/)) $ Integer 1),
      ("quotient", binNumOp $ integerOpToLispNumOp quot),
      ("remainder", binNumOp $ integerOpToLispNumOp rem),
      ("modulo", binNumOp $ integerOpToLispNumOp mod)
    ]

eval :: Exception.MonadThrow m => LispVal -> m LispVal
eval (List [Atom "quote", ls]) = pure ls
eval (List (Atom func : args)) = do
  args <- traverse eval args
  apply func args
eval val = pure val

apply :: forall m. Exception.MonadThrow m => Text -> [LispVal] -> m LispVal
apply funcName args =
  case primitivesEnv !? funcName :: Maybe ([LispVal] -> m LispVal) of
    Nothing -> Exception.throw [UnboundVar funcName]
    Just f -> f args

main :: IO ()
main = do
  print $
    (eval :: LispVal -> Either Exception.SomeException LispVal)
      <$> [ List [Atom "+", Number $ Integer 1, Number $ Integer 3],
            List [Atom "*", Number $ Integer 1, Number $ Real 3, Number $ Rational 1, Number $ Complex 3],
            List [Atom "/", Number $ Real 1, Number $ Real 3, Number $ Rational 2, Number $ Integer 3],
            List [Atom "remainder", Number $ Integer 12, Number $ Integer 5]
          ]
