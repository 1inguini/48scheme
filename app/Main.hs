module Main where

-- import           Lib

import qualified Control.Exception.Safe as Exception
import Data.Functor (($>))
import qualified Data.Generics.Sum as GSum
import qualified Data.List as List
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Optics as Op
import System.Environment (getArgs)

-- import Text.Megaparsec
-- import qualified Text.Megaparsec.Char as MChar
-- import qualified Text.Megaparsec.Char.Lexer as L

data LispVal
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | String T.Text
  | Bool Bool
  deriving (Generic, Eq, Show)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | --   | Parser ParseError
    BadSpecialForm T.Text LispVal
  | -- | NotFunction T.Text T.Text
    UnboundVar T.Text
  | Default T.Text
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

foldOp :: Exception.MonadThrow m => (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> m LispVal
foldOp op identity ls =
  let mNumbers = Op.preview (GSum._As @"Number") <$> ls
   in case List.elemIndices Nothing mNumbers of
        [] -> pure $ Number $ foldl op identity $ Maybe.catMaybes mNumbers
        indices -> Exception.throw $ TypeMismatch "Number" . (ls !!) <$> indices

primitivesEnv :: Exception.MonadThrow m => Map.Map T.Text ([LispVal] -> m LispVal)
primitivesEnv =
  Map.fromList
    [ ("+", foldOp (+) 0),
      ("-", foldOp (-) 0),
      ("*", foldOp (*) 1) -- ,
      -- ("/", foldOp div 1)
    ]

eval :: Exception.MonadThrow m => LispVal -> m LispVal
eval (List (Atom func : args)) = do
  args <- traverse eval args
  apply func args
eval val = pure val

apply :: forall m. Exception.MonadThrow m => T.Text -> [LispVal] -> m LispVal
apply funcName args =
  case primitivesEnv !? funcName :: Maybe ([LispVal] -> m LispVal) of
    Nothing -> Exception.throw [UnboundVar funcName]
    Just f -> f args

main :: IO ()
main = do
  print $
    (eval :: LispVal -> Either Exception.SomeException LispVal)
      <$> [ List [Atom "+", Number 1, Number 3],
            List [Atom "*", Number 1, Number 3, Number 1, Number 3]
          ]
