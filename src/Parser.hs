module Parser where

import Lexer
import Syntax
import Text.Parsec ((<|>), ParseError, eof, many, parse, try)
import Text.Parsec.Expr (Assoc (AssocLeft), Operator (Infix), buildExpressionParser)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (whiteSpace)

binary s assoc = Infix (reservedOp s >> return (BinaryOp s)) assoc

binops =
  [ [ binary "*" AssocLeft,
      binary "/" AssocLeft
    ],
    [ binary "+" AssocLeft,
      binary "-" AssocLeft
    ]
  ]

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

expr :: Parser Expr
expr = buildExpressionParser binops factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor =
  try floating
    <|> try int
    <|> try extern
    <|> try function
    <|> try call
    <|> variable
    <|> parens expr

defn :: Parser Expr
defn =
  try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s = parse (contents toplevel) "<stdin>" s
