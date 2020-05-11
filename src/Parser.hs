module Parser where

import Lexer
import Syntax
import Text.Parsec ((<|>), ParseError, eof, many, parse, try)
import Text.Parsec.Expr (Assoc (AssocLeft), Operator (Infix), buildExpressionParser)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (whiteSpace)

binary s = Infix (reservedOp s >> return (BinaryOp s))

binops =
  [ [ binary "*" AssocLeft,
      binary "/" AssocLeft
    ],
    [ binary "+" AssocLeft,
      binary "-" AssocLeft
    ],
    [binary "<" AssocLeft]
  ]

int :: Parser Expr
int = Float . fromInteger <$> integer

floating :: Parser Expr
floating = Float <$> float

expr :: Parser Expr
expr = buildExpressionParser binops factor

variable :: Parser Expr
variable = Var <$> identifier

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  If cond tr <$> expr

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  Function name args <$> expr

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
    <|> try variable
    <|> ifthen
    <|> for
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
parseExpr = parse (contents expr) "<stdin>"

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel = parse (contents toplevel) "<stdin>"
