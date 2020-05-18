module Parser where

import Lexer
import Syntax
import Text.Parsec ((<|>), ParseError, eof, many, parse, try)
import Text.Parsec.Expr (Assoc (AssocLeft), Operator (Infix, Prefix), buildExpressionParser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (opLetter, opStart, whiteSpace)

binary s = Infix (reservedOp s >> return (BinaryOp s))

binops =
  [ [binary "=" AssocLeft],
     [ binary "*" AssocLeft,
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
expr = buildExpressionParser (binops ++ [[unop], [binop]]) factor

binarydef :: Parser Expr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ many identifier
  BinaryDef o args <$> expr

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  UnaryDef o args <$> expr

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
  For var start cond step <$> expr

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

operator :: Parser String
operator = do
  c <- opStart emptyDef
  cs <- many $ opLetter emptyDef
  return (c : cs)

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

unop = Prefix (UnaryOp <$> op)

binop = Infix (BinaryOp <$> op) AssocLeft

letins :: Parser Expr
letins = do
  reserved "var"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs

factor :: Parser Expr
factor =
  try floating
    <|> try int
    <|> try call
    <|> try variable
    <|> ifthen
    <|> try letins
    <|> for
    <|> parens expr

defn :: Parser Expr
defn =
  try extern
    <|> try function
    <|> try unarydef
    <|> try binarydef
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
