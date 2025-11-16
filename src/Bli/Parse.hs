{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Bli.Parse where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Char.Lexer qualified as L

import Bli.Ast
import Bli.Error (BliException)
import Control.Monad (void, when, liftM2)
import Data.Maybe (isNothing, fromMaybe, catMaybes)

type Parser = Parsec BliException Text

pNum :: Parser Literal
pNum = LitNum <$> (try float <|> integer) <?> "number literal"
  where
    float :: Parser Float
    float = lexeme L.float
    integer :: Parser Float
    integer = lexeme L.decimal

pStr :: Parser Literal
pStr = do
  strToken <- lexeme (char '"' >> manyTill charLiteral (char '"') <?> "string literal")
  let str = LitStr $ T.pack strToken
  return str

pBool :: Parser Literal
pBool =
  choice
    [ LitBool True <$ symbol "true"
    , LitBool False <$ symbol "false"
    ] <?> "boolean literal"

pNil :: Parser Literal
pNil =
  LitNil <$ symbol "nil" <?> "nil"

pSuper :: Parser Literal
pSuper = do
  void $ symbol "super"
  void $ symbol "."
  var <- pVar
  return $ LitSuper var

pIdent :: Parser Ident
pIdent =
  Ident . T.pack <$> lexeme ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_') <?> "identifier")

pVar :: Parser Var
pVar = do
  (Ident ident) <- pIdent
  return $ Var ident

pOperand :: Parser Expr
pOperand = try $ do
  choice
    [ ExprLit <$> try (pNum <|> pStr <|> pBool <|> pNil <|> pSuper)
    , ExprGroup <$> try (between (symbol "(") (symbol ")") pExpr)
    , ExprVar <$> try pVar
    ]

pAsgn :: Parser Expr
pAsgn = try $ do
  lval <-
    choice
      [ try pSet
      , LValVar <$> try pVar
      ]
  void $ symbol "="
  rval <- pExpr
  return . ExprAsgn $ Asgn lval rval

pCallArgs :: Parser [Expr]
pCallArgs = try $ do
  sepBy pExpr (symbol ",")

pCall :: Parser (Expr -> Expr)
pCall = try $ do
  void $ symbol "("
  args <- try pCallArgs
  void $ symbol ")"
  return $ \target -> ExprCall target args

pGet :: Parser (Expr -> Expr)
pGet = try $ do
  void $ symbol "."
  prop <- pVar <* notFollowedBy (symbol "=")
  return $ \obj -> ExprGet obj prop

pSet :: Parser LVal
pSet = try $ do
  objExpr <-
    composeTerms
      (ExprVar <$> pVar)
      (many (pCall <|> pGet))
  void $ symbol "."
  field <- pVar
  return $ LValSet objExpr field

-- | Supports construction of an AST tree by threading
-- a term through a sequence of functions which augment
-- the AST.
-- Useful for supporting syntax such as:
-- f()().x.y where a sequence of calls and field accesses
-- occur.
composeTerms :: Parser a -> Parser [a -> a] -> Parser a
composeTerms = liftM2 (foldl (flip ($)))

pExpr :: Parser Expr
pExpr = try pAsgn <|> pExpr'

pPostfix :: Parser Expr
pPostfix = 
  composeTerms pOperand (many (pCall <|> pGet))

pExpr' :: Parser Expr
pExpr' =
  makeExprParser
    pPostfix
    [
      [ prefix "-" (ExprUn . UnExpr UnNeg)
      , prefix "!" (ExprUn . UnExpr UnNot)
      ]
    ,
      [ binary "*" (\x y -> ExprBin $ BinExpr BinMul x y)
      , binary "/" (\x y -> ExprBin $ BinExpr BinDiv x y)
      ]
    ,
      [ binary "+" (\x y -> ExprBin $ BinExpr BinAdd x y)
      , binary "-" (\x y -> ExprBin $ BinExpr BinSub x y)
      ]
    ,
      [ binary "<=" (\x y -> ExprBin $ BinExpr BinLte x y)
      , binary "<" (\x y -> ExprBin $ BinExpr BinLt x y)
      , binary ">=" (\x y -> ExprBin $ BinExpr BinGte x y)
      , binary ">" (\x y -> ExprBin $ BinExpr BinGt x y)
      ]
    ,
      [ binary "==" (\x y -> ExprBin $ BinExpr BinEq x y)
      , binary "!=" (\x y -> ExprBin $ BinExpr BinNeq x y)
      ]
    ,
      [ binary "and" (\x y -> ExprBin $ BinExpr BinLogAnd x y)
      ]
    ,
      [ binary "or" (\x y -> ExprBin $ BinExpr BinLogOr x y)
      ]
    ]
  where
    binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary op ctor = InfixL $ ctor <$ symbol op
    prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
    prefix op ctor = Prefix $ ctor <$ symbol op

pStmt :: Parser Stmt
pStmt =
  choice
    [ try pStmtExpr
    , StmtPrint <$> try (between (symbol "print" ) (symbol ";") pExpr)
    , try pStmtVarDecl
    , try pStmtWhile
    , try pStmtFor
    , try pStmtBlock
    , try pStmtIf
    , try pStmtFuncDecl
    , try pStmtClassDecl
    , try pStmtReturn
    ]

pStmtBlock :: Parser Stmt
pStmtBlock =
  StmtBlock <$> try (between (symbol "{") (symbol "}") $ many pStmt)

pStmtExpr :: Parser Stmt
pStmtExpr = StmtExpr <$> try (pExpr <* symbol ";")

pStmtVarDecl :: Parser Stmt
pStmtVarDecl = do
    void $ symbol "var"
    name <- pVar
    mVal <- try . optional $ symbol "=" *> pExpr
    void $ symbol ";"
    case mVal of
      Just val -> return $ StmtVarDecl name val
      Nothing -> return $ StmtVarDecl name (ExprLit LitNil)

pStmtWhile :: Parser Stmt
pStmtWhile = do
  void $ symbol "while"
  void $ symbol "("
  cond <- pExpr
  void $ symbol ")"
  body <- pStmt
  return $ StmtWhile cond body

pStmtFor :: Parser Stmt
pStmtFor = do
  void $ symbol "for"
  void $ symbol "("
  mInit <- optional $ choice [ try pStmtVarDecl
                             , try pStmtExpr
                             ]
  -- If no variable declaration or assignment
  -- is present, we expect a lone semicolon
  when (isNothing mInit) (void $ symbol ";")
  -- Parse the optional condition expression
  mCond <- try . optional $ pExpr
  void $ symbol ";"
  -- Parse the optional increment expression
  mIncr <- try . optional $ pExpr
  void $ symbol ")"
  body <- pStmt

  let whileBody = case mIncr of
        Just incr -> StmtBlock [body, StmtExpr incr]
        Nothing -> body
      while = StmtWhile (fromMaybe (ExprLit (LitBool True)) mCond) whileBody
  return $ StmtBlock (catMaybes [mInit, Just while])

pStmtIf :: Parser Stmt
pStmtIf = do
  void $ symbol "if"
  void $ symbol "("
  cond <- pExpr
  void $ symbol ")"
  trueBody <- pStmt
  falseBody <- try . optional $ symbol "else" *> pStmt
  return $ StmtIf cond trueBody falseBody

pFuncParams :: Parser [Var]
pFuncParams =
  sepBy pVar (symbol ",")

pStmtFuncDecl :: Parser Stmt
pStmtFuncDecl = do
  void $ symbol "fun"
  name <- pVar
  void $ symbol "("
  params <- pFuncParams
  void $ symbol ")"
  block <- pStmtBlock
  return $ StmtFuncDecl name params block

pStmtMethodDecl :: Parser Stmt
pStmtMethodDecl = do
  name <- pVar
  void $ symbol "("
  params <- pFuncParams
  void $ symbol ")"
  block <- pStmtBlock
  return $ StmtFuncDecl name params block

pStmtClassDecl :: Parser Stmt
pStmtClassDecl = do
  void $ symbol "class"
  name <- pVar
  mSuper <- try . optional $ symbol "<" *> pVar
  methods <- try (between (symbol "{") (symbol "}") $ many pStmtMethodDecl)
  return $ StmtClassDecl name mSuper methods

pStmtReturn :: Parser Stmt
pStmtReturn = do
  void $ symbol "return"
  val <- pExpr
  void $ symbol ";"
  return $ StmtReturn val

pProg :: Parser [Stmt]
pProg = between spaceConsumer eof (many pStmt)

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer
