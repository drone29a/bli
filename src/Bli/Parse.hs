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

pIdent :: Parser Ident
pIdent = 
  Ident . T.pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

pVar :: Parser Var
pVar = do
  (Ident ident) <- pIdent
  return $ Var ident

pOperand :: Parser PExpr
pOperand = try $ do
  choice
    [ PExprLit <$> try (pNum <|> pStr <|> pBool <|> pNil)
    , PExprGroup <$> try (between (symbol "(") (symbol ")") pExpr)
    , PExprVar <$> try pVar
    , try pAsgn
    ]

pAsgn :: Parser PExpr
pAsgn = try $ do
  -- MBR: Will need to expand to support other tokens that aren't simple variables. 
  --      e.g., myobj(1, "foo").x = 10
  var <- pVar
  void $ symbol "="
  right <- pExpr
  return . PExprAsgn $ Asgn (LValVar var) right

pCallArgs :: Parser [PExpr]
pCallArgs = try $ do
  sepBy pExpr (symbol ",")

pCall :: Parser (PExpr -> PExpr)
pCall = try $ do
  void $ symbol "("
  args <- try pCallArgs
  void $ symbol ")"
  return $ \target -> PExprCall target args 

-- | Supports construction of an AST tree by threading
-- a term through a sequence of functions which augment
-- the AST.
-- Useful for supporting syntax such as:
-- f()().x.y where a sequence of calls and field accesses
-- occur.
composeTerms :: Parser a -> Parser [a -> a] -> Parser a
composeTerms = liftM2 (foldl (flip ($)))

pExpr :: Parser PExpr
pExpr = try pAsgn <|> try (composeTerms pExpr' (many pCall))
-- pExpr = try pAsgn <|> try pExpr' 

pExpr' :: Parser PExpr
pExpr' =
  makeExprParser
    pOperand
    [
      [ prefix "-" (PExprUn . UnExpr UnNeg)
      , prefix "!" (PExprUn . UnExpr UnNot)
      ]
    ,
      [ binary "*" (\x y -> PExprBin $ BinExpr BinMul x y)
      , binary "/" (\x y -> PExprBin $ BinExpr BinDiv x y)
      ]
    ,
      [ binary "+" (\x y -> PExprBin $ BinExpr BinAdd x y)
      , binary "-" (\x y -> PExprBin $ BinExpr BinSub x y)
      ]
    ,
      [ binary "<=" (\x y -> PExprBin $ BinExpr BinLte x y)
      , binary "<" (\x y -> PExprBin $ BinExpr BinLt x y)
      , binary ">=" (\x y -> PExprBin $ BinExpr BinGte x y)
      , binary ">" (\x y -> PExprBin $ BinExpr BinGt x y)
      ]
    ,
      [ binary "==" (\x y -> PExprBin $ BinExpr BinEq x y)
      , binary "!=" (\x y -> PExprBin $ BinExpr BinNeq x y)
      ]
    ,
      [ binary "and" (\x y -> PExprBin $ BinExpr BinLogAnd x y)
      ]
    ,
      [ binary "or" (\x y -> PExprBin $ BinExpr BinLogOr x y)
      ]
    ]
  where
    binary :: Text -> (PExpr -> PExpr -> PExpr) -> Operator Parser PExpr
    binary op ctor = InfixL $ ctor <$ symbol op
    prefix :: Text -> (PExpr -> PExpr) -> Operator Parser PExpr
    prefix op ctor = Prefix $ ctor <$ symbol op

pStmt :: Parser (Stmt PExpr)
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
    , try pStmtReturn
    ]

pStmtBlock :: Parser (Stmt PExpr)
pStmtBlock =
  StmtBlock <$> try (between (symbol "{") (symbol "}") $ many pStmt)

pStmtExpr :: Parser (Stmt PExpr)
pStmtExpr = StmtExpr <$> try (pExpr <* symbol ";")

pStmtVarDecl :: Parser (Stmt PExpr)
pStmtVarDecl = do
    void $ symbol "var"
    name <- pVar
    mVal <- try . optional $ symbol "=" *> pExpr
    void $ symbol ";"
    case mVal of
      Just val -> return $ StmtVarDecl name val
      Nothing -> return $ StmtVarDecl name (PExprLit LitNil)

pStmtWhile :: Parser (Stmt PExpr)
pStmtWhile = do
  void $ symbol "while"
  void $ symbol "("
  cond <- pExpr
  void $ symbol ")"
  body <- pStmt
  return $ StmtWhile cond body

pStmtFor :: Parser (Stmt PExpr)
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
      while = StmtWhile (fromMaybe (PExprLit (LitBool True)) mCond) whileBody
  return $ StmtBlock (catMaybes [mInit, Just while])
  
pStmtIf :: Parser (Stmt PExpr)
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

pStmtFuncDecl :: Parser (Stmt PExpr)
pStmtFuncDecl = do
  void $ symbol "fun"
  name <- pVar
  void $ symbol "("
  params <- pFuncParams
  void $ symbol ")"
  block <- pStmtBlock
  return $ StmtFuncDecl name params block

pStmtReturn :: Parser (Stmt PExpr)
pStmtReturn = do
  void $ symbol "return"
  val <- pExpr
  void $ symbol ";"
  return $ StmtReturn val

pProg :: Parser [Stmt PExpr]
pProg = many pStmt

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
