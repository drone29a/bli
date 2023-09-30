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
import Bli.Error (ErrorMsg)
import Control.Monad (void, when)
import Data.Maybe (isNothing, fromMaybe, catMaybes)

type Parser = Parsec ErrorMsg Text

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

pVar :: Parser Var
pVar =
  Var . T.pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

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
  _ <- symbol "="
  right <- pExpr
  return . PExprAsgn $ Asgn (LValVar var) right

pExpr :: Parser PExpr
pExpr = try pAsgn <|> try pExpr'

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
    , try pStmtDecl
    , try pStmtWhile
    , try pStmtFor
    , StmtBlock <$> try (between (symbol "{") (symbol "}") $ many pStmt)
    , try pStmtIf
    ]

pStmtExpr :: Parser (Stmt PExpr)
pStmtExpr = StmtExpr <$> try (pExpr <* symbol ";")

pStmtDecl :: Parser (Stmt PExpr)
pStmtDecl = do
    void $ symbol "var"
    var <- pVar
    mVal <- try . optional $ symbol "=" *> pExpr
    void $ symbol ";"
    case mVal of
      Just val -> return $ StmtDecl var val
      Nothing -> return $ StmtDecl var (PExprLit LitNil)

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
  mInit <- optional $ choice [ try pStmtDecl
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

  let whileBody = case (body, mIncr) of
        (StmtBlock stmts, Just incr) -> StmtBlock $ stmts ++ [StmtExpr incr]
        _ -> body
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
