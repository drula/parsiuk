{
-- | Synt module containing utilities for syntax analysis. The code is
-- generated by Happy.
module Synt where
import Lex
}

%name toPTree
%tokentype { Token }
%monad { Either String } { (>>=) } { return }

%token
    ident   { TIdent $$ }
    struct  { TStruct }
    "{"     { TLeftCrBrace }
    "}"     { TRightCrBrace }
    eof     { TEOF }

%%

Tree : Struct eof { PTree $1 }

Struct : struct ident "{" "}" { PStruct $2 }

{
-- | Parsing error function.
happyError :: [Token] -> Either String a
happyError tokens = Left $ "Syntax error: " ++ show tokens
-- TODO: show human readable message

-- | Parsiuk AST. The type is not yet fully implemented.
data PTree = PTree PStruct -- ^ Parsiuk Tree constructor taking a main
                           -- structure description
    deriving (Eq, Show)
-- TODO: implement

-- | Structure (a list of fields) with its name.
-- The type is not yet fully implemented.
data PStruct = PStruct String -- ^ A structure with its name
    deriving (Eq, Show)
-- TODO: implement
}
