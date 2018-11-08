{
module PrimalSpec.Parser
( prspParser
, Expr(..)
, Stmt(..)
)
where

import PrimalSpec.Lexer -- (AlexPosn, Token(..), TokenClass(..), lexer, tokenToLoc)
import PrimalSpec.Type

}

%name prspParser
%tokentype { Token }

%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token _ TokenEOF }

%token
  assert                                { Token _    TokenAssert      }
  global                                { Token _    TokenGlobal      }
  channel                               { Token _    TokenChannel     }
  datatype                              { Token _    TokenDataType    }
  recordtype                            { Token _    TokenRecordType  }
  nametype                              { Token _    TokenNameType    }
  true                                  { Token _    TokenTrue        }
  false                                 { Token _    TokenFalse       }
  if                                    { Token _    TokenIf          }
  then                                  { Token _    TokenThen        }
  else                                  { Token _    TokenElse        }
  let                                   { Token _    TokenLet         }
  within                                { Token _    TokenIn          }
  not                                   { Token _    TokenNot         }
  and                                   { Token _    TokenAnd         }
  or                                    { Token _    TokenOr          }
  Int                                   { Token _    TokenIntType     }
  Bool                                  { Token _    TokenBoolType    }
  ','                                   { Token _    TokenComma       }
  '::'                                  { Token _    TokenJudge       }
  ':'                                   { Token _    TokenColon       }
  '.'                                   { Token _    TokenPeriod      }
  '_'                                   { Token _    TokenUnderbar    }
  '?'                                   { Token _    TokenQuestion    }
  '!'                                   { Token _    TokenExclam      }
  '+'                                   { Token _    TokenPlus        }
  '-'                                   { Token _    TokenMinus       }
  '*'                                   { Token _    TokenMult        }
  '/'                                   { Token _    TokenDiv         }
  '%'                                   { Token _    TokenMod         }
  '^'                                   { Token _    TokenHat         }
  '#'                                   { Token _    TokenSharp       }
  '\\'                                  { Token _    TokenBackSlash   }
  '['                                   { Token _    TokenLS          }
  ']'                                   { Token _    TokenRS          }
  '<'                                   { Token _    TokenOA          }
  '>'                                   { Token _    TokenCA          }
  '{'                                   { Token _    TokenOB          }
  '}'                                   { Token _    TokenCB          }
  '<='                                  { Token _    TokenLE          }
  '>='                                  { Token _    TokenGE          }
  '='                                   { Token _    TokenAssign      }
  '=='                                  { Token _    TokenEQ          }
  '!='                                  { Token _    TokenNE          }
  ';'                                   { Token _    TokenSequence    }
  '&'                                   { Token _    TokenGuard       }
  '@'                                   { Token _    TokenAndThen     }
  '('                                   { Token _    TokenOP          }
  ')'                                   { Token _    TokenCP          }
  '&~'                                  { Token _    TokenActionUpdate}
  '^.'                                  { Token _    TokenFieldAccess}
  '->'                                  { Token _    TokenRArrow      }
  '<-'                                  { Token _    TokenLArrow      }
  '[]'                                  { Token _    TokenEC          }
  '|~|'                                 { Token _    TokenIC          }
  '/\\'                                 { Token _    TokenInterrupt   }
  '|||'                                 { Token _    TokenInterleave  }
  '||'                                  { Token _    TokenParSplitter }
  '|'                                   { Token _    TokenBar         }
  '[|'                                  { Token _    TokenParLeft     }
  '|]'                                  { Token _    TokenParRight    }
  '[T='                                 { Token _    TokenRefT        }
  int                                   { Token _   (TokenInt     _ )   }
  Id                                    { Token _   (TokenCapId   _ )   }
  id                                    { Token _   (TokenLowId   _ )   }

%nonassoc '=' '[T='
%left ';'
%left '/\\'
%left '[]'
%right '->'
%right '&'
%left and or
%nonassoc '>=' '<=' '>' '<' '==' '!='
%left '+' '-'
%left '*' '/'
%left NEG
%left NOT

%%

Stmts :: {[Stmt]}
: Stmt                            { [$1]    }
| Stmt Stmts                      { $1 : $2 }

Stmt :: {Stmt}
: Id '=' Expr                           { SProcAssign (tokenToLoc $2) (tokenToString $1) [] $3 }
| Id '(' PatternsWithComma ')' '=' Expr { SProcAssign (tokenToLoc $5) (tokenToString $1) $3 $6 }
| id '=' Expr                           { SExprAssign (tokenToLoc $2) (tokenToString $1) [] $3 }
| id '(' PatternsWithComma ')' '=' Expr { SExprAssign (tokenToLoc $5) (tokenToString $1) $3 $6 }
| id '::' TypeExpr                      { STypeDecl   (tokenToLoc $2) (tokenToString $1) $3    }
| datatype Id '=' ConstrsWithBar        { SDataTypeDecl (tokenToLoc $1) (tokenToString $2) $4  }
| datatype Id '{' FieldDeclWithComma '}'{ SRecordTypeDecl (tokenToLoc $1) (tokenToString $2) $4  }
| channel id ':' TypesWithPeriod        { SEventDecl (tokenToLoc $1) (tokenToString $2) $4  }
| channel id                            { SEventDecl (tokenToLoc $1) (tokenToString $2) []  }
| assert Expr                           { SAssert (tokenToLoc $1) $2           }

FieldDeclWithComma :: { [FieldDecl] }
: id '::' TypeExpr                        { [FieldDecl (tokenToLoc $1) (tokenToString $1) $3] }
| id '::' TypeExpr ',' FieldDeclWithComma { (FieldDecl (tokenToLoc $1) (tokenToString $1) $3): $5 }

ConstrsWithBar :: {[DataConstrDecl]}
: Constr                        { [$1]    }
| Constr '|' ConstrsWithBar     { $1 : $3 }

Constr :: {DataConstrDecl}
: Id                           {DataConstrDecl (tokenToString $1) []}
| Id '.' TypesWithPeriod       {DataConstrDecl (tokenToString $1) $3}

TypeExpr :: {Type}
: Int                                  { TyInt            }
| Bool                                 { TyBool           }
| Id                                   { TyData (tokenToString $1)      }
-- | id                                  { TyVar $1         }
| '(' TypesWithComma ')' '->' TypeExpr { TyFun $2 $5      }

TypesWithComma :: {[Type]}
: TypeExpr                        { [$1]    }
| TypeExpr ',' TypesWithComma     { $1 : $3 }

TypesWithPeriod :: {[Type]}
: TypeExpr                        { [$1]    }
| TypeExpr '.' TypesWithPeriod     { $1 : $3 }

Event :: {EEvent}
: id                              { EEvent   (tokenToLoc $1) (tokenToString $1) [] }
| id Payloads                     { EEvent   (tokenToLoc $1) (tokenToString $1) $2 }

Payloads :: {[Payload]}
: Payload                         { [$1]               }
| Payload Payloads                { $1 : $2            }

Payload :: {Payload}
: '.' '(' Expr ')'                { PLExp      (tokenToLoc $1) $3      }
| '.' Literal                     { PLExp      (tokenToLoc $1) $2      }
| '?' Pattern                     { PLPat      (tokenToLoc $1) $2      }
| '?' id ':' '{' id '|' id '<-' TypeExpr ',' Expr '}' { PLElm (tokenToLoc $1) (tokenToString $2) (tokenToString $5) (tokenToString $7) $9 $11  }

Literal :: {Expr}
: int                             { EInt       (tokenToLoc $1) (tokenToInt $1)    }
| true                            { EBool      (tokenToLoc $1) True  }
| false                           { EBool      (tokenToLoc $1) False }
| global                          { EVar       (tokenToLoc $1) "global"  }
| id                              { EVar       (tokenToLoc $1) (tokenToString $1)  }
| Id                              { EId        (tokenToLoc $1) (tokenToString $1)}

Expr :: {Expr}
: Literal                         { $1                               }
| '(' Expr ')'                    { EParenthesis (tokenToLoc $1) $2    }
| '\\' PatternsWithComma '@' Expr { EAbst      (tokenToLoc $1) $2 $4 }
| Expr '(' ExprsWithComma ')'     { EApply     (tokenToLoc $2) $1 $3 }
| Expr '{' ActionsWithComma '}'   { EActionUpdate (tokenToLoc $2) $1 $3 }
| Expr '^.' Accessors             { EFieldAccess (tokenToLoc $2) $1 $3 }
| Id '(' ExprsWithComma  ')'      { EProcRef    (tokenToLoc $1) (tokenToString $1) $3 }
| Id '.' ExprsWithPeriod          { EConstr    (tokenToLoc $1) (tokenToString $1) $3 }
| Expr '*' Expr                   { ETimes     (tokenToLoc $2) $1 $3 }
| Expr '/' Expr                   { EDiv       (tokenToLoc $2) $1 $3 }
| Expr '%' Expr                   { EMod       (tokenToLoc $2) $1 $3 }
| Expr '^' Expr                   { EPower     (tokenToLoc $2) $1 $3 }
| Expr '+' Expr                   { EPlus      (tokenToLoc $2) $1 $3 }
| Expr '-' Expr                   { EMinus     (tokenToLoc $2) $1 $3 }
| Expr '<' Expr                   { ELT        (tokenToLoc $2) $1 $3 }
| Expr '>' Expr                   { EGT        (tokenToLoc $2) $1 $3 }
| Expr '<=' Expr                  { ELE        (tokenToLoc $2) $1 $3 }
| Expr '>=' Expr                  { EGE        (tokenToLoc $2) $1 $3 }
| Expr '==' Expr                  { EEQ        (tokenToLoc $2) $1 $3 }
| Expr '!=' Expr                  { ENE        (tokenToLoc $2) $1 $3 }
| Expr and Expr                   { EAnd       (tokenToLoc $2) $1 $3 }
| Expr or  Expr                   { EOr        (tokenToLoc $2) $1 $3 }
| '-' Expr %prec NEG              { EUnMinus   (tokenToLoc $1) $2    }
| not Expr %prec NOT              { EUnNot     (tokenToLoc $1) $2    }
| let Pattern '=' Expr within Expr{ ELet       (tokenToLoc $1) $2 $4 $6 }
| if Expr then Expr else Expr     { EIf        (tokenToLoc $1) $2 $4 $6 }
| Expr '[T=' Expr                 { ERefTrace  (tokenToLoc $2) $1 $3 }
| Event '->' Expr                 { EPrefix    (tokenToLoc $2) $1 $3 Nothing     }
| Event '@' '(' Expr ')' '->' Expr      { EPrefix    (tokenToLoc $6) $1 $7 (Just $4)   }
| Expr '&' Expr                   { EGuard     (tokenToLoc $2) $1 $3                 }
| Expr '[]' Expr                  { EEChoise   (tokenToLoc $2) $1 $3                 }
| Expr ';' Expr                   { ESequence  (tokenToLoc $2) $1 $3                 }
| Expr '/\\' Expr                 { EInterrupt (tokenToLoc $2) $1 $3                 }


ActionsWithComma :: {[Action]}
: Accessors '=' Expr                          {[Action (tokenToLoc $2) $1 $3]}
| Accessors '=' Expr ',' ActionsWithComma     {(Action (tokenToLoc $2) $1 $3): $5}

Accessors :: {[Accessor]}
: id                    { [Accessor (tokenToLoc $1) (tokenToString $1) ] }
| id '.' Accessors       { (Accessor (tokenToLoc $1) (tokenToString $1)) : $3 }

PatternsWithComma :: {[Pattern]}
: Pattern                      { [$1] }
| Pattern ',' PatternsWithComma{ $1 : $3}

Pattern :: {Pattern}
: int                             { PInt       (tokenToLoc $1) (tokenToInt $1)    }
| true                            { PBool      (tokenToLoc $1) True  }
| false                           { PBool      (tokenToLoc $1) False }
| '_'                             { PWildcard  (tokenToLoc $1)       }
| id                              { PVar       (tokenToLoc $1) (tokenToString $1)  }
| Id                              { PConstr    (tokenToLoc $1) (tokenToString $1) [] }
| Id '.' PatternsWithPeriod       { PConstr    (tokenToLoc $1) (tokenToString $1) $3 }
| '(' Pattern ')'                 { PParenthesis (tokenToLoc $1) $2  }

PatternsWithPeriod :: {[Pattern]}
: Pattern                        { [$1] }
| Pattern '.' PatternsWithPeriod { $1 : $3}


ExprsWithComma :: {[Expr]}
: Expr                      { [$1] }
| Expr ',' ExprsWithComma   { $1 : $3}

ExprsWithPeriod :: {[Expr]}
: Expr                      { [$1] }
| Expr '.' ExprsWithPeriod  { $1 : $3}


{

}
