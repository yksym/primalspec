{
{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module PrimalSpec.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexError
  , happyError
  , alexMonadScan
  , runAlex
  , tokenToLoc
  , alexShowErr
  , alexGetPos
  , tokenToString
  , tokenToInt
  , lexer
  , Parser
  )
where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe(fromMaybe)

}

%wrapper "monadUserState-bytestring"

tokens :-
  $white+                               ;
  "--".*                                ;
  assert                                { tok          TokenAssert      }
  channel                               { tok          TokenChannel     }
  datatype                              { tok          TokenDataType    }
  recordtype                            { tok          TokenRecordType    }
  nametype                              { tok          TokenNameType    }
  Int                                   { tok          TokenIntType     }
  Bool                                  { tok          TokenBoolType    }
  true                                  { tok          TokenTrue        }
  false                                 { tok          TokenFalse       }
  if                                    { tok          TokenIf          }
  then                                  { tok          TokenThen        }
  else                                  { tok          TokenElse        }
  let                                   { tok          TokenLet         }
  within                                { tok          TokenIn          }
  not                                   { tok          TokenNot         }
  and                                   { tok          TokenAnd         }
  or                                    { tok          TokenOr          }
  [\,]                                  { tok          TokenComma       }
  [\.]                                  { tok          TokenPeriod      }
  [\_]                                  { tok          TokenUnderbar    }
  [\?]                                  { tok          TokenQuestion    }
  [\!]                                  { tok          TokenExclam      }
  [\+]                                  { tok          TokenPlus        }
  [\-]                                  { tok          TokenMinus       }
  [\*]                                  { tok          TokenMult        }
  [\/]                                  { tok          TokenDiv         }
  [\%]                                  { tok          TokenMod         }
  [\^]                                  { tok          TokenHat         }
  [\#]                                  { tok          TokenSharp       }
  [\\]                                  { tok          TokenBackSlash   }
  [\[]                                  { tok          TokenLS          }
  [\]]                                  { tok          TokenRS          }
  [\<]                                  { tok          TokenOA          }
  [\>]                                  { tok          TokenCA          }
  [\{]                                  { tok          TokenOB          }
  [\}]                                  { tok          TokenCB          }
  [\<][=]                               { tok          TokenLE          }
  [\>][=]                               { tok          TokenGE          }
  [=]                                   { tok          TokenAssign      }
  [=][=]                                { tok          TokenEQ          }
  [!][=]                                { tok          TokenNE          }
  [\;]                                  { tok          TokenSequence    }
  [:]                                   { tok          TokenColon       }
  [:][:]                                { tok          TokenJudge       }
  [&]                                   { tok          TokenGuard       }
  [@]                                   { tok          TokenAndThen     }
  [\(]                                  { tok          TokenOP          }
  [\)]                                  { tok          TokenCP          }
  [&][\~]                               { tok          TokenActionUpdate}
  [\^][\.]                              { tok          TokenFieldAccess }
  [\-][\>]                              { tok          TokenRArrow      }
  [\<][\-]                              { tok          TokenLArrow      }
  [\[][\]]                              { tok          TokenEC          }
  [\|][\~][\|]                          { tok          TokenIC          }
  [\/][\\]                              { tok          TokenInterrupt   }
  [\|][\|][\|]                          { tok          TokenInterleave  }
  [\|][\|]                              { tok          TokenParSplitter }
  [\|]                                  { tok          TokenBar         }
  [\[][\|]                              { tok          TokenParLeft     }
  [\|][\]]                              { tok          TokenParRight    }
  [\[] T [=]                            { tok          TokenRefT        }
  [0-9] [0-9]*                          { tok_read     TokenInt         }
  [a-z] [a-z A-Z 0-9 \_ \']*            { tok_string   TokenLowId       }
  [A-Z] [a-z A-Z 0-9 \_ \']*            { tok_string   TokenCapId       }


{
tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_read x = tok' (\s -> x (read (B.unpack s)))


data TokenClass
 = TokenAssert
 | TokenChannel
 | TokenDataType
 | TokenRecordType
 | TokenNameType
 | TokenIntType
 | TokenBoolType
 | TokenTrue
 | TokenFalse
 | TokenIf
 | TokenThen
 | TokenElse
 | TokenLet
 | TokenIn
 | TokenNot
 | TokenAnd
 | TokenOr
 | TokenComma
 | TokenPeriod
 | TokenUnderbar
 | TokenQuestion
 | TokenExclam
 | TokenPlus
 | TokenMinus
 | TokenMult
 | TokenDiv
 | TokenMod
 | TokenHat
 | TokenSharp
 | TokenBackSlash
 | TokenLS
 | TokenRS
 | TokenOA
 | TokenCA
 | TokenOB
 | TokenCB
 | TokenLE
 | TokenGE
 | TokenAssign
 | TokenEQ
 | TokenNE
 | TokenSequence
 | TokenJudge
 | TokenColon
 | TokenGuard
 | TokenAndThen
 | TokenOP
 | TokenCP
 | TokenActionUpdate
 | TokenFieldAccess
 | TokenLArrow
 | TokenRArrow
 | TokenEC
 | TokenIC
 | TokenInterrupt
 | TokenInterleave
 | TokenParSplitter
 | TokenBar
 | TokenParLeft
 | TokenParRight
 | TokenRefT
 | TokenInt Int
 | TokenCapId String
 | TokenLowId String
 | TokenEOF
 deriving (Eq, Show)

data Token = Token AlexPosn TokenClass
  deriving (Show)

type AlexUserState = ()
alexInitUserState = ()

tokenToLoc :: Token -> String
tokenToLoc (Token (AlexPn _ line col) _) = " line: " ++ show line ++ " col: " ++ show col ++ ": "

tokenToString :: Token -> String
tokenToString (Token _ (TokenCapId s)) = s
tokenToString (Token _ (TokenLowId s)) = s
tokenToString _ = error "invalid arg"

tokenToInt :: Token -> Int
tokenToInt (Token _ (TokenInt n)) = n
tokenToInt _ = error "invalid arg"


alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

alexShowErr :: (Show t, Show t1) => (t, t1, Maybe String) -> Alex a
alexShowErr (line, column, e) = alexError $ "error: line: " ++ show line ++ " column: " ++ show column ++ " " ++ fromMaybe "parse error" e

alexGetPos :: Alex AlexPosn
alexGetPos = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

lexer :: (Token -> Alex a) -> Alex a
lexer f = alexMonadScan >>= f

type Parser a = Alex a

happyError :: Alex a
happyError = do
  (AlexPn _ line col) <- alexGetPos
  alexShowErr (line, col, Nothing)


}
