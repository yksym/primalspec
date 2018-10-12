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
  , tokenToPosN
  , alexShowErr
  , alexGetPos
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
  [\%] header                           { tok          TokenHeader }
  [\%] footer                           { tok          TokenFooter }
  [\%] entry                            { tok          TokenEntry }
  [\%] state                            { tok          TokenStateType }
  [\%] initstate                        { tok          TokenInitState }
  [\%] event                            { tok          TokenEventType }
  [\%] [\%]                             { tok          TokenDirectiveSplitter }
  [\,]                                  { tok          TokenProcArgSplitter }
  [\.]                                  { tok          TokenEventArgSplitter }
  [\_]                                  { tok          TokenPlaceholder }
  [\?]                                  { tok          TokenForallSplitter }
  STOP                                  { tok          TokenStop }
  SKIP                                  { tok          TokenSkip }
  Load                                  { tok          TokenLoad }
  Store                                 { tok          TokenStore }
  [@]                                   { tok          TokenSuchThat  }
  [&]                                   { tok          TokenGuard  }
  [\[][\]]                              { tok          TokenEC }
  [\/][\\]                              { tok          TokenIntr }
  [\-][>]                               { tok          TokenPrefix }
  [:][=]                                { tok          TokenEq }
  [\[] T [=]                            { tok          TokenRefT }
  [\(]                                  { tok          TokenOB }
  [\)]                                  { tok          TokenCB }
  [1-9] [0-9]*                          { tok_read     TokenInt }
  [a-zA-Z] [a-z A-Z 0-9 \_ \']*         { tok_string   TokenId }
  [\{]$white*[^\}]*$white*[\}]                        { tok_string   TokenTargetExp }


{

-- (memo)
-- alexMonadScan :: Alex Token
-- result == Token

tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_read x = tok' (\s -> x (read (B.unpack s)))


data TokenClass
 = TokenHeader
 | TokenFooter
 | TokenDirectiveSplitter
 | TokenProcArgSplitter
 | TokenEventArgSplitter
 | TokenPlaceholder
 | TokenForallSplitter
 | TokenEntry
 | TokenStateType
 | TokenInitState
 | TokenEventType
 | TokenStop
 | TokenSkip
 | TokenLoad
 | TokenStore
 | TokenSuchThat
 | TokenGuard
 | TokenEC
 | TokenIntr
 | TokenPrefix
 | TokenEq
 | TokenRefT
 | TokenOB
 | TokenCB
 | TokenInt    Int
 | TokenId String
 | TokenTargetExp String
 | TokenEOF
 deriving (Eq, Show)

data Token = Token AlexPosn TokenClass
  deriving (Show)

type AlexUserState = ()
alexInitUserState = ()

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

alexShowErr :: (Show t, Show t1) => (t, t1, Maybe String) -> Alex a
alexShowErr (line, column, e) = alexError $ "error: line: " ++ show line ++ " column: " ++ show column ++ " type: " ++ fromMaybe "-" e

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
