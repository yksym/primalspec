{
module PrimalSpec.Parser
( prspParser
, Program(..)
)
where

import PrimalSpec.Lexer -- (AlexPosn, Token(..), TokenClass(..), lexer, tokenToPosN)

}

%name prspParser
%tokentype { Token }

%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token _ TokenEOF }

%token
        '%header'       { Token _ TokenHeader   }
        '%footer'       { Token _ TokenFooter   }
        '%entry'        { Token _ TokenEntry    }
        '%state'        { Token _ TokenStateType}
        '%initstate'    { Token _ TokenInitState}
        '%event'        { Token _ TokenEventType}
        '%%'            { Token _ TokenDirectiveSplitter }
        ','             { Token _ TokenProcArgSplitter }
        '.'             { Token _ TokenEventArgSplitter }
        '_'             { Token _ TokenPlaceholder }
        '?'             { Token _ TokenForallSplitter   }
        STOP            { Token _ TokenStop     }
        SKIP            { Token _ TokenSkip     }
        Load            { Token _ TokenLoad     }
        Store           { Token _ TokenStore    }
        '@'             { Token _ TokenSuchThat }
        '&'             { Token _ TokenGuard    }
        '[]'            { Token _ TokenEC       }
        '/\\'           { Token _ TokenIntr     }
        '->'            { Token _ TokenPrefix    }
        ':='            { Token _ TokenEq       }
        '[T='           { Token _ TokenRefT     }
        '('             { Token _ TokenOB       }
        ')'             { Token _ TokenCB       }
        int             { Token _ (TokenInt $$) }
        id              { Token _ (TokenId $$)  }
        texp            { Token _ (TokenTargetExp $$)}

%nonassoc ':=' '[T='
%left '[]' '/\\'
%right '->'

%%

Program :: {Program}
    : Header EventTypePragma StateTypePragma InitStatePragma EntryPragma '%%' StmtList '%%' Footer { Program $1 $2 $3 $4 $5 $7 $9 }

Header :: {String}
    : '%header' texp { $2 }

Footer :: {String}
    : '%footer' texp { $2 }

EntryPragma :: { Proc }
    : '%entry' Proc { $2 }

StateTypePragma :: {String}
    : '%state' texp { $2 }

InitStatePragma :: {String}
    : '%initstate' texp { $2 }

EventTypePragma :: {String}
    : '%event' texp { $2 }

StmtList :: { [Stmt] }
    : Stmt           { [$1] }
    | Stmt StmtList  { $1 : $2 }


Stmt :: { Stmt }
    : Proc ':=' Exp { ProcDef $1 $3 }
    | Proc '[T=' Exp { RefTDef $1 $3 }

Exp :: { Exp }
     : STOP { Stop }
     | SKIP { Skip }
     | texp '&' Exp { Guard $1 $3 }
     | Event '->' Exp { Prefix $1 $3 }
     | Exp '[]' Exp { ExternalChoise $1 $3 }
     | Exp '/\\' Exp { Interrupt $1 $3 }
     | '(' Exp ')' { $2 }
     | Proc { ProcRef $1 }


Proc :: { Proc }
Proc : id               { Proc $1 [] }
     | id '(' ProcArgs ')' { Proc $1 $3 }

ProcArgs :: { [ProcArg] }
ProcArgs : ProcArg          { [$1] }
         | ProcArg ',' ProcArgs { $1 : $3 }

ProcArg :: {ProcArg}
ProcArg  : id            { PAId  $1 }
         | int           { PAInt $1 }
         | texp          { PATExp $1 }
         | '_'           { PAPH }

Event :: { Event }
Event : id                         { Event $1 [] Nothing }
      | id EventArgs           { Event $1 $2 Nothing }
      | id EventArgs '@' texp  { Event $1 $2 (Just $4) }
      | Load '?' id                { Load $3     }
      | Store '.' texp                 { Store $3    }

EventArgs :: {[EventArg]}
EventArgs : EventArg                { [$1]    }
          | EventArg EventArgs  { $1 : $2 }

EventArg :: {EventArg}
EventArg : '.' id            { EAId  $2 }
         | '.' int           { EAInt $2 }
         | '.' texp          { EATExp $2 } 
         | '?' id            { EAVar $2 }
         | '?' '_'           { EAPH }

{

type Condition = String

data Program = Program {
    _header    :: String
  , _eventType :: String
  , _stateType :: String
  , _initState :: String
  , _entryProc :: Proc
  , _stmts     :: [Stmt]
  , _footer    :: String
  }
    deriving (Show, Eq)

data Stmt
    = ProcDef Proc Exp
    | RefTDef Proc Exp
    deriving (Show, Eq)

data Exp
    = Stop
    | Skip
    | Guard Condition Exp
    | Prefix Event Exp
    | ExternalChoise Exp Exp
    | Interrupt Exp Exp
    | ProcRef Proc
    deriving (Show, Eq)

data Proc = Proc String [ProcArg]
    deriving (Show, Eq)

data ProcArg
    = PAId  String
    | PAInt Int
    | PAVar String
    | PATExp String
    | PAPH
    deriving (Show, Eq)

data Event
    = Event String [EventArg] (Maybe Condition)
    | Load  String
    | Store String
    deriving (Show, Eq)

data EventArg
    = EAId  String
    | EAInt Int
    | EAVar String
    | EATExp String
    | EAPH
    deriving (Show, Eq)

}
