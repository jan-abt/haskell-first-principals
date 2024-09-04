{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}

module
    DOTParser
        where

import Text.Trifecta
import Text.Regex
import Control.Applicative
import Text.RawString.QQ ( r )
import Data.Maybe (isJust,fromJust)
import Control.Monad

-- DOT Language 
-- website: https://graphviz.org/doc/info/lang.html 

-- ================================================================================================== --
-- ======================================  DATA TYPES   ============================================= --
-- ================================================================================================== --

type Id = String

type LeftSide = String

type RightSide = String

type Key = String

type Value = String

type Edgeop = String

type Strict = Bool

-- graph ::= [ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
data Graph =
  UndirectedGraph Strict (Maybe Id) StatementList |
  DirectedGraph Strict (Maybe Id) StatementList
  deriving (Show)

-- subgraph ::= [ subgraph [ ID ] ] '{' stmt_list '}'
data SubGraphStatement =
  SubGraph (Maybe Id) StatementList
  deriving (Show)

-- stmt_list ::= [ stmt [ ';' ] stmt_list ]
newtype StatementList = StatementList [Statement] deriving (Show)

-- stmt ::= node_stmt | edge_stmt | attr_stmt | asgmt_stmt | subgraph    
data Statement =
   EdgeStatement EdgeStatement  |
   NodeStatement NodeStatement |
   SubGraphStatement  SubGraphStatement  |
   AttributeStatement AttributeStatement  |
   KeyValueStatement KeyValueStatement
  deriving (Show)

-- attr_list ::= '[' [ a_list ] ']' [ attr_list ]
-- a_list ::= ID '=' ID  [ (';' | ',') ] [ a_list ]    
data KeyValueStatement =
  KV Key Value
  deriving (Show)

-- attr_stmt ::= (graph | node | edge) attr_list
data AttributeStatement =
  GraphAttributes [KeyValueStatement] |
  NodeAttributes [KeyValueStatement] |
  EdgeAttributes [KeyValueStatement]
  deriving (Show)

-- node_stmt ::= node_id [ attr_list ]    
data NodeStatement = -- 
  NodeId NodeId (Maybe [KeyValueStatement])
  deriving (Show)

-- node_id ::= ID [ port ]
-- Id "nodeIdent" (Just (TaggedPort "portIdent" (Just NE)))
data NodeId =
  Id String (Maybe Port)
  deriving (Show)

-- port ::= ':' ID [ ':' compass_pt ] | ':' compass_pt
-- TaggedPort "portIdent" (Just NE)
-- UntaggedPort NE
data Port =
  TaggedPort String (Maybe CompassPoint) |
  UntaggedPort CompassPoint
  deriving (Show)

-- compass_pt ::= (n | ne | e | se | s | sw | w | nw | c | _)    
data CompassPoint =
  N | NE | E | SE | S | SW |W | NW | C | UNDIRECTED
  deriving (Show)

-- edge_stmt ::= (node_id | subgraph) edgeRHS [ attr_list ]
data EdgeStatement =
  NodeIdEdge NodeId [EdgeRHS] (Maybe [KeyValueStatement]) |
  SubGraphEdge SubGraphStatement [EdgeRHS] (Maybe [KeyValueStatement])
 deriving (Show)

-- edgeRHS ::= edgeop (node_id | subgraph) [ edgeRHS ]
data EdgeRHS =
  NodeEdgeRHS Operand NodeId |
  SubGraphEdgeRHS Operand SubGraphStatement
  deriving (Show)

-- edgeop ::= -> | --
data Operand =
  Unidirectional | Bidirectional
  deriving (Show)

-- ================================================================================================== --
-- ========================================== UTILITIES ============================================= --
-- ================================================================================================== --

-- Collect each item of type "[a]" in the manner specified in the "item" parser, until the "lastItem" parser succeeds.  
-- Includes the parse result of the first parsed element and drops the parse result of the "lastItem" parsed.
-- "AbcdZ" -> "Abcd"
collectUntil:: Parser a ->  Parser a -> Bool -> Parser [a]
collectUntil item lastItem skipSpaces =
  try
      -- Whenver the parser succeeds in parsing the stop-signal "lastItem", the end of input reached.
      -- Complete the list (monad) construction by "consing" an empty [] at its head.
      -- Effectively, we are swapping the stop-signal monad's contents with an empty list and 
      -- thus dropping the last parse result.
     ([] <$ lastItem)
    <|>
     if skipSpaces
     -- cons "(:)" the character parsed in the mannter of the "item" parser to the list monad.      
     -- "spaces" uses "isSpace", a prelude function, which skips formatting characters such as \n, \t or " ".     
     then (:) <$> ( spaces >> item <* spaces ) <*> collectUntil item lastItem skipSpaces
     else (:) <$> item <*>  collectUntil item lastItem skipSpaces
    <|>
      unexpected "unexpected character encountered"

-- Collect each item of type "a" in the manner specified in the "item" parser, until the "lastItem" parser succeeds.  
-- Includes both, the parse result of the first parsed element and the parse result of the "lastItem" parsed.
-- "AbcdZ" -> "AbcdZ"
collectThrough :: Parser a -> Parser a -> Bool  -> Parser [a]
collectThrough item lastItem skipSpaces =
  try
      -- Whenver the parser succeeds in parsing the stop-signal "lastItem", the end of input reached.
      -- Complete the list (monad) construction by "consing" the "lastItem" to an empty [].
      -- Here we are including "lastItem" in the result list monad.
     ((:[]) <$> lastItem )
    <|>
    if skipSpaces
     then (:) <$> ( spaces >> item <* spaces ) <*> collectThrough item lastItem skipSpaces
     else (:) <$> item <*>  collectThrough item lastItem skipSpaces
    <|>
      error "unexpected character encountered"

-- Collect each item of type "[a]" in the manner specified in the "item" parser, until the "lastItem" parser succeeds.
-- Note that we are dropping the parse result of the first parsed element, as well as the parse result of the "lastItem" parsed.
-- Also indicate whether you want to skip space/format characters for your result.  
-- "AbcdZ" -> "bcd"
collectInner::  Parser [a] -> Parser a -> Parser [a] -> Bool -> Parser [a]
collectInner leftItem it rightItem skipSpaces =
  leftItem >> processNext it rightItem
    where
      processNext item lastItem =
        try
          -- Whenver the parser succeeds in parsing the stop-signal "lastItem", the end of input reached.
          -- Complete the list (monad) construction by "consing" an empty [] at its head.
          -- Effectively, we are swapping the stop-signal monad's contents with an empty list and 
          -- thus dropping the last parse result.
          ([] <$ lastItem)
          <|>
          if skipSpaces
          -- cons "(:)" the character parsed in the mannter of "item" to the list monad.      
          -- "spaces" uses "isSpace", a prelude function, which skips formatting characters such as \n, \t or " ".     
          then (:) <$> ( spaces >> item <* spaces ) <*> processNext item lastItem
          else (:) <$> item <*> processNext item lastItem
          <|>
            unexpected "unexpected character encountered"


-- uses an alternative "|" of either one or two capturing groups e.g.,the round-braketed content.
-- The first alternative has only one group, which is for capturing quoted content and it admits any character.
-- The other alternative has two grouos, only match on letters and alphanumeric characters.
-- The anchor "^" preceeding ([a-zA-Z]+), indicates that matching characters must start with one or more letters, 
-- with an underscore optionally preceeding the characters.
-- The result of this epxression is a list of either one or two intermediary strings. 
-- Each contains the characters that were "captured" by their respective regular expression. 
-- The string list is then concatinated, or flattened into a single string and wrapped in a Maybe that can fail.
matchString :: String -> Maybe String
matchString str =
  let xs = matchRegex (mkRegex "(\".*\")|^([_]?[a-zA-Z_]+)([0-9a-zA-Z_]*)") str
  in case xs of
      Nothing -> fail str
      Just x  -> Just (concat x)


-- ================================================================================================== --
-- ========================================= PARSER LOGIC =========================================== --
-- ================================================================================================== --

-- a=b | c=\"d e f\"
parseKeyValueStatement :: Parser String -> Parser KeyValueStatement
parseKeyValueStatement stopSignal = do

    keySideRaw      <- concat <$> collectUntil admissableChars (spaces *> string "=") True
    maybeValueSide   <- spaces >> try ( quotedContent <|> nonQuotedContent)

    case matchString keySideRaw of
      Nothing -> fail $ fail $ "couldn't parse left hand side epression "++keySideRaw++"= ..."
      Just keySide ->
        case maybeValueSide of
          Nothing ->  fail $ "couldn't parse  expression on the right side of "++keySideRaw++"= ???"
          Just valueSide -> pure $ KV keySide valueSide
    where
      quotedContent =  Just <$> collectInner (string "\"") (noneOf "\"") (string "\"" *> stopSignal) False

      nonQuotedContent = matchString . concat <$> collectUntil admissableChars (spaces *> stopSignal) True
      admissableChars =  pure <$> (alphaNum <|> char '_')

-- [a=b;c=d; ...] 
parseAttributes :: Parser (Maybe [KeyValueStatement])
parseAttributes = do
    _    <- optional spaces >> string "["
    try
      (Just <$> (optional spaces <* string "]" >> pure []))
      <|>
      (Just <$> collectThrough
          (parseKeyValueStatement (optional spaces >> (string ";" <|> string ",") <* optional spaces))
          (parseKeyValueStatement (optional spaces >> string "]" ))
          True)
      <|>
        pure Nothing

-- graph [a=b;c=d]  | node [e=f]  | edge [...
parseAttributeStatement ::  Parser String ->   Parser AttributeStatement
parseAttributeStatement  stopSignal =
        (spaces >> string "graph " >> spaces >> GraphAttributes . fromJust <$> parseAttributes) <* stopSignal
       <|>
        (spaces >> string "node " >> NodeAttributes . fromJust <$> parseAttributes) <* stopSignal
       <|>
        (spaces >> string "edge " >> EdgeAttributes . fromJust <$> parseAttributes) <* stopSignal

-- subgraph aSubgraphId { ... }
parseSubGraphStatement ::  Parser String -> Parser SubGraphStatement
parseSubGraphStatement stopSignal =
  try (
    do
      maybeSubgraph <-  spaces >> optional (string "subgraph")
      maybeId       <-
        case maybeSubgraph of
          Just _ ->  try $ matchString . concat <$> collectUntil admissableChars bracketOpen True
          Nothing -> try $ Nothing <$ bracketOpen
      statementList <- parseStatementList bracketClose <* stopSignal

      pure $ SubGraph  maybeId statementList)
      where
        admissableChars = pure <$> (alphaNum <|> char '_')
        bracketOpen = spaces >>  string "{"
        bracketClose = spaces >>  string "}" <* spaces

-- node_stmt ::= node_id [ attr_list ]    
parseNodeStatement :: Parser String -> Parser NodeStatement
parseNodeStatement  stopSignal = do
    nodeId           <- try (parseNodeId <* notFollowedBy (string "->" <|> string "--"))
    nodeAttributes   <- spaces >> (join <$> optional parseAttributes)
    _                <- stopSignal
    pure $ NodeId nodeId nodeAttributes

-- aNodeId:aPortId:sw
parseNodeId ::  Parser NodeId
parseNodeId  = do
    --_ <- optional stopSignal 
    idSegments <- spaces >> sepBy (some (alphaNum <|> symbolic '_')) (symbolic ':') <* optional spaces
    case length idSegments of
      1 -> pure $ Id (head idSegments) Nothing
      2 -> case matchCompassPoint (last idSegments) of
            Just compassPoint -> pure $ Id (head idSegments) (Just (UntaggedPort compassPoint ))
            _                 -> pure $ Id (head idSegments) (Just (TaggedPort (last idSegments) Nothing ))
      _ -> pure $ Id (head idSegments) (Just (TaggedPort (head $ tail idSegments) (matchCompassPoint (last idSegments))))
    where
      matchCompassPoint :: String -> Maybe CompassPoint
      matchCompassPoint xs =
        case xs of
          "n"  -> Just N
          "ne" -> Just NE
          "e"  -> Just E
          "se" -> Just SE
          "s"  -> Just S
          "sw" -> Just SW
          "w"  -> Just W
          "nw" -> Just NW
          "c"  -> Just C
          "_"  -> Just UNDIRECTED
          _    -> Nothing

-- edge_stmt ::= (node_id | subgraph) edgeRHS [ attr_list ]
parseEdgeStatement:: Parser String -> Parser EdgeStatement
parseEdgeStatement stopSignal =    
     (SubGraphEdge <$> parseSubGraphStatement (pure "") <*> parseEdgeRHS <*> (join <$> optional parseAttributes)) <*  stopSignal
    <|> 
     try ( -- if it fails, reset the cursor so that the calling function can check further alternatives
        do
          nid <- parseNodeId 
          rhs <-  parseEdgeRHS
          if null rhs
          then fail "no right hand side, this isn't an edge statement"
          else (NodeIdEdge  nid  rhs <$> (join <$> optional parseAttributes))  <*  stopSignal
     )

-- edgeRHS ::= edgeop (node_id | subgraph) [ edgeRHS ]
parseEdgeRHS:: Parser [EdgeRHS]
parseEdgeRHS = collect []
  where
    collect :: [EdgeRHS] -> Parser [EdgeRHS]
    collect xs = do
      operand <- 
              optional ( 
                try 
                  (string "->" <* spaces >> pure Unidirectional)
                 <|>
                  (string "--" <* spaces >> pure Bidirectional)
              )
              <|>
              pure Nothing
      case operand of
        Just op ->
          ((:) <$> try (SubGraphEdgeRHS op <$> parseSubGraphStatement (pure "") ) <*> collect xs)
          <|>
          ((:) <$> try (NodeEdgeRHS op <$> parseNodeId ) <*> collect xs)
        _ ->
          pure xs

-- parseString parseEdgeStatement mempty "a1:p1:nw -> a2:p:2se"
-- a=b | graph [a=b;c=d] 
parseStatement :: Parser String -> Parser Statement
parseStatement stopSignal =
  spaces >> (
    try
      (KeyValueStatement <$> parseKeyValueStatement stopSignal)
     <|>
      (AttributeStatement <$> parseAttributeStatement stopSignal)
     <|>
      (SubGraphStatement <$> parseSubGraphStatement stopSignal)
     <|>
      (EdgeStatement <$> parseEdgeStatement stopSignal)
     <|>
      (NodeStatement <$> parseNodeStatement stopSignal)
     <|> 
      fail "not a valid statement"
  )

-- a=b; graph [a=b;c=d]; node [e=f]; g=h; ... 
parseStatementList :: Parser String ->  Parser StatementList
parseStatementList stopSignal =
    StatementList <$>
      collectThrough
        (parseStatement (optional spaces >> (string ";" <|> string "," <|> string " " ) <* optional spaces))
        (parseStatement stopSignal)
        True


-- graph : [ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
parseGraph :: Parser Graph
-- ------------------------------- --  
-- --- parser main entry point --- --
-- ------------------------------- --
parseGraph = do
  isStrict      <-  isJust <$> (spaces >> optional (string "strict"))
  isDirected    <-  spaces >> 
                      try 
                        (string "digraph" >> pure True) 
                      <|> 
                        (string "graph" >> pure False)
  graphId       <-  matchString . concat <$> collectUntil admissableChars bracketOpen True
  statementList <-  parseStatementList bracketClose
  if isDirected
  then pure $ DirectedGraph isStrict graphId statementList
  else pure $ UndirectedGraph isStrict graphId statementList
  
  where
    admissableChars = pure <$> (alphaNum <|> char '_')
    bracketOpen     = spaces >>  string "{"
    bracketClose    = spaces >>  string "}" <* spaces
  
-- ================================================================================================== --
-- =========================================== PLAYGROUND =========================================== --
-- ================================================================================================== --

sampleStatements :: IO ()
sampleStatements = do

    let _eof = "" <$ notFollowedBy anyChar
    let _anyStr = (:[]) <$> anyChar
    let _ans = (:[]) <$> (alphaNum <|> space)

    print $ parseString (parseStatementList _eof) mempty "graph [e=f,a=a],a=b"
    print $ parseString parseGraph  mempty "strict graph ja4{ node [c=s,a=l],graph [e=f]}"
    -- =============== TEST ERRORS ================== --
    putStrLn $ concat (replicate 38 "--")
    putStrLn $ " --" ++ concat (replicate 8 " Error --")
    putStrLn $ concat (replicate 38 "--")

    print $ parseString ( collectInner _anyStr  alphaNum  _eof True)  mempty "123$ tom"
    print $ parseString (parseKeyValueStatement _eof) mempty "0 tom=Jones1934"

    -- =============== TEST SUCCESSES ================== --
    putStrLn $ concat (replicate 46 "--")
    putStrLn $ " --" ++ concat (replicate 8 " Success --")
    putStrLn $ concat (replicate 46 "--")

    print $ parseString ( collectInner (string "|") anyChar (string "|") True) mempty "| tom|"
    print $ parseString ( collectInner _anyStr  anyChar _eof True) mempty "\n\t12 3  J   a    n\nJones "
    print $ parseString ( collectInner (  string "\"") anyChar (string "\"") False) mempty "\"\n\t12 3J a n\nJones\""
    print $ parseString ( collectInner _anyStr  (try alphaNum <|> space)  (string "=") False)  mempty "J an 1 971   ="
    print $ parseString ( collectInner _anyStr  (try alphaNum <|> space)  (string "=") True)  mempty "J an 1 971   ="

    print $ parseString (collectThrough ( alphaNum <|> space) ( char '=' ) False)  mempty "J an 1 971   ="

    print $ parseString ( concat <$>collectUntil _ans (string "=") False)  mempty "J an 1 971   ="


    print $ parseString (parseKeyValueStatement _eof ) mempty " tom1971=_Jones1934"

    print $ parseString (parseStatementList _eof) mempty "a=b"
    print $ parseString (parseStatementList _eof) mempty "a=b;c=d"
    print $ parseString (parseStatementList _eof) mempty "graph [a=b]"
    print $ parseString (parseStatementList _eof) mempty "node [a=b]; graph [comment = \" james was born in 1934\" ;more=afterwards\n]"
    print $ parseString (parseStatementList _eof) mempty "more=afterwards;node [a=b]; graph [comment = \" james was born in 1934\"\n]"
    print $ parseString (parseStatementList _eof) mempty "a=b;node [c=d];graph [e=f]; g=h "
    print $ parseString (parseStatementList _eof) mempty "a=b;node [c=d];e=f;graph [g=h]; i=j ;k=l "

    print $ parseString (parseStatementList _eof) mempty "\nstyle =\"border:blue;45px\";\n  node [ label=\"My uncle !!!\",\n\t\t\trank=\"A15\", \n\t\t\tcolor = green,\n\t\t\tstyle =\"border:blie;12px\"\n\t ];\n\t  tom=hi;\n\t  graph [ label=\"Sample Graph\",\n\t\t\trankdir=TB, \n\t\t\tfontcolor = blue,\n\t\t\tfontsize =\"7500pt\"]"

    print $ parseString parseGraph mempty "graph {graph [ label=\"My Graph\", fontcolor = blue,fontsize = \"25pt\", color=red]}"

    print $ parseString parseGraph mempty "strict graph j12 {\n node \n[\nmany=\"tab and next line symbols\"\n]\n }\n"
    print $ parseString parseGraph mempty "strict graph  {\n node [a=b] }\n"
    print $ parseString parseGraph  mempty "strict graph ja4{ a=b;node [d=r];b=d;graph [qqqqq=\"fdfsdfdsf  sdfdf\"]}"

    print $ parseString parseGraph mempty "strict graph  tom12 {  tom=\"ww\"; node [ label=\"My uncle !!!\",rank= tom15, color = green,style =\"border:black;solid;12px\"];  tom=hi;graph [ label=\"Sample Graph\",rankdir=TB, fontcolor = blue,fontsize =\"25pt\"\n]\n}\n"

    print $ parseString parseGraph mempty
     "strict graph  tom12 {\n   tom=\"ww\";\n  node [ label=\"My uncle !!!\",\n\t\t\trank=A15, \n\t\t\tcolor = green,\n\t\t\tstyle =\"border:orange;dotted;12px\"\n\t ];\n\t  tom=hi;\n\t  graph [ label=\"Sample Graph\",\n\t\t\trankdir=TB, \n\t\t\tfontcolor = blue,\n\t\t\tfontsize =\"75pt\"]\n}\n\n"

    print $ parseString parseGraph mempty "strict graph G1{ nodeIdent1:portIdent1:ne [label=\"Node Identifier 1\" ; color=\"green\"] }"
    print $ parseString parseGraph mempty "strict graph G2{ nodeIdent2:portIdent2:ne }"
    print $ parseString parseGraph mempty "strict graph G3{ nodeIdent3:portIdent3 [label=\"nodeIdent3\" ; color=\"green\"] }"
    print $ parseString parseGraph mempty "strict graph G4{ nodeIdent4:portIdent4 }"
    print $ parseString parseGraph mempty "strict graph G5{ nodeIdent5:ne [label=\"nodeIdent5\" ; color=\"green\"] }"
    print $ parseString parseGraph mempty "strict graph G6{ nodeIdent6:ne }"
    print $ parseString parseGraph mempty "strict graph G7{ nodeIdent7 [label=\"nodeIdent7\" ; color=\"green\"] }"
    print $ parseString parseGraph mempty "strict graph G8{ nodeIdent8; a=b }"
    print $ parseString parseGraph mempty "strict graph G9 {abs:mid:ne [why=not];lot:of:nw [say=yes]}"
    
    print $ parseString parseEdgeRHS mempty "-> nodeId1:portId1:sw"
    print $ parseString parseEdgeRHS mempty "-> nodeId1:portId1:sw -> nodeIdent2:portIdent2:ne"
    print $ parseString parseEdgeRHS mempty "-> subgraph SBT1 { kT1=vT1;node [nkT1=nvT1];asT1=asT1} -> subgraph SBT2 { kT2=vT2;node [nkT2=nvT2];askT2=asvT2}"

    print $ parseString parseEdgeRHS mempty "-> nodeIdent1:portIdent1 -> subgraph S1 {attribute1=value1}"
    print $ parseString parseEdgeRHS mempty "-> subgraph S1 {attribute1=value1} "
    print $ parseString parseEdgeRHS mempty "-> subgraph SBT1 {a1=b1} -> n1:p1:se -> subgraph SBT2 {a2=b2} -> n2:p2:se ->  subgraph SBT3 {a3=b3}"

    print $ parseString (parseEdgeStatement (pure "")) mempty " a1:p1:nw -> a2:p2:nw "
    print $ parseString (parseEdgeStatement (pure "")) mempty " a1:p1:nw -> subgraph SG1 {a=b} "
    print $ parseString (parseEdgeStatement (pure "")) mempty " subgraph SG1 {a=b} -> a1:p1:nw "
    print $ parseString (parseEdgeStatement (pure "")) mempty " a1:p1:nw -> subgraph SG1 {a=b} [ tom= jones]"
    print $ parseString (parseEdgeStatement (pure "")) mempty " subgraph SG1 {a=b} -> a1:p1:nw [ tom= jones]"
    print $ parseString (parseEdgeStatement (pure "")) mempty " subgraph cluster_0 { a=b } ->  w:b0  [label=\"always keep the edge !!!\" ; color=blue]  "
    print $ parseString (parseEdgeStatement (pure "")) mempty " a1:p1:nw -> subgraph SG1 {a=b} -> subgraph SG2 {a2=b2} [jelly=beans]"
    print $ parseString (parseEdgeStatement (pure "")) mempty " a1:p1:nw -> subgraph SG1 {a=b} -> subgraph SG2 {a2=b2} -> a2:p2:se [bird=house]"
    print $ parseString (parseEdgeStatement (pure "")) mempty " subgraph cluster { a=b } ->  w:b0  [label=\"edge bla\" ; color=\"blue\"] "

sampleDOTFile1 :: String
sampleDOTFile1 = [r|
  strict digraph G  {
   node[cccccc=dddddd];
     nodeIdent0[label="node identifier #0" ; color="blue"];
     nodeIdent1:  portIdent1 -> b1 -> fb;
     nodeIdent2:  portIdent2:ne [label=" node identifier #2 " ; color="green"];  
    a=b;
    graph[c=d];
    node[c=d];
    edge [c=d];
    subgraph cluster_0 {
       nodeIdent3:  portIdent3:ne [label=" node identifier #3"; color="green"] ;  
      style=filled;
      color=lightgrey;
      node [style="a.filled",color=white];
      a0 -> a1 -> a2 -> a3;
      label = "process #1"
    } ->  b0 -> b1 -> b2 -> b3  [label="edge bla" ; color="blue"]  ;

    {
      node [style=filled];
      b0 -> b1 -> b2 -> b3  [label="edge bla" ; color="blue"]  ;
      label = "process #2";
      color=green
    };

    start -> a0;
    start -> b0;
    a0 -> b0:sw [label="edge bla" ; color="blue"]  ;
    b2 -> a3;
    a3 -> a0;
    a3 -> end;
    b3 -> end;

    start [shape=Mdiamond];
    end [shape=Msquare]
   
  }
 
|]

sampleDOTFile2 :: String
sampleDOTFile2 = [r|
  digraph {
    a -> b[label="0.2",weight="0.2"];
    a -> c[label="0.4",weight="0.4"];
    c -> b[label="0.6",weight="0.6"];
    c -> e[label="0.6",weight="0.6"];
    e -> e[label="0.1",weight="0.1"];
    e -> b[label="0.7",weight="0.7"]
}
|]

sampleDOTFile3 :: String
sampleDOTFile3 = [r|
  digraph {
    subgraph cluster_0 {
        label="Subgraph A";
        a -> b;
        b -> c;
        c -> d
    };

    subgraph cluster_1 {
        label="Subgraph B";
        a -> f;
        f -> c
    }
}
|]

sampleDOTFile4 :: String
sampleDOTFile4 = [r|
  graph {
    splines=line;
    subgraph cluster_0 {
        label="Subgraph A";
        a; b; 
        c
    };

    subgraph cluster_1 {
        label="Subgraph B";
        d; 
        e
    };

    a -- e;
    a -- d;
    b -- d;
    b -- e;
    c -- d;
    c -- e
}
      
|]

sampleDOTFile5 :: String
sampleDOTFile5 = [r|
 graph {
    rankdir=LR; 
    a -- { b; c; d };
    b -- { c; e };
    c -- { e; f };
    d -- { f; g };
    e -- h;
    f -- { h; i; j; g };
    g -- k;
    h -- { o; l };
    i -- { l; m; j };
    j -- { m; n; k };
    k -- { n; r };
    l -- { o; m };
    m -- { o; p; n };
    n -- { q; r };
    o -- { s; p };
    p -- { s; t; q };
    q -- { t; r };
    r -- t;
    s -- z;
    t -- z
}
    
 

|]

main:: IO ()
main = do

      
      print $ parseString parseGraph mempty "graph{ subgraph cluster_0 { nodeIdent3:portIdent3:ne [a=b] ; style=filled;color=lightgrey;node [style=\"a.filled\", color=white]; a0 -> a1 -> a2 -> a3;label = \"process #1\"} ->  b0 -> b1 -> b2 -> b3  [label=\"edge bla\" ; color=\"blue\"]}}"
      print $ parseString parseGraph mempty sampleDOTFile1
      print $ parseString parseGraph mempty sampleDOTFile2
      print $ parseString parseGraph mempty sampleDOTFile3
      print $ parseString parseGraph mempty sampleDOTFile4
      print $ parseString parseGraph mempty sampleDOTFile5
      sampleStatements