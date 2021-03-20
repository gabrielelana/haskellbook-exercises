-- Grammar: https://graphviz.org/doc/info/lang.html
--
-- graph      : [ *strict* ] (*graph* | *digraph*) [ ID ] '{' stmt_list '}'
-- stmt_list  : [ stmt [ ';' ] stmt_list ]
-- stmt	      : node_stmt
--            | edge_stmt
--            | attr_stmt
--            | ID '=' ID
--            | subgraph
-- attr_stmt  :	(*graph* | *node* | *edge*) attr_list
-- attr_list  :	'[' [ a_list ] ']' [ attr_list ]
-- a_list     :	ID '=' ID [ (';' | ',') ] [ a_list ]
-- edge_stmt  :	(node_id | subgraph) edgeRHS [ attr_list ]
-- edgeRHS    :	edgeop (node_id | subgraph) [ edgeRHS ]
-- node_stmt  :	node_id [ attr_list ]
-- node_id    :	ID [ port ]
-- port	      :	':' ID [ ':' compass_pt ]
--            |	':' compass_pt
-- subgraph   :	[ *subgraph* [ ID ] ] '{' stmt_list '}'
-- compass_pt :	(*n* | *ne* | *e* | *se* | *s* | *sw* | *w* | *nw* | *c* | *_*)

-- Examples:
-- https://graphs.grevian.org/example
-- https://renenyffenegger.ch/notes/tools/Graphviz/examples/index
-- https://github.com/awalterschulze/gographviz/tree/master/testdata

-- Render Online:
-- https://graphs.grevian.org/graph

-- Data definition from:
-- http://hackage.haskell.org/package/haphviz-0.2.0.1/docs/src/Text-Dot-Types-Internal.html

module Dot where

import Prelude hiding (lex)
import Control.Monad ( forM_, void )
import Data.Char ( toUpper, toLower )
import Data.Text (Text)
import Text.Parsec (ParseError, runParser, eof, manyTill, anyToken, satisfy, many, (<|>), try, newline, sepBy1, sepBy, option, Parsec, getState, updateState)
import Text.Parsec.Char (anyChar, space, upper, char, letter, oneOf, string, digit)
import Text.Parsec.Combinator (many1)
import Test.Hspec ( shouldSatisfy, it, describe, hspec )
import Data.Either ( isRight )
import Data.Functor ( ($>) )
import Text.Parser.Combinators ( skipOptional, between, optional, some )
import Text.Parser.Char (notChar, noneOf)

-- | A graph identifier.
--
-- This is either a user supplied name or a generated numerical identifier.
data GraphId = GraphName String
             | GraphCounter Int
             deriving (Eq, Show)

-- | Type of a graph, directed or undirected.
--
-- This also specifies what edge declarations look like.
data GraphType = UndirectedGraph
               | DirectedGraph
               deriving (Show, Eq)

-- | Attribute: a tuple of name and value.
type AttributeName = String
type AttributeValue = String
type Attribute = (AttributeName, AttributeValue)

-- | A node identifier.
--
-- This is either a user supplied name or a generated numerical identifier.
data NodeId = NodeName String
            | NodeCounter Int
            deriving (Show, Eq)

-- | Declaration type
--
-- Used to declare common attributes for nodes or edges.
data DecType = DecGraph
             | DecNode
             | DecEdge
             deriving (Show, Eq)

-- | A Haphviz Graph
data DotGraph = Graph GraphType GraphId Dot
              deriving (Show, Eq)

-- | Rankdir Type
--
-- Used to specify the default node layout direction
data RankdirType = LR
                 | RL
                 | TB
                 | BT
                 deriving (Show, Eq)

-- | Haphviz internal graph content AST
data Dot = Node NodeId [Attribute]
         | Edge NodeId NodeId [Attribute]
         | Declaration DecType [Attribute]
         | Subgraph GraphId Dot
         | Local Attribute -- Added for current graph attributes (ID = ID)
         -- | RawDot Text ???
         -- | Label Text ???
         -- | Rankdir RankdirType
         | DotSeq Dot Dot
         | DotEmpty -- Maybe don't needed?
    deriving (Show, Eq)

instance Semigroup Dot where
  DotEmpty <> d = d
  d <> DotEmpty = d
  d <> (DotSeq d1 d2) = DotSeq (d <> d1) d2
  d1 <> d2 = DotSeq d1 d2

instance Monoid Dot where
  mempty = DotEmpty

type Parser = Parsec String Int

parse :: String -> Either ParseError DotGraph
parse = parse' dot

parse' :: Parser a -> String -> Either ParseError a
parse' p = runParser (whitespaces *> p <* eof) 0 ""

whitespaces :: Parser ()
whitespaces = void $ many $ oneOf " \n\t"

ot :: Parser a -> Parser ()
ot = skipOptional . try

lex :: String -> Parser String
lex s = string s <* whitespaces

string' :: String -> Parser String
string' = traverse $ \c -> char (toLower c) <|> char (toUpper c)

keyword :: String -> Parser String
keyword s = string' s <* whitespaces

dot :: Parser DotGraph
dot = do
  ot comments
  ot $ keyword "strict"
  t <- graphType
  n <- graphId <* ot comments
  (Subgraph _ d) <- dotGraph
  return $ Graph t n d
    where graphType :: Parser GraphType
          graphType = (keyword "digraph" $> DirectedGraph) <|>
                      (keyword "graph" $> UndirectedGraph)

dotStatementList :: Parser [Dot]
dotStatementList = some $ comments *> dotStatement <* ot (lex ";") <* comments

dotStatement :: Parser Dot
dotStatement = try dotDeclaration <|>
               try dotEdge <|>
               try dotGraph <|>
               try dotLocalAttribute <|>
               dotNode

dotGraph :: Parser Dot
dotGraph = do
  ot $ keyword "subgraph"
  n <- graphId <* ot comments
  Subgraph n <$> (lex "{" *> (mconcat <$> dotStatementList) <* lex "}")

dotEdge :: Parser Dot
dotEdge = do
  t <- target <* op
  ts <- target `sepBy1` op
  attributes <- option [] dotAttributes
  let targets = t:ts
  let edges = (zip <*> tail) targets >>= edgesFrom attributes
  return $ mconcat $ targets ++ edges
    where target = dotNode <|> dotGraph
          op = (try (string "->") <|> string "--") <* whitespaces

dotDeclaration :: Parser Dot
dotDeclaration = Declaration <$> decType <*> dotAttributes <* comments
  where decType = (keyword "node" $> DecNode) <|>
                  (keyword "edge" $> DecEdge) <|>
                  (keyword "graph" $> DecGraph)

dotLocalAttribute :: Parser Dot
dotLocalAttribute = Local <$> dotAttribute

dotNode :: Parser Dot
dotNode = Node <$> nodeId <*> option [] dotAttributes

dotAttributes :: Parser [Attribute]
dotAttributes = lex "[" *> (dotAttribute `sepBy` s) <* lex "]"
  where s = (oneOf ";," <* whitespaces) <|> pure ' '

dotAttribute :: Parser Attribute
dotAttribute = (,) <$> identifier <* lex "=" <*> identifier

-- TODO: support NodeCounter nodes
nodeId :: Parser NodeId
nodeId = nodeName -- <|> nodeCounter

nodeName :: Parser NodeId
nodeName = NodeName <$> identifier <* ot port
  where port = string ":" <* identifier `sepBy` string ":"

nodeCounter :: Parser NodeId
nodeCounter = do
  i <- getState
  updateState (+1)
  return $ NodeCounter i

graphId :: Parser GraphId
graphId = graphName <|> graphCounter

graphName :: Parser GraphId
graphName = GraphName <$> identifier <* ot port
  where port = string ":" <* identifier `sepBy` string ":"

graphCounter :: Parser GraphId
graphCounter = do
  i <- getState
  updateState (+1)
  return $ GraphCounter i


identifier :: Parser String
identifier = (  alphabeticIdentifier
            <|> numericIdentifier
            <|> htmlIdentifier
            <|> quotedIdentifier
             ) <* whitespaces

-- Any string of alphabetic ([a-zA-Z\200-\377]) characters,
-- underscores ('_') or digits([0-9]), not beginning with a digit;
alphabeticIdentifier :: Parser String
alphabeticIdentifier = many1 (oneOf prefix) <> many (oneOf suffix)
  where prefix = ['a'..'z'] ++ ['A'..'Z'] ++ ['\200'..'\377'] ++ ['_']
        suffix = prefix ++ ['0'..'9']

-- A numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
numericIdentifier :: Parser String
numericIdentifier = m <> (n1 <|> n2)
  where m = option "" (string "-")
        n1 = string "." <> many1 (oneOf ['0'..'9'])
        n2 = many1 (oneOf ['0'..'9']) <> option "" (string "." <> many (oneOf ['0'..'9']))

quotedIdentifier :: Parser String
quotedIdentifier = string "\"" *> (mconcat <$> many (try (string "\\\"") <|> (pure <$> notChar '"'))) <* string "\""

htmlIdentifier :: Parser String
htmlIdentifier = string "<" <> (mconcat <$> many html) <> string ">"
  where html = some (noneOf "<>") <|> (mconcat <$> some htmlIdentifier)

comments :: Parser ()
comments = void $ many $ (try inline <|> block <|> preprocessor) <* whitespaces
  where inline = string "//" *> many (notChar '\n')
        block = string "/*" *> manyTill anyChar (try $ string "*/")
        preprocessor = string "#" *> many (notChar '\n')

nodesFrom :: Dot -> [Dot]
nodesFrom n@(Node _ _) = [n]
nodesFrom (Subgraph _ d) = nodesFrom d
nodesFrom (Edge n1 n2 _) = [Node n1 [], Node n2 []]
nodesFrom (DotSeq d1 d2) = nodesFrom d1 ++ nodesFrom d2
nodesFrom _ = []

edgesFrom :: [Attribute] -> (Dot, Dot) -> [Dot]
edgesFrom a (d1, d2) = [ Edge (nid n1) (nid n2) a
                       | n1 <- nodesFrom d1
                       , n2 <- nodesFrom d2
                       ]
  where nid (Node id _) = id

check :: IO ()
check = hspec $
  describe "Dot Language" $
  let examples = [ "cluster"
                 , "crazy"
                 , "datastruct"
                 , "er"
                 , "fdpclust"
                 , "fsm"
                 , "gd_1994_2007" -- anonymous graph
                 , "helloworld"
                 , "kennedyanc"
                 , "lion_share"
                 , "networkmap_twopi"
                 , "philo"
                 , "process"
                 , "profile"
                 , "psg"
                 , "root"
                 , "sdh"
                 , "siblings"
                 , "softmaint"
                 , "switch"
                 , "traffic_lights"
                 , "transparency"
                 , "twopi"
                 , "unix"
                 , "world"
                 ] in
    forM_ examples $ \name ->
      it ("should parse `" <> name <> "` example") $ do
        content <- readFile $ "./testcases/" <> name <> ".gv"
        parse content `shouldSatisfy` isRight
