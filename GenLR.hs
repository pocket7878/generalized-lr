{-# LANGUAGE TemplateHaskell #-}
module GenLR where
import Data.List
import Control.Monad
import Control.Lens
import Text.Printf
import System.IO 
import System.Environment 

-- 終端記号と非終端記号(行末も終端記号としておく)
data TerminalPart = Noun | Det | Prep | Verb | EOL deriving (Show,Eq)
data NonterminalPart = S | NP | VP | PP deriving (Show, Eq)
-- 終端記号と非終端記号どちらか
type Part = Either TerminalPart NonterminalPart 

-- 文法規則 非終端記号から記号列への展開 _left -> _light
data Rule = Rule {
  _left :: NonterminalPart,
  _right :: [Part]
} deriving (Show, Eq)
makeLenses ''Rule
-- 文法規則の生成のためのラッパー
makeRule :: NonterminalPart -> [Part] -> Rule
makeRule np ps = Rule {_left = np, _right = ps}

-- 文中のトークン 実際の文字と、品詞(終端記号)を持つ
data Token = Token { 
  _str :: String,
  _tp  :: TerminalPart
} deriving (Show,Eq)
makeLenses ''Token

-- 文はトークンの列
type Sentence = [Token]
-- 状態番号(0..)、文法規則番号(1..)
type State = Int
type RuleId = Int
-- 解析された構文木
data ParseTree = Leaf Token Int | Fork NonterminalPart [ParseTree] State RuleId deriving (Show,Eq)

-- アクション shift, reduction, accept, empty(error), conflict
data LRAction = Shift State | Re RuleId | Acc | Empty | Conf [LRAction] deriving (Show,Eq)
-- アクションテーブル
type ActionTable = [(State, TerminalPart, LRAction)]
-- タプルの3つめを返すための簡易関数
third (a,b,c) = c
-- ある状態から非終端記号を見てあたらしい状態へ
type GotoTable = [(State, NonterminalPart, State)]
-- GotoテーブルからのLookup
lookupGoto :: GotoTable -> State -> NonterminalPart -> Maybe State
lookupGoto table st np = case matchedGoto of
                            Just (_,_,s) -> Just s
                            Nothing -> Nothing
  where
    matchedGoto = find (\(fs, p, ts) -> fs == st && np == p) table

-- LR構文解析表 アクションテーブルとGotoテーブルの両方
data LRTable = LRTable {
  _actions :: ActionTable,
  _gotos :: GotoTable
} deriving (Show)
makeLenses ''LRTable

-- 言語定義,文法規則とLR構文解析表
data Language = Language {
  _rules :: [Rule],
  _table :: LRTable
} deriving (Show)
makeLenses ''Language

-- パース中の構文木の状態と状態番号のペア
type ParseState = (ParseTree, State)
-- スタックに入れられる可能性のある要素 (0で初期化するためにIntもしくはParseState)
type StackItem = Either Int ParseState
-- 構文木をStackItemからとりだす Intがくる事は原則ないので無視
getParseTree :: StackItem -> ParseTree
getParseTree (Right ps) = fst ps
-- スタック
data Stack = Stack [StackItem]

empty :: Stack
empty = Stack []

push :: Stack -> StackItem -> Stack
push (Stack xs) p = Stack (p:xs)

pop :: Stack -> Stack
pop (Stack (x:xs)) = Stack xs
pop (Stack []) = Stack []

peek :: Stack ->  StackItem
peek (Stack (x:xs)) = x

-- 現在のスタックのトップから状態番号を取りだすIntだったらそれを返す,ParseStateだったらその状態番号を返す
currentState :: Stack -> State
currentState (Stack (x:xs)) = case x of
                                Left i -> i
                                Right (_,s) -> s

-- Shiftアクションの処理、現在のトークンと指定された次状態のペアをスタックにつむ
runShift :: Sentence -> Language -> Stack -> State -> [Maybe ParseTree]                          
runShift st@(x:xs) lang stack newState = generalizedLR' xs lang (push stack (Right ((Leaf x newState),newState)))

-- Reductionアクションの処理
runReduction :: Sentence -> Language -> Stack -> RuleId -> [Maybe ParseTree]
runReduction st lang stack rid = generalizedLR' st lang newStack
        where
          rule :: Rule
          -- 対応する文法規則を取りだし
          rule = (lang^.rules) !! (rid-1)
          popedStack :: Stack
          -- スタックの先頭から文法規則の右辺の長さと同じだけpopする
          popedStack = iterate pop stack !! (length (rule^.right))
          -- popされた後のスタックの先頭から得られる状態と、文法規則の右辺によりGotoテーブルから次状態を決定
          nextState = lookupGoto (lang^.table^.gotos) (currentState popedStack) (rule^.left)
          -- popされた要素を確保
          popedTree = reverse $ map (getParseTree . peek) $ take (length (rule^.right)) (iterate pop stack)
          -- 次状態が決定していたら、popされた要素を子とするパース木をスタックの先頭に追加,次状態が存在しなければ還元失敗(エラー)
          newStack = case nextState of
                      Just s -> (push popedStack (Right ((Fork (rule^.left) popedTree s rid,s))))
                      Nothing -> error "parse Reduction Failed"

-- アクションの処理
runAction :: Sentence -> LRAction -> Language -> Stack -> [Maybe ParseTree]
runAction st action lang stack = case action of 
                                          (Shift s) -> runShift st lang stack s
                                          Acc -> [Just(getParseTree $ peek stack)]
                                          Empty -> [Nothing]
                                          (Conf acs) -> concat $ map (\ac -> runAction st ac lang stack) acs
                                          (Re i) -> runReduction st lang stack i

-- 文の状態とスタックからアクションを決定する
--- 文が空だったら、それは文末に来たのだと扱う
generalizedLR' [] lang stack = case matchAction of
                                  Just ac -> runAction [] (third ac) lang stack
  where 
    state = currentState stack
    matchAction = find (\(s,t,ac) -> s == state && t == EOL) (lang^.table^.actions)

--- 文が空でなかったら、文の先頭のTokenからアクションを決定
generalizedLR' st@(x:xs) lang stack = case matchAction of
                                          Just ac -> runAction st (third ac) lang stack
  where
    state = currentState stack
    matchAction = find (\(s,t,ac) -> s == state && t == x^.tp) (lang^.table^.actions)

-- 文と言語からGLR構文解析 (0で初期化したスタックを利用する)
generalizedLR sent lang = generalizedLR' sent lang (push empty (Left 0))

-- 英語の文法規則
englishRule = [
  (makeRule S [(Right NP), (Right VP)]), 
  (makeRule S [(Right S), (Right PP)]),
  (makeRule NP [(Left Noun)]),
  (makeRule NP [(Left Det), (Left Noun)]),
  (makeRule NP [(Right NP), (Right PP)]),
  (makeRule PP [(Left Prep), (Right NP)]),
  (makeRule VP [(Left Verb), (Right NP)])]

-- パース対象の英文
englishSentence = [
  (Token "I" Noun),
  (Token "saw" Verb),
  (Token "a" Det),
  (Token "man" Noun),
  (Token "with" Prep),
  (Token "a" Det),
  (Token "telescope" Noun),
  (Token "in" Prep),
  (Token "the" Det),
  (Token "park" Noun)]

-- 英語のアクションテーブル
englishActionTable = [
  (0,Det,(Shift 3)),
  (0,Noun,(Shift 4)),
  (0,Verb,Empty),
  (0,Prep,Empty),
  (0,EOL,Empty),

  (1,Det,Empty),
  (1,Noun,Empty),
  (1,Verb,Empty),
  (1,Prep,(Shift 6)),
  (1,EOL,Acc),

  (2,Det,Empty),
  (2,Noun,Empty),
  (2,Verb,(Shift 7)),
  (2,Prep,(Shift 6)),
  (2,EOL,Empty),

  (3,Det,Empty),
  (3,Noun,(Shift 10)),
  (3,Verb,Empty),
  (3,Prep,Empty),
  (3,EOL,Empty),

  (4,Det,Empty),
  (4,Noun,Empty),
  (4,Verb,(Re 3)),
  (4,Prep,(Re 3)),
  (4,EOL,(Re 3)),

  (5,Det,Empty),
  (5,Noun,Empty),
  (5,Verb,Empty),
  (5,Prep,(Re 2)),
  (5,EOL,(Re 2)),

  (6,Det,(Shift 3)),
  (6,Noun,(Shift 4)),
  (6,Verb,Empty),
  (6,Prep,Empty),
  (6,EOL,Empty),

  (7,Det,(Shift 3)),
  (7,Noun,(Shift 4)),
  (7,Verb,Empty),
  (7,Prep,Empty),
  (7,EOL,Empty),

  (8,Det,Empty),
  (8,Noun,Empty),
  (8,Verb,Empty),
  (8,Prep,(Re 1)),
  (8,EOL,(Re 1)),

  (9,Det,Empty),
  (9,Noun,Empty),
  (9,Verb,(Re 5)),
  (9,Prep,(Re 5)),
  (9,EOL,(Re 5)),

  (10,Det,Empty),
  (10,Noun,Empty),
  (10,Verb,(Re 4)),
  (10,Prep,(Re 4)),
  (10,EOL,(Re 4)),

  (11,Det,Empty),
  (11,Noun,Empty),
  (11,Verb,(Re 6)),
  (11,Prep,(Conf [(Shift 6),(Re 6)])),
  (11,EOL,(Re 6)),

  (12,Det,Empty),
  (12,Noun,Empty),
  (12,Verb,Empty),
  (12,Prep,(Conf [(Shift 6),(Re 7)])),
  (12,EOL,(Re 7))]

-- 英語のGotoテーブル
englishGotoTable = [
  (0,S,1),
  (0,NP,2),
  (1,PP,5),
  (2,VP,8),
  (2,PP,9),
  (6,NP,11),
  (7,NP,12),
  (11,PP,9),
  (12,PP,9)]

-- 英語のLR構文解析表
englishLRTable = LRTable {_actions = englishActionTable, _gotos = englishGotoTable}

-- 英語の言語定義
englishLang = Language englishRule englishLRTable


-- ParseTreeをDOT言語として出力
type IndentLevel = Int
--- インデントをスペースでつける
putIndent :: Handle -> IndentLevel -> IO ()
putIndent h ident = hPutStr h $ take ident (repeat ' ')

--- 辺を出力(始点はかならずforkである)
printEdge :: Handle -> Int -> Int -> ParseTree -> IO ()
printEdge h tid nid (Fork _ _ _ _) = hPrintf h "f%d -> f%d;\n" tid nid
printEdge h tid nid (Leaf _ _) = hPrintf h "f%d -> n%d;\n" tid nid

type MaxId = Int
parseTree2DOT' :: Handle -> MaxId -> ParseTree -> Bool -> IO Int
parseTree2DOT' h i (Leaf token st) v = do {
      putIndent h 8;
      if v 
        then hPrintf h "n%d [label=\"%s (%s) state=%d\"];\n" i (token^.str) (show (token^.tp)) st;
        else hPrintf h "n%d [label=\"%s (%s)\"];\n" i (token^.str) (show (token^.tp));
      return (i+1);
}
parseTree2DOT' h i (Fork ntp ts st rid) v = do {
      putIndent h 8;
      if v
        then hPrintf h "f%d [label=\"%s state=%d rule=%d\"];\n" i (show ntp) st rid;
        else hPrintf h "f%d [label=\"%s\"];\n" i (show ntp);
      foldM (\a b -> do {
                  putIndent h 8;
                  printEdge h i a b;
                  parseTree2DOT' h a b v;
                  }) (i+1) ts;
}

parseTree2DOT :: Handle -> ParseTree -> Bool -> IO ()
parseTree2DOT h t@(Fork _ _ _ _) verbose = do {
      hPutStrLn h "digraph parsetree {";
      parseTree2DOT' h 0 t verbose;
      hPutStrLn h "}";
}

-- Maybe ParseTreeのListを個別のファイルに出力
type FilenamePrefix = String
type Verbose = Bool
outputTrees :: FilenamePrefix -> [Maybe ParseTree] -> Verbose ->  IO ()
outputTrees prefix ts verbose = do {
    foldM_ (\i t -> do {
      withFile (prefix ++ (show i) ++ ".dot") WriteMode (\h -> case (ts !! i) of
                                                                 Just t -> do {parseTree2DOT h t verbose; return (i+1)};
                                                                 Nothing -> return i)
    }) 0 ts;
}
