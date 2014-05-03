{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
module Language.ECMAScript5.ParserState 
       ( Comment(..)
       , SourceSpan(..) 
       , Positioned
       , ParserAnnotation
       , ParserState
       , InParserState
       , HasComments
       , Parser
       , PosParser
       , InParser
       , PosInParser
       , ParExpr
       , withPos
       , postfixWithPos
       , prefixWithPos
       , infixWithPos
       , getComments
       , allowIn
       , liftIn
       , withIn
       , withNoIn
       , assertInAllowed
       , changeState
       , initialParserState
       , modifyNewLine
       , modifyLabelSet
       , modifyComments
       , pushLabel
       , clearLabelSet
       , pushEnclosing
       , popEnclosing
       , withFreshEnclosing
       , setNewLineState 
       , hadNewLine 
       , hadNoNewLine
       , getEnclosing
       , isIter
       , isIterSwitch
       , HasLabelSet (..)
       , EnclosingStatement (..)
       , spanBegin
       , spanEnd
       , WhiteSpaceState
       ) where 
 
import Text.Parsec hiding (labels) 
import Text.Parsec.Pos (initialPos)
--import Test.Parsec.Prim
import Language.ECMAScript5.Syntax 
import Language.ECMAScript5.Syntax.Annotations 
import Data.Default.Class 
import Data.Default.Instances.Base 
import Control.Monad.Identity 
import Control.Applicative
import Control.Monad.State (modify)
 
import Data.Data (Data)
import Data.Typeable (Typeable)



type Positioned x = x ParserAnnotation 
 
type Parser   a = forall s. Stream s Identity Char => ParsecT s ParserState Identity a 
type InParser a =  forall s. Stream s Identity Char => ParsecT s InParserState Identity a 
type PosParser x = Parser (Positioned x)
type PosInParser x = InParser (Positioned x)

type ExprParser = Parser (Expression ExpressionAnnotation)
 
type WhiteSpaceState = (Bool, SourcePos)

data ParserState = ParserState { whiteSpaceState :: WhiteSpaceState, comments :: [Comment], enclosing :: [EnclosingStatement], labelSet :: [Label] }
                 deriving (Show)
data InParserState = InParserState { allowIn :: Bool, baseState :: ParserState } 

data EnclosingStatement = EnclosingIter [Label]
                          -- ^ The enclosing statement is an iteration statement
                        | EnclosingSwitch [Label]
                          -- ^ The enclosing statement is a switch statement
                        | EnclosingOther [Label]
                          -- ^ The enclosing statement is some other
                          -- statement.  Note, `EnclosingOther` is
                          -- never pushed if the current `labelSet` is
                          -- empty, so the list of labels in this
                          -- constructor should always be non-empty

instance Show EnclosingStatement where
  show (EnclosingIter ls)   = "iteration" ++ show ls
  show (EnclosingSwitch ls) = "switch" ++ show ls
  show (EnclosingOther ls)  = "statement" ++ show ls

isIter :: EnclosingStatement -> Bool
isIter (EnclosingIter _) = True
isIter _                 = False

isIterSwitch :: EnclosingStatement -> Bool
isIterSwitch (EnclosingIter _)   = True
isIterSwitch (EnclosingSwitch _) = True
isIterSwitch _                   = False

class HasLabelSet a where
  getLabelSet :: a -> [Label]
  setLabelSet :: [Label] -> a -> a

modifyLabelSet :: HasLabelSet a => ([Label] -> [Label]) -> a -> a
modifyLabelSet f a = setLabelSet (f $ getLabelSet a) a

instance HasLabelSet EnclosingStatement where
  getLabelSet e = case e of
    EnclosingIter ls   -> ls
    EnclosingSwitch ls -> ls
    EnclosingOther ls  -> ls
  setLabelSet ls e = case e of
    EnclosingIter _   -> EnclosingIter ls
    EnclosingSwitch _ -> EnclosingSwitch ls
    EnclosingOther _  -> EnclosingOther ls

instance HasLabelSet ParserState where
  getLabelSet ps = labelSet ps
  setLabelSet ls ps = ps {labelSet = ls}

type Label = String

data SourceSpan =  
  SourceSpan {spanBegin :: SourcePos
             ,spanEnd :: SourcePos}
  deriving (Data, Typeable)

data Comment  
  = SingleLineComment String  
  | MultiLineComment String  
    deriving (Show, Data, Typeable)
 
class HasWhiteSpacePos a where
  getWhiteSpaceStartPos :: a -> SourcePos

instance HasWhiteSpacePos ParserState where
  getWhiteSpaceStartPos = snd . whiteSpaceState
    
instance HasWhiteSpacePos InParserState where
  getWhiteSpaceStartPos = snd . whiteSpaceState . baseState

class HasComments a where 
  getComments :: a -> [Comment] 
  setComments :: [Comment] -> a -> a 
  modifyComments :: ([Comment] -> [Comment]) -> a -> a 
  modifyComments f st = setComments (f $ getComments st) st
 
instance HasComments ParserState where 
  getComments = comments 
  setComments cs st = st { comments = cs } 
 
instance HasComments InParserState where 
  getComments = comments . baseState 
  setComments cs st = st { baseState = setComments (baseState st) cs }

instance HasComments ParserAnnotation where
  getComments (ParserAnnotation _ cmts) = cmts
  setComments cmts (ParserAnnotation span _) = ParserAnnotation span cmts

instance HasComments ExpressionAnnotation where
  getComments = getComments . toParserAnnotation
  setComments cmts (ExpressionAnnotation par pa) = ExpressionAnnotation par $ setComments pa cmts
  
data ParserAnnotation = ParserAnnotation SourceSpan [Comment]

data ExpressionAnnotation = ExpressionAnnotation {parenthesized :: Parenthesized
                                                 ,toParserAnnotation :: ParserAnnotation
                                                 }

class HasPosition x where
  getPos :: x -> SourceSpan
  setPos :: SourceSpan -> x -> x

modifyPos :: (HasPosition x) => (SourceSpan -> SourceSpan) -> x -> x
modifyPos f x = setPos (f $ getPos x) x

instance HasPosition ParserAnnotation where
  getPos (ParserAnnotation span _) = span
  setPos span (ParserAnnotation _ cmt) = ParserAnnotation span cmt

instance HasPosition ExpressionAnnotation where
  getPos = getPos . toParserAnnotation
  setPos span (ExpressionAnnotation par pa) = ExpressionAnnotation par $ setPos span pa

-- | Tells if an expression was in parenthesis in the source text
newtype Parenthesized = Parenthesized Bool

instance Default Parenthesized where
  def = Parenthesized False

instance Default SourcePos where 
  def = initialPos "" 
 
instance Default SourceSpan where 
  def = SourceSpan def def
 
instance Show SourceSpan where 
  show (SourceSpan p1 p2) = let 
    l1 = show $ sourceLine p1 - 1 
    c1 = show $ sourceColumn p1 - 1 
    l2 = show $ sourceLine p2 - 1 
    c2 = show $ sourceColumn p2 - 1 
    s1 = l1 ++ "-" ++ c1 
    s2 = l2 ++ "-" ++ c2 
    in "(" ++ show (s1 ++ "/" ++ s2) ++ ")" 
 
consumeComments :: (HasComments state) => Stream s Identity Char => ParsecT s state Identity [Comment] 
consumeComments = do comments <- getComments <$> getState 
                     modifyState $ modifyComments (const []) 
                     return comments 

-- a convenience wrapper to take care of the position, "with
-- position". Whenever we return something `Positioned` we need to use
-- it.
withPos   :: (HasAnnotation x, HasComments state, HasWhiteSpacePos state, Stream s Identity Char, HasPosition a, HasComments a) 
          => ParsecT s state Identity (x a)
          -> ParsecT s state Identity (x a)
withPos p = do start <- getPosition 
               comments <- consumeComments 
               result <- p 
               end <- getWhiteSpaceStartPos <$> getState
               return $ withAnnotation (setComments comments . setPos (SourceSpan start  end)) result 
 
postfixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char, HasPosition a, HasComments a) => 
                  ParsecT s state Identity (x a -> x a) ->  
                  ParsecT s state Identity (x a -> x a)
postfixWithPos p = do 
  f <- p 
  high <- getPosition 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan low _) = getPos $ getAnnotation e  
                 in withAnnotation (setPos (SourceSpan low high) . setComments comments) (f e) 
 
prefixWithPos :: (HasAnnotation x, HasComments state, Stream s Identity Char) => 
                  ParsecT s state Identity (Positioned x -> Positioned x) ->  
                  ParsecT s state Identity (Positioned x -> Positioned x) 
prefixWithPos p = do 
  low <- getPosition 
  f <- p 
  comments <- consumeComments 
  return $ \e -> let (SourceSpan _ high) = getPos $ getAnnotation e  
                 in withAnnotation (setPos (SourceSpan low high) . setComments comments) (f e) 

infixWithPos :: (HasAnnotation x, Stream s Identity Char) =>
                ParsecT s state Identity (Positioned x -> Positioned x -> Positioned x) ->
                ParsecT s state Identity (Positioned x -> Positioned x -> Positioned x)
infixWithPos p = 
  liftAnnotations2 combinePos <$> p
  where combinePos an1 an2 =
          let (SourceSpan _ high) = getPos an2
          in modifyPos (\(SourceSpan low _) -> SourceSpan low high) an2
        liftAnnotations2 f g x y = setAnnotation (f (getAnnotation x) (getAnnotation y)) (g x y)

liftIn :: Bool -> Parser a -> InParser a 
liftIn x p = changeState (InParserState x) baseState p 
 
withIn, withNoIn :: InParser a -> Parser a 
withIn   p = changeState baseState (InParserState True) p 
withNoIn p = changeState baseState (InParserState False) p 
 
assertInAllowed :: InParser () 
assertInAllowed = getState >>= guard.allowIn 
 
changeState 
  :: forall m s u v a . (Functor m, Monad m) 
  => (u -> v) 
  -> (v -> u) 
  -> ParsecT s u m a 
  -> ParsecT s v m a 
changeState forward backward = mkPT . transform . runParsecT 
  where 
    mapState f st = st { stateUser = f (stateUser st) } 
    mapReply f (Ok a st err) = Ok a (mapState f st) err 
    mapReply _ (Error e) = Error e 
    transform p st = (fmap . fmap . fmap) (mapReply forward) (p (mapState backward st)) 
 
modifyNewLine  f st = st { whiteSpaceState = f (whiteSpaceState st) }
modifyEnclosing f st = st { enclosing = f (enclosing st) }
 
initialParserState :: ParserState 
initialParserState = ParserState (False, initialPos "") [] [] []

-- | checks if the label is not yet on the stack, if it is -- throws 
-- an error; otherwise it pushes it onto the stack 
pushLabel :: Id a -> Parser (Id a)
pushLabel ident = do ps <- getState 
                     pos <- getPosition
                     encs <- getEnclosing
                     let lab  = unId ident
                     let labs = labelSet ps ++ concatMap getLabelSet encs
                     if lab `elem` labs 
                       then fail $ "Duplicate label at " ++ show pos 
                       else putState (ps {labelSet = (lab:(labelSet ps))}) >> return ident

clearLabelSet :: Parser ()
clearLabelSet = modifyState $ modifyLabelSet (const [])

pushEnclosing :: ([Label] -> EnclosingStatement) -> Parser ()
pushEnclosing ctr = do labs <- getLabelSet <$> getState
                       modifyState (modifyEnclosing (ctr labs:))
                       clearLabelSet

popEnclosing :: Parser () 
popEnclosing = modifyState (modifyEnclosing safeTail) 
  where safeTail [] = [] 
        safeTail (_:xs) = xs 

clearEnclosing = modifyEnclosing (const [])

getEnclosing :: Parser [EnclosingStatement]
getEnclosing = enclosing <$> getState

withFreshEnclosing :: Parser a -> Parser a 
withFreshEnclosing p = do oldEnclosing <- getEnclosing
                          modifyState clearEnclosing
                          clearLabelSet
                          a <- p 
                          modifyState $ modifyEnclosing (const oldEnclosing)
                          return a 
 
setNewLineState :: WhiteSpaceState -> Parser WhiteSpaceState
setNewLineState st = do
    modifyState $ modifyNewLine (const st) 
    return st
 
hadNewLine :: Parser () 
hadNewLine = fst . whiteSpaceState <$> getState >>= guard 
 
hadNoNewLine :: Parser () 
hadNoNewLine = fst . whiteSpaceState <$> getState >>= guard.not
