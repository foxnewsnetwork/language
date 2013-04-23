{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.English 
( WordState(..)
, Word(..)
, Noun(..)
, Verb(..)
, Adjective(..)
, Adverb(..)
, NounState(..)
, VerbState(..)
, Plurality(..)
, Temporality(..)
, Specificity(..)
, English
, Descriptive(..)
) where

import Control.Monad.State

type English state a = State state a
type EnglishNoun = English NounState Noun
type EnglishVerb = English VerbState Verb
class WordState a where
  plurality     :: a -> Maybe Plurality
  specificity   :: a -> Maybe Specificity
  temporality   :: a -> Maybe Temporality
  transitivity  :: a -> Maybe Transitivity

class Descriptive a where
  describeNoun :: a -> EnglishNoun -> EnglishNoun
  describeVerb :: a -> EnglishVerb -> EnglishVerb

class Word a where
  singular    :: a -> String
  plural      :: a -> String
  past        :: a -> String
  pastPerfect :: a -> String

data Noun = N { nSingular :: String, nPlural :: String }
data Verb = V { vSingular :: String, vPlural :: String, vPast :: String, vPastPerfect :: String }
data Adjective = Adj String
data Adverb = Adv String

instance Descriptive Adjective where
  describeVerb _ = id
  describeNoun (Adj str) = liftM $ nmap ( (str ++) . (" " ++) )
    where nmap f (N a b) = N (f a) (f b)

instance Descriptive Adverb where
  describeNoun _ = id
  describeVerb (Adv adverb) verbM = get >>= modifyBasedOnTransitivity . transitivity
    where modifyBasedOnTransitivity Nothing = verbM
          modifyBasedOnTransitivity (Just Transitive) = liftM ( vmap ( (adverb ++) . (" " ++) ) ) verbM
          modifyBasedOnTransitivity (Just Intransitive) = liftM ( vmap ( (++ adverb) . (++ " ") ) ) verbM
          vmap f (V a b c d) = V (f a) (f b) (f c) (f d)

instance Word Noun where
  singular    = nSingular
  plural      = nPlural
  past        = singular
  pastPerfect = singular

instance Word Verb where
  singular    = vSingular
  plural      = vPlural
  past        = vPast
  pastPerfect = vPastPerfect

instance WordState NounState where
  plurality ns    = Just $ nPlurality ns
  specificity ns  = Just $ nSpecificity ns
  temporality _   = Nothing -- in English, nouns do not have temporality
  transitivity _  = Nothing -- in English, nouns don't have transitivity either  

instance WordState VerbState where
  plurality vs    = Just $ vPlurality vs
  specificity _   = Nothing -- in English, verbs do not have specificity
  temporality vs  = Just $ vTemporality vs
  transitivity _  = Nothing

data NounState = NState { nPlurality :: Plurality, nSpecificity :: Specificity }
data VerbState = VState { vPlurality :: Plurality, vTemporality :: Temporality, vTransitivity :: Transitivity }
data Plurality = Singular | Plural deriving(Show, Eq)
data Temporality = PastPerfect | Past | Present deriving(Show, Eq)
data Specificity = The | A | Unknown | Some deriving(Show, Eq)
data Transitivity = Transitive | Intransitive deriving(Show, Eq)

specify  :: WordState a => a -> String -> String
specify ws = case specificity ws of
  The     -> ("the " ++)
  A       -> appendAOrAn
  Some    -> ("some " ++)
  Unknown -> id

appendAOrAn :: String -> String
appendAOrAn [] = []
appendAOrAn s@(x:_)
  | isVowel x = "an " ++ s
  | otherwise = "a " ++ s
  where isVowel 'a' = True
        isVowel 'e' = True
        isVowel 'i' = True
        isVowel 'u' = True
        isVowel _   = False

pastPerfectify :: WordState a => a -> String -> String
pastPerfectify ws = case temporality ws of
  PastPerfect -> ("had " ++  )
  _           -> id

chooseForm :: (WordState a, Word b) => a -> b -> String
chooseForm ws = case temporality ws of
  Present     -> choosePlurality ws
  Past        -> past
  PastPerfect -> pastPerfect
    
choosePlurality :: (WordState a, Word b) => a -> b -> String
choosePlurality ws = case plurality ws of
  Plural    -> plural
  Singular  -> singular

stringifyWord :: (WordState a, Word b) => a -> b -> String 
stringifyWord ws w = pastPerfectify ws . specify ws $ chooseForm ws w


toString :: (WordState a, Word b) => English a b -> English a String
toString eword = get >>= \s -> 
  let (word, s') = runState eword s 
  in  return $ stringifyWord s' word

unwrapNoun :: English NounState String -> String
unwrapNoun english = fst $ runState english nullState
  where nullState = NState Singular The

unwrapVerb :: English VerbState String -> String
unwrapVerb english = fst $ runState english nullState
  where nullState = VState Singular Present

instance Show (English VerbState String) where
  show = unwrapVerb
instance Show (English NounState String) where
  show = unwrapNoun
instance Show (English VerbState Verb) where
  show = show . toString
instance Show (English NounState Noun) where
  show = show . toString