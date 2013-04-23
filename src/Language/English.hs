{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.English 
( WordState(..)
, Word(..)
, Noun(..)
, Verb(..)
, NounState(..)
, VerbState(..)
) where

import Control.Monad.State

class WordState a where
  plurality   :: a -> Plurality
  specificity :: a -> Specificity
  temporality :: a -> Temporality

class Word a where
  singular    :: a -> String
  plural      :: a -> String
  past        :: a -> String
  pastPerfect :: a -> String

data Noun = N { nSingular :: String, nPlural :: String }
data Verb = V { vSingular :: String, vPlural :: String, vPast :: String, vPastPerfect :: String }

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

data NounState = NState { nPlurality :: Plurality, nSpecificity :: Specificity }
data VerbState = VState { vPlurality :: Plurality, vTemporality :: Temporality }
data Plurality = Singular | Plural deriving(Show, Eq)
data Temporality = PastPerfect | Past | Present deriving(Show, Eq)
data Specificity = The | A | Unknown | Some deriving(Show, Eq)

instance WordState NounState where
  plurality     = nPlurality
  specificity   = nSpecificity
  temporality _ = Present -- in English, nouns do not have temporality

instance WordState VerbState where
  plurality     = vPlurality
  specificity _ = Unknown -- in English, verbs do not have specificity
  temporality   = vTemporality

type English state a = State state a

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