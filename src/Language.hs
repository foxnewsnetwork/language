module Language 
( Language(..)
, NounPhrase(..)
, VerbPhrase(..)
, AdjectivePhrase(..)
, AdverbPhrase(..)
) where

import qualified Idea as I

class Language l where
  grammar :: I.Idea (NounPhrase l) (VerbPhrase l) -> String

data NounPhrase word = NounPhrase word 
  | DescribeNoun (AdjectivePhrase word) (NounPhrase word)
  | AndNoun (NounPhrase word) (NounPhrase word)
  | OrNoun (NounPhrase word) (NounPhrase word)
  | NorNoun (NounPhrase word) (NounPhrase word)

data VerbPhrase word = VerbPhrase word 
  | DescribeVerb (AdverbPhrase word) (VerbPhrase word)
  | AndVerb (VerbPhrase word) (VerbPhrase word)
  | OrVerb (VerbPhrase word) (VerbPhrase word)
  | NorVerb (VerbPhrase word) (VerbPhrase word)

data AdjectivePhrase word = Possessive | Adjective word | Scope word
  | AndAdjective (AdjectivePhrase word) (AdjectivePhrase word)
  | OrAdjective (AdjectivePhrase word) (AdjectivePhrase word)
  | NorAdjective (AdjectivePhrase word) (AdjectivePhrase word)

data AdverbPhrase word = Temporal word | Adverb word
  | AndAdverb (AdverbPhrase word) (AdverbPhrase word)
  | OrAdverb (AdverbPhrase word) (AdverbPhrase word)
  | NorAdverb (AdverbPhrase word) (AdverbPhrase word)