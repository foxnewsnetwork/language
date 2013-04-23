{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module American where

import Control.Monad.State
import Language.English



nouns :: [Noun]
nouns = [n1, n2, n3, n4, n5, n6]
  where n1 = N "faggot" "faggots"
        n2 = N "dog" "dogs"
        n3 = N "sheep" "sheep"
        n4 = N "dick" "dicks"
        n5 = N "asshole" "assholes"
        n6 = N "apple" "apples"

verbs :: [Verb]
verbs = [v1, v2, v3, v4, v5, v6]
  where v1 = V "eat" "eats" "ate" "eaten"
        v2 = V "hit" "hits" "hit" "hit"
        v3 = V "stab" "stabs" "stabbed" "stabbed"
        v4 = V "rape" "rapes" "raped" "raped"
        v5 = V "beat" "beats" "beat" "beaten"
        v6 = V "see" "sees" "saw" "seen"

nounStates :: [NounState]
nounStates = filter isNounGood [ NState x y | x <- [Singular, Plural], y <- [The, A, Some, Unknown] ]

isNounGood :: NounState -> Bool
isNounGood ns = foldr ( (&&) . ($ ns) ) True checks
  where checks = [pluralSpecificity]
        pluralSpecificity s = case (specificity s, plurality s) of
          (A, Plural) -> False
          _           -> True

verbStates :: [VerbState]
verbStates = [ VState x y | x <- [Singular, Plural], y <- [Past, Present, PastPerfect] ]

testSentences :: [String]
testSentences = [ sentencify subject action object | subject <- subjects, action <- actions, object <- subjects ]

sentencify :: English NounState Noun -> English VerbState Verb -> English NounState Noun -> String
sentencify s a o = unwords [show s, show a, show o]

actions :: [English VerbState Verb]
actions = [ put s >> return verb | s <- verbStates, verb <- verbs ]

subjects :: [English NounState Noun]
subjects = [ put s >> return noun | s <- nounStates, noun <- nouns ]