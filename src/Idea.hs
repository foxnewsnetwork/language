module Idea 
( Idea(..)
) where

data Idea noun verb = Idea { subject :: noun, object :: noun, action :: verb }