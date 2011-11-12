module Stacktest where

import Control.Monad.State


type StackState = State [Int]

pop :: StackState Int
pop = do oldstate <- get
         put $ tail oldstate
         return $ head oldstate

push :: Int -> StackState ()
push mynum = do oldstate <- get
                put $ mynum : oldstate

peek :: StackState Int
peek = do oldstate <- get
          return $ head oldstate
