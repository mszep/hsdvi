module Testmaybe where

import Data.Maybe


data Op = Optellen
        | Aftrekken
        | Vermenigvuldigen
        | Delen

isDistr :: Op -> Maybe Bool
isDistr op = do 



