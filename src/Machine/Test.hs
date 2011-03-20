module Machine.Test() where

import Data.Binary(decode, encode)
import Machine.Data
import Test.QuickCheck

serializationCheck :: Operation -> Bool
serializationCheck (Literal _) = True
serializationCheck op = (decode $ encode op) == op

main = quickCheck $ label "Binary Format" serializationCheck
