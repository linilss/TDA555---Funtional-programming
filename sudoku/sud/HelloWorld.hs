module Main where

import Haste

main = do
  name <- prompt "Dear user, what is your name?"
  withElem "hastebox" $ \box -> do
    oldtext <- getProp box "value"
    setProp box "value" (oldtext ++ "\n-- Hello " ++ name)
