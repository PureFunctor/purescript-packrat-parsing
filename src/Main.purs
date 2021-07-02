module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Packrat.Basic as Basic


main :: Effect Unit
main = do
  log "Basic Parser: 2 * (3 + 4)"
  logShow $ Basic.eval "2*(3+4)"
