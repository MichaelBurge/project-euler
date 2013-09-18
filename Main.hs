module Main where

import Problems (allProblems)
import Types

main = mapM (\problem ->
                 putStrLn $
                 description problem ++
                 " | " ++
                 solution problem
            ) $ allProblems