module Main where

import Criterion.Config
import Criterion.Main
import Problems (allProblems)
import Types

myConfig = defaultConfig {
             cfgSamples = ljust 1
           }

toBench :: Problem -> Benchmark
toBench prob = bench (description prob) $ ((return $ solution prob) :: IO String)

main = Criterion.Main.defaultMainWith myConfig (return ()) $
       map toBench allProblems