module Main (main) where

import AppStart as App

main :: IO () 
main =
    putStrLn startMessage >>
    App.run

startMessage = "                       _\n\
\                    /o _/_            //            /\n\
\                 __/,  /     _, __,  // _  _ _   __/ __,  _\n\
\                (_/_(_(__---(__(_/(_(/_(/_/ / /_(_/_(_/(_/ (_\n\
\\n\
\"