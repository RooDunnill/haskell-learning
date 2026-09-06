module Main (main) where

import Html
import GHC.Internal.Text.Read (Lexeme(String))

-- should always give the typing of each function beforehand
main :: IO ()
main = putStrLn (render myhtml)

-- calls the function to make the HTML by inputting the Title and the body of text
myhtml :: Html
myhtml = html_ "My Title>" (append_ (h1_ "Hello World!") (p_ "My Body"))

