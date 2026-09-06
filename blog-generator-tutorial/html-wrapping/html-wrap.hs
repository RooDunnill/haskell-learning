-- should always give the typing of each function beforehand
main :: IO ()
main = putStrLn myhtml

myhtml :: String
myhtml = makeHtml "My Title" (h1_ "Hello World!" <> p_ "My Body")

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

body_ :: String -> String
body_ = el "body"
html_ :: String -> String
html_ = el "html"
title_ :: String -> String
title_ = el "title"
head_ :: String -> String
head_ = el "head"
p_ :: String -> String
p_ = el "p"
h1_ :: String -> String
h1_ = el "h1"  

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ body)  
