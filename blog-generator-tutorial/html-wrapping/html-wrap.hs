-- should always give the typing of each function beforehand
main :: IO ()
main = putStrLn myhtml

myhtml :: String
myhtml = makeHtml "My Title" "My Body"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

body_ :: String -> String
body_ content = "<body>" <> content <> "</body>"
html_ :: String -> String
html_ content = "<html>" <> content <> "</html>"
title_ :: String -> String
title_ content = "<title>" <> content <> "</title>"
head_ :: String -> String
head_ content = "<head>" <> content <> "</head>"

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ body)  
