-- should always give the typing of each function beforehand
main :: IO ()
main = putStrLn (render myhtml)

-- calls the function to make the HTML by inputting the Title and the body of text
myhtml :: Html
myhtml = makeHtml "My Title" (h1_ "Hello World!" <> p_ "My Body")

-- takes in the title and body as strings and creates the necessary HTML around it before then returning it as an HTML type
makeHtml :: String -> String -> Html
makeHtml title body = Html (el "html" (head_ (title_ title) <> body_ body))

-- adds the HTML arrows around the division element you wish to create
el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- a set of functions that creates different HTML structures
body_ :: String -> String
body_ = el "body"
title_ :: String -> String
title_ = el "title"
head_ :: String -> String
head_ = el "head"
p_ :: String -> String
p_ = el "p"
h1_ :: String -> String
h1_ = el "h1"  


-- defines new types Html and Structure
newtype Html = Html String
newtype Structure = Structure String

-- relables exisitng types to other types that are syntactial the same
type Title = String

-- returns the string embedded in the Structure type
returnStructureString :: Structure -> String
returnStructureString struct = 
    case struct of
        Structure str -> str

-- returns the string embedded in the Html type
render :: Html -> String
render html =
    case html of
        Html str -> str

-- appends two structures together
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

