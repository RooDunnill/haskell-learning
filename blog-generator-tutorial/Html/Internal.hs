module Html.Internal where

-- a set of functions that creates different HTML structures
head_ :: String -> String
head_ = el "head"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape 


-- defines new types Html and Structure
newtype Html = Html String
newtype Structure = Structure String

-- relables exisitng types to other types that are syntactial the same
type Title = String

-- takes in the title and body as strings and creates the necessary HTML around it before then returning it as an HTML type
html_ :: String -> Structure -> Html
html_ title body = Html (el "html" (head_ (el "title" (escape title)) <> el "body" (returnStructureString body)))

-- adds the HTML arrows around the division element you wish to create
el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


-- returns the string embedded in the Html type
render :: Html -> String
render html =
    -- case basically works as a substitute function, where if it matches the input it then converts to the output
    -- everything not caught by case stays the same
    case html of
        Html str -> str

-- appends two structures together
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

-- returns the string embedded in the Structure type
returnStructureString :: Structure -> String
returnStructureString struct = 
    case struct of
        Structure str -> str

-- removes all characters that would interfere with the HTML and replaces with seperate codes
escape :: String -> String
escape =
    let
        escapeChar :: Char -> String
        escapeChar c =
            case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
    in
        -- String type is a list of Chars [Char]
        -- concat concatinates a list of lists of a into a list of a :: [[a]] -> [a]
        -- map applies a function to each element in a list
        -- so concat . map escapeChar is applying escapeChar to every element in the String and then merging all of the lists of lists into a String
        concat . map escapeChar


-- unordered list wrapper
ul_ :: [Structure] -> Structure
ul_ =
  -- this takes each element of the list and wraps it in "li", then wraps the whole list in "ul"
  -- map goes through each structure in the list
  -- concat takes the list and turns it into one variable
  Structure . el "ul" . concat . map (el "li" . returnStructureString)

-- ordered list wrapper
ol_ :: [Structure] -> Structure
ol_ =
  -- this takes each element of the list and wraps it in "li", then wraps the whole list in "ul"
  -- map goes through each structure in the list
  -- concat takes the list and turns it into one variable
  Structure . el "ol" . concat . map (el "li" . returnStructureString)
