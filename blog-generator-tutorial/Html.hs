module Html
  ( Html,
  Title,
  Structure,
  p_,
  h1_,
  append_,
  render,
  escape,
  html_
  )
where


-- a set of functions that creates different HTML structures

head_ :: String -> String
head_ = el "head"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

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