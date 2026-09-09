module Markup (Document, Structure(..)) where

import Numeric.Natural

import Data.Word (Word8) 


type Document = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]

parse :: String -> Document
-- lines is an inbuilt function that takes String -> [String]
-- It splits up a String into a list where each element of the list is 1 line of that String
-- parseLines [] runs the function parseLines with an empty list as its first input
parse = parseLines [] . lines


parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
    -- let creates a temporary value to use in the in statement
  let
    -- reverse is a function that reverses the order of the list
    -- unlines is the opposite of lines taking [String] -> String
    -- Paragraph is the element in the data Structure
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case txts of
    -- this is saying, do this conversion if you get an empty line
      [] -> [paragraph]
      -- a single colon means put something at the front of the list
      -- it is used within pattern matching so splits the first line of txts and defines it as currentLine
      -- the rest of txts goes into rest
      currentLine : rest ->
        if trim currentLine == ""
        -- checking if after removing whitespace if there is anything left
          then
            -- starts a new parseLines with a blank paragraph
            paragraph : parseLines [] rest
          else
            -- calls the function parseLines with currentLine and currentParagraph concatinated
            parseLines (currentLine : currentParagraph) rest

-- removes unnesccessary whitespace from a String
trim :: String -> String
trim = unwords . words

-- creates a boolean data typr
data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data Colour
  = RGB Word8 Word8 Word8

data AnsiColour
  = AnsiColour Brightness EightColor

ansiColorToVGA :: AnsiColour -> Colour
ansiColorToVGA ansicolour =
  case ansicolour of
    -- red green blue values
    AnsiColour Dark Black ->
      RGB 0 0 0
    AnsiColour Bright Black ->
      RGB 85 85 85
    AnsiColour Dark Red ->
      RGB 170 0 0
    AnsiColour Bright Red ->
      RGB 255 85 85

ansiToUbuntu :: AnsiColour -> Colour
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColour brightness colour ->
      case brightness of
        Dark ->
          case colour of
            Black -> RGB 1 1 1
            Red -> RGB 22 56 43
            Green -> RGB 57 181 74
            Yellow -> RGB 255 199 6
            Blue -> RGB 0 111 184
            Magenta -> RGB 118 38 113
            Cyan -> RGB 44 181 233
            White -> RGB 204 204 204

        Bright ->
          case colour of
            Black -> RGB 128 128 128
            Red -> RGB 255 0 0
            Green -> RGB 0 255 0
            Yellow -> RGB 255 255 0
            Blue -> RGB 0 0 255
            Magenta -> RGB 255 0 255
            Cyan -> RGB 0 255 255
            White -> RGB 255 255 255

isBright :: AnsiColour -> Bool
isBright ansicolour =
    case ansicolour of
        AnsiColour Bright _ -> True
        AnsiColour Dark _ -> False

isEmpty :: [a] -> Bool
isEmpty list =
    case list of
        [] -> True
        _ : _ -> False