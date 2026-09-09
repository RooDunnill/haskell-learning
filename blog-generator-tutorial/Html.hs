-- exports all of these functions as the module Html
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

-- imports all functions to be used as an unstable package for developers
import Html.Internal
