{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "*.html" $ do
      route idRoute
      compile $
        getResourceBody >>=
        applyAsTemplate defaultContext >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
