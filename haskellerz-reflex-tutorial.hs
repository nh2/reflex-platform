-- My setup with compile loop:
--   $ ./try-reflex
--   $ while (inotifywait -e close_write haskellerz-reflex-tutorial.hs || true); do ghcjs --make haskellerz-reflex-tutorial.hs 2>&1 | tee ghcid.txt; done
-- For my editor:
--   https://github.com/nh2/ghcid-sublime
-- Seeing the compiled result in my browser:
--   ./haskellerz-reflex-tutorial.jsexe/index.html
-- API docs:
--   $ ghc-pkg field reflex     haddock-html
--   $ ghc-pkg field reflex-dom haddock-html
--   Open the printed paths in your browser (appending "index.html" to the end).
-- Cheat sheets:
--   reflex:     https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
--   reflex-dom: https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import           Data.Monoid

import           Reflex.Dom


tutorialSection :: (MonadWidget t m) => m () -> m ()
tutorialSection widgets = divClass "tutorial-section" widgets


main :: IO ()
main = mainWidget $ do
  elAttr
    "link"
    ("href" =: "../haskellerz-reflex-tutorial.css" <>
     "rel" =: "stylesheet" <>
     "type" =: "text/css"
    ) $ return ()

  tutorialSection $ do
    text "Hello, world!"

  tutorialSection $ do
    text "We now have different tutorial sections."

    -- Nested HTML elements.
    el "p" (el "span" (text "Text in a span in a paragraph."))

    -- Nested HTML elements, monadic style.
    el "p" $ do
      el "span" $ do
        text "Text in a span in a paragraph."
