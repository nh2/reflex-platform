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
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- Library imports
import           Control.Monad.IO.Class
import           Data.Monoid
-- GHCJS specific imports
import           GHCJS.Types (JSString)
import           GHCJS.DOM.Types (toJSString)
-- Reflex specific imports
import           Reflex
import           Reflex.Dom


tutorialSection :: (MonadWidget t m) => m () -> m ()
tutorialSection widgets = divClass "tutorial-section" widgets


-- | Convenient function that pops up a javascript alert dialog box when an
-- event fires with a message to display.
--
-- Taken from the `reflex-dom-contrib` package.
alertEvent :: forall t m a . MonadWidget t m => (a -> String) -> Event t a -> m ()
alertEvent eventValueToStr e = performEvent_ (alert <$> e)
  where
    -- alert :: (MonadIO m) => String -> IO ()
    alert a = liftIO $ js_alert $ toJSString $ eventValueToStr a

foreign import javascript unsafe
  "alert($1)"
  js_alert :: JSString -> IO ()


main :: IO ()
main = mainWidget myWidgets

myWidgets :: (MonadWidget t m) => m ()
myWidgets = do
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

  tutorialSection $ do
    -- The type returned by a button is an Event that contains
    -- () -- no more information than the fact that the event occurred.
    -- buttonEv :: Event t () <- button "Click me"
    -- But of course, we may leave the type away, as Haskell infers it.
    buttonEv <- button "Click me"
    alertEvent
      (\val -> "The button was clicked; the event contained: " ++ show val)
      buttonEv

  tutorialSection $ do
    -- Attaching values to events part 1:
    --   Pure values that are in scope.
    --
    -- We modify `Event`s purely via their `Functor`
    -- interface (using `fmap` or `<$>`).
    let myval = 5

    buttonEv :: Event t () <- button "Submit (value in scope)"

    let buttonEvWithLocalVal :: Event t Int
        buttonEvWithLocalVal = fmap (\() -> myval) buttonEv
     -- buttonEvWithLocalVal = (\() -> myval) <$> buttonEv

    alertEvent
      (\val -> "The button was clicked; the event contained: " ++ show val)
      buttonEvWithLocalVal
