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
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- Library imports
import           Control.Monad.IO.Class
import           Data.Default (def)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
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


showText :: (Show a) => a -> Text
showText = T.pack . show


dynButton :: (MonadWidget t m) => Dynamic t Text -> m (Event t ())
dynButton textDyn = do
  (e, _) <- el' "button" (dynText textDyn)
  return $ domEvent Click e


main :: IO ()
main = mainWidget myWidgets

myWidgets :: forall t m . (MonadWidget t m) => m ()
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

  tutorialSection $ do
    -- Dynamic values.
    text "Enter your name: "

    nameInput :: TextInput t <- textInput def

    let nameDyn :: Dynamic t Text
        nameDyn = _textInput_value nameInput

    text " Your name is: "
    dynText nameDyn

    divClass "age" $ do
      text "And age: "
      ageDyn <- _textInput_value <$> textInput def

      -- `Dynamic`s are Applicative; that way you can combine them.
      dynText $
        (\n a -> n <> " is " <> a) <$> nameDyn <*> ageDyn

    -- There are lots of other interesting Events and
    -- Dynamics on `TextInput` widgets, for example:
    --   _textInput_value :: Dynamic t Text    -- contents
    --   _textInput_keypress :: Event t Int    -- when a key is pressed
    --   _textInput_hasFocus :: Dynamic t Bool

    -- Having this pure `TextInput` type to pass around,
    -- containing various Events and Dynamics other parts of
    -- the code can react to, is a nice and composable design
    -- approach to GUI programming.

  tutorialSection $ do
    -- Attaching values to events part 2:
    --   Attaching the current value of a Dynamic value
    --   to an Event, when that event happens.
    --
    -- Example: We have a text box (whose events happen
    -- on every key press), but we want an Event to occur
    -- only when the submit button is clicked, and we want
    -- that event to carry the current value of the text box.
    text "What food would you like to order? "
    foodInputDyn <- _textInput_value <$> textInput def
    buttonEv <- button "Order"

    let foodOrderEv :: Event t Text
        foodOrderEv = tagPromptlyDyn foodInputDyn buttonEv

    alertEvent
      (\food -> "Ordered: " ++ T.unpack food)
      foodOrderEv

  tutorialSection $ do
    -- Making Dynamic values from events.
    --
    -- Dynamics contain the "state" of reflex FRP.
    -- Any input/output of information happens via Events.
    -- We declaratively express how events change our state
    -- by using `fold`-like functions.
    -- Recall
    --   foldl   :: (b -> a -> b) -> b -> [a] -> b
    -- What we use now looks very similar:
    --   foldDyn :: (a -> b -> b) -> b -> Event a -> m (Dynamic b)

    -- Helper function:
    let -- Makes a button with a number as label; the event
        -- generated by the label contains that number.
        numberButton :: Int -> m (Event t Int)
        numberButton n = do
          buttonEv <- button (showText n)
          let numberEv = fmap (\() -> n) buttonEv
          return numberEv

    button1Ev <- numberButton 1
    button2Ev <- numberButton 2
    button3Ev <- numberButton 3

    -- Now we want to create an event that happens when
    -- any of the buttons are clicked.
    -- Because multiple Events can in general happen at
    -- the same time (in practice we can't click that fast),
    -- we use `leftmost` to disambiguate which one to prefer.
    let anyButtonEv :: Event t Int
        anyButtonEv = leftmost [button1Ev, button2Ev, button3Ev]

    sumDyn <- foldDyn (+) 0 anyButtonEv

    dynText (showText <$> sumDyn)


  tutorialSection $ do
    -- Dependency loops.
    --
    -- So far our data flow was always one-directional
    -- (e.g. textbox to displayed text).
    -- A Monad captures this well with sequential `do`
    -- notation, where values can syntactically only refer
    -- to earlier declared values.
    -- But sometimes two Dynamic values must depend on
    -- each other -- a loop!
    -- This is possible using MonadFix and `rec` notation
    -- (`RecursiveDo`).

    rec
      textDyn <- _textInput_value <$> textInput def{
                   _textInputConfig_setValue = clearEvent
                 }
      buttonEv <- dynButton (("Clear: " <>) <$> textDyn)
      let clearEvent = const "" <$> buttonEv

      -- Notice how `textDyn` depends on `clearEvent`
      -- defined *below* it. `rec` makes it possible.

    return ()
