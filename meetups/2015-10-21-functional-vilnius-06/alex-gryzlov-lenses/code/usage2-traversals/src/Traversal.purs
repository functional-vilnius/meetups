module Traversal where

  import Prelude

  import Data.Either (either, Either(..))
  import Data.Maybe (maybe, Maybe(..))
  import Data.String (length)
  import Data.Tuple (Tuple(..))

  import Optic.Index
  import Optic.Core
  import Optic.Extended

  foo :: Array (Tuple (Either Int (Maybe String)) (Maybe Boolean))
  foo =
    [ Tuple (Left 1) Nothing
    , Tuple (Left 2) (Just true)
    , Tuple (Right (Just "three")) (Just true)
    , Tuple (Right (Just "four")) (Just false)
    , Tuple (Right Nothing) Nothing
    ]

  wat :: Tuple (Either Int (Maybe String)) (Maybe Boolean)
  wat = Tuple (Left 12) (Nothing)

  -- x # a # b # c # d = (((x # a) # b) # c) # d = d (c (b (a x)))
  -- `mapped` maps inside a functor
  -- `..`  = `<<<` = `compose`
  -- `.~`  = `put`
  -- `%~`  = `put with custom function`
  -- `++~` = `put with append`
  -- `*~` = `put with multiply`

  -- Append " wat" to all the justs in the rights.
  bar = foo # mapped.._1.._Right.._Just ++~ " wat"
  -- Multiply all Lefts by 5.
  baz = foo # mapped.._1.._Left *~ 5
  -- Convert all of the `Maybe Boolean` to `Maybe String`.
  quux = foo # mapped.._2.._Just %~ (\b -> if b then "yes" else "nope")
  -- Get the length of the strings in each just in the rights.
  wibble = foo # mapped.._1.._Right.._Just %~ length
  -- Attempt (successfully) to set index 1 of `foo` to `wat`
  wobble = foo # ix 1 .~ wat
  -- We attempt (unsuccessfully) to set index 100 of `foo` to `wat`
  -- N.B. This is a safe runtime index.
  wubble = foo # ix 100 .~ wat

  -- lens & prisms
  _1 :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
  _1 a2fb (Tuple a c) = (\b -> Tuple b c) <$> a2fb a

  _2 :: forall a b c. Lens (Tuple a b) (Tuple a c) b c
  _2 a2fb (Tuple a b) = Tuple a <$> a2fb b

  _Left :: forall a b c. Prism (Either a c) (Either b c) a b
  _Left = prism Left $ either Right (Left <<< Right)

  _Right :: forall a b c. Prism (Either a b) (Either a c) b c
  _Right = prism Right $ either (Left <<< Left) Right

  _Just :: forall a b. Prism (Maybe a) (Maybe b) a b
  _Just = prism Just $ maybe (Left Nothing) Right
