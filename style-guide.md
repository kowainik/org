# Haskell Style Guide

> Style guide used in Kowainik.

This document is a collection of the practices gained
by working on the commercial and free open source Haskell libraries and applications.

## Style guide goals

The purpose of this document is to help developers and people working with
Haskell sources to have smoother experience while dealing with code in different
aspects. So, this style guide tries to provide these advantages by defining the
following goals:

1. Make code **easier to understand:** the ideas for solutions should not be hidden
   behind the complex and obscure code.
2. Make code **easier to read:** code arrangement should be immediately apparent
   after looking at the existing code.
3. Make code **easier to write:** developers should think about code formatting
   rules as little as possible. The style guide should answer every question
   concerning how to format a specific piece of code.
4. Make code **easier to maintain:** this style guide aims to reduce the burden
   of maintaining packages using version control systems unless this conflicts
   with the previous points.

## Main rule when working with the existing source code

The general rule is to stick to the same coding style that is already used in the
file you are editing. If you must make significant style modifications, then commit them
independently from the functional changes so that someone looking back through the
changelog can easily separate them.

## Indentation

Indent the code blocks with _4 spaces_.

Indent `where` keyword with _2 spaces_ and always put `where` keyword on its own line.

```haskell
showSign :: Int -> String
showSign n
    | n == 0    = "Zero"
    | n < 0     = "Negative"
    | otherwise = "Positive"

greet :: IO ()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ greeting name
  where
    greeting :: String -> String
    greeting name = "Hey " ++ name ++ "!"
```

## Line length

Maximum allowed line length is _90 characters_. If your line doesn't fit into
this limit, try to split code into smaller pieces or break long lines over
multiple shorter ones.

## Whitespaces

**No trailing whitespaces** (use some tools to automatically cleanup trailing
whitespaces).

Surround binary operators with a single space on either side.

## Alignment

Use _comma-leading_ style for formatting module exports, lists, tuples, records, etc.

```haskell
answers :: [Maybe Int]
answers =
    [ Just 42
    , Just 7
    , Nothing
    ]
```

If function definition doesn't fit the line limit then align multiple lines
according to the same separator like `::`, `=>`, `->`.

```haskell
printQuestion
    :: Show a
    => Text  -- ^ Question text
    -> [a]   -- ^ List of available answers
    -> IO ()
```

Align records with every field on a separate line with leading commas.

```haskell
data Foo = Foo
    { fooBar  :: Bar
    , fooBaz  :: Baz
    , fooQuux :: Quux
    } deriving (Eq, Show, Generic)
      deriving anyclass (FromJSON, ToJSON)
```

Align sum types with every constructor on its own line with leading `=` and `|`.

```haskell
data TrafficLight
    = Red
    | Yellow
    | Green
    deriving (Eq, Ord, Enum, Bounded, Show, Read)
```

+ **Indentation of a line should not depend on the length of any identifier in preceding lines.**

Try to follow the above rule inside function definition but without fanatism:

```haskell
-- + Good
createFoo = Foo
    <$> veryLongBar
    <*> veryLongBaz

-- - Bad
createFoo = Foo <$> veryLongBar
                <*> veryLongBaz

-- - Meh
createFoo =
    Foo  -- no need to put constructor on the separate line and have an extra line
    <$> veryLongBar
    <*> veryLongBaz
```

Basically, it is often possible to join consequent lines without introducing
alignment dependency. Try not to span multiple short lines without need.

If a function application must spawn multiple lines to fit within the maximum line
length, then write one argument on each line following the head, indented by one
level:

```haskell
veryLongProductionName
    firstArgumentOfThisFunction
    secondArgumentOfThisFunction
    (DummyDatatype withDummyField1 andDummyField2)
    lastArgumentOfThisFunction
```

## Naming

### Functions and variables

+ **lowerCamelCase** for function and variable names.
+ **UpperCamelCase** for data types, typeclasses and constructors.

Try not to create new operators.

```haskell
-- What does this 'mouse operator' mean? :thinking_suicide:
(~@@^>) :: Functor f => (a -> b) -> (a -> c -> d) -> (b -> f c) -> a -> f d
```

Do not use short names like `a`, `par`, `g` unless types of variables are general enough.

```haskell
-- + Good
mapSelect :: forall a . (a -> Bool) -> (a -> a) -> (a -> a) -> [a] -> [a]
mapSelect test ifTrue ifFalse = go
  where
    go :: [a] -> [a]
    go [] = []
    go (x:xs) = if test x
        then ifTrue  x : go xs
        else ifFalse x : go xs

-- - Bad
mapSelect :: forall a . (a -> Bool) -> (a -> a) -> (a -> a) -> [a] -> [a]
mapSelect p f g = go
  where
    go :: [a] -> [a]
    go [] = []
    go (x:xs) = if p x
        then f x : go xs
        else g x : go xs
```

Do not introduce unnecessary long names for variables.

```haskell
-- + Good
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- - Bad
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map function (firstElement:remainingList) =
    function firstElement : map function remainingList
```

For readability reasons, do not capitalize all letters when using an
abbreviation as a part of a longer name. For example, write `HttpServer` instead
of `HTTPServer`.

Unicode symbols are allowed only in modules that already use unicode symbols. If
you create unicode name, you should also create a non-unicode one as an alias.

### Data types

Creating data types is extremely easy in Haskell. It is usually a good idea to
introduce custom data type (enum or `newtype`) instead of using commonly used
data type (like `Int` or `String`).

`type` aliases are allowed only for specializing general types:

```haskell
-- + Good
data StateT s m a
type State s = StateT s Identity

-- - Bad
type Size = Int
```

Use the data type name as the constructor name for `data` with single
constructor and `newtype`.

```haskell
data User = User Int String
```

Field name for `newtype` must start with the `un` prefix followed by the type name.

```haskell
newtype Size = Size { unSize :: Int }
newtype App a = App { unApp :: ReaderT Context IO a }
```

Field names for the record data type should start with the full name of the data type.

```haskell
-- + Good
data HealthReading = HealthReading
    { healthReadingDate        :: UTCTime
    , healthReadingMeasurement :: Double
    }
```

It is acceptable to use an abbreviation for the field prefixes if the data type name is
too long.

```haskell
-- + Acceptable
data HealthReading = HealthReading
    { hrDate        :: UTCTime
    , hrMeasurement :: Double
    }
```

## Comments

Separate end-of-line comments from the code with _2 spaces_.

```haskell
newtype Measure = Measure
    { unMeasure :: Double  -- ^ See how 2 spaces separate this comment
    }
```

Write [Haddock documentation](https://github.com/aisamanra/haddock-cheatsheet/blob/master/haddocks.pdf)
for the top-level functions, function arguments
and data type fields. The documentation should give enough
information to apply the function without looking at its definition.

Use block comment style (`{- |` and `-}`) for Haddock for multiple line comments.

```haskell
-- + Good
{- | Example of multi-line block comment which is very long
and doesn't fit single line.
-}
foo :: Int -> [a] -> [a]

-- + Also good
-- | Single-line short comment.
foo :: Int -> [a] -> [a]

-- ~ Bad
-- | Example of multi-line block comment which is very long
-- and doesn't fit single line.
foo :: Int -> [a] -> [a]
```

For commenting function arguments, data type constructors and their fields,
you are allowed to use end-of-line Haddock comments if they fit line length
limit. Otherwise, use block style comments. It is _allowed_ to align end-of-line
comments with each other. But it is _forbidden_ to use comments of different
styles for the function arguments, data type constructors, and fields.

```haskell
-- + Good
{- | 'replicate' @n x@ returns list of length @n@ with @x@ as the value of
every element. This function is lazy in its returned value.
-}
replicate
    :: Int  -- ^ Length of returned list
    -> a    -- ^ Element to populate list
    -> [a]

-- - Bad
{- | 'replicate' @n x@ returns list of length @n@ with @x@ as the value of
every element. This function is lazy in its returned value.
-}
replicate
    :: Int  -- ^ Length of returned list
    {- | Element to populate list -}
    -> a
    -> [a]
```

If possible include typeclasses laws and function usage examples into the
documentation.

```haskell
{- | The class of semigroups (types with an associative binary operation).

Instances should satisfy the associativity law:

* @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
-}
class Semigroup a where
    (<>) :: a -> a -> a


{- | The 'intersperse' function takes a character and places it
between the characters of a 'Text'.

>>> T.intersperse '.' "SHIELD"
"S.H.I.E.L.D"
-}
intersperse :: Char -> Text -> Text
```

## Guideline for module formatting

Allowed tools for automatic module formatting:

* [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell)
  (with according [`.stylish-haskell.yaml`](https://github.com/kowainik/org/blob/master/.stylish-haskell.yaml)):
  for formatting import section and alignment.
* [`smuggler`](https://github.com/kowainik/smuggler): for removing unused imports.

### {-# LANGUAGE #-}

Put `OPTIONS_GHC` pragma before `LANGUAGE` pragmas in the separate section. Write
each `LANGUAGE` pragma on its own line, sort them alphabetically and align by
max width among them.

```haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
```

You can put commonly-used language extensions into `default-extensions` in the
`.cabal` file. Here is the list of extensions this style guide allows to put in there:

```haskell
ConstraintKinds
DeriveGeneric
GeneralizedNewtypeDeriving
InstanceSigs
KindSignatures
LambdaCase
OverloadedStrings
RecordWildCards
ScopedTypeVariables
StandaloneDeriving
TupleSections
TypeApplications
ViewPatterns
```

### Export lists

Use the following rules to format export section:

1. **Always write** the explicit export list.
2. Use _7 spaces_ indentation for the export list (so that bracket is below the
   first letter in module name).
3. You can split the export list into sections. Use Haddock to assign names to
   these sections.
4. Classes, data types and type aliases should be written before functions in
   each section.

```haskell
module Map
       ( -- * Data type
         Map
       , Key
       , empty

         -- * Update
       , insert
       , insertWith
       , alter
       ) where
```

### Imports

Always use explicit import lists or qualified imports. Try to use qualified
imports only if the import list is big enough or there are conflicts in names. This
makes the code more robust against changes in the libraries.

* __Exception:__ modules that only reexport the whole modules.

Imports should be grouped in the following order:

1. Non-qualified imports from Hackage packages.
2. Non-qualified imports from the current project.
3. Qualified imports from Hackage packages.
4. Qualified imports from the current project.

Put a blank line between each group of imports.

Put _2 blank lines_ after import section.

The imports in each group should be sorted alphabetically by module name.

```haskell
module MyProject.Foo
       ( Foo (..)
       ) where

import Control.Exception (catch, try)
import Data.Traversable (for)

import MyProject.Ansi (errorMessage, infoMessage)

import qualified Data.Aeson as Json
import qualified Data.Text as Text

import qualified MyProject.BigModule as Big


data Foo
...
```

### Data declaration

Refer to the [Alignment section](#alignment) to see how to format data type
declarations.

Records for data types with multiple constructors are forbidden.

```haskell
-- - Bad
data Foo
    = Bar { bar1 :: Int, bar2 :: Double }
    | Baz { baz1 :: Int, baz2 :: Double, baz3 :: Text }

-- + Good
data Foo
    = FooBar Bar
    | FooBaz Baz

data Bar = Bar { bar1 :: Int, bar2 :: Double }
data Baz = Baz { baz1 :: Int, baz2 :: Double, baz3 :: Text }

-- + Also good
data Foo
    = Bar Int Double
    | Baz Int Double Text
```

### Deriving

Type classes in the deriving section should be always surrounded by parentheses.
Don't derive typeclass without need.

Use [`-XDerivingStrategies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-strategies)
extension for `newtype`s to explicitly specify the way you want to derive type classes:

```haskell
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Id a = Id { unId :: Int }
    deriving stock    (Generic)
    deriving newtype  (Eq, Ord, Show, Hashable)
    deriving anyclass (FromJSON, ToJSON)
```

Constructor fields should be strict unless there is an explicit reason to make
them lazy. This helps to avoid space leaks and gives the error instead of the warning
when you forgot to initialize some fields.

```haskell
-- + Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }

-- - Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

### Function declaration

All top-level functions _must_ have type signatures.

All functions inside `where` _must_ have type signatures. Explicit type
signatures help to avoid cryptic type errors.

> You might need `-XScopedTypeVariables` extensions to write polymorphic types
> of functions inside `where`.

Surround `.` after `forall` in type signatures with spaces.

```haskell
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
```

If the function type signature is very long then place type of each argument under
its own line with respect to alignment.

```haskell
sendEmail
    :: forall env m .
       ( MonadLog m
       , MonadEmail m
       , WithDb env m
       )
    => Email
    -> Subject
    -> Body
    -> Template
    -> m ()
```

If the line with argument names is too big, then put each argument on its own line
and separate it somehow from the body section.

```haskell
sendEmail
    toEmail
    subject@(Subject subj)
    body
    Template{..}  -- default body variables
  = do
    <code goes here>
```

In other cases place `=` sign on the same line where function definition is.

Put operator fixity before operator signature:

```haskell
-- | Flipped version of '<$>'.
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
```

Put pragmas immediately following the function they apply to.

```haskell
-- | Lifted version of 'T.putStrLn'.
putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . Text.putStrLn
{-# INLINE putTextLn #-}
{-# SPECIALIZE putTextLn :: Text -> IO () #-}
```

In case of data type definitions, you must put the pragma before
the type it applies to. Example:

```haskell
data TypeRepMap (f :: k -> Type) = TypeRepMap
    { fingerprintAs :: {-# UNPACK #-} !(PrimArray Word64)
    , fingerprintBs :: {-# UNPACK #-} !(PrimArray Word64)
    , trAnys        :: {-# UNPACK #-} !(Array Any)
    , trKeys        :: {-# UNPACK #-} !(Array Any)
    }
```

### if-then-else clauses

Prefer guards over _if-then-else_ where possible.

```haskell
-- + Good
showParity :: Int -> Bool
showParity n
    | even n    = "even"
    | otherwise = "odd"

-- - Meh
showParity :: Int -> Bool
showParity n =
    if even n
    then "even"
    else "odd"
```

When writing monadic code in `do`-blocks where guards can not be used,
add one indentation level before `then` and `else`:

```haskell
choose
    :: Text  -- ^ Question text.
    -> NonEmpty Text  -- ^ List of available options.
    -> IO Text  -- ^ The chosen option.
choose question choices = do
    printQuestion question choices
    answer <- prompt
    if null answer
        then pure (head choices)
        else pure answer
```

In the code outside `do`-blocks you can align _if-then-else_ clauses
like you would normal expressions:

```haskell
shiftInts :: [Int] -> [Int]
shiftInts = map $ \n -> if even n then n + 1 else n - 1
```

### Case expressions

Align the `->` arrows in the alternatives when it helps readability.

```haskell
-- + Good
firstOrDefault :: [a] -> a -> a
firstOrDefault list def = case list of
    []  -> def
    x:_ -> x

-- - Bad
foo :: IO ()
foo = getArgs >>= \case
    []                      -> do
        putStrLn "No arguments provided"
        runWithNoArgs
    firstArg:secondArg:rest -> do
        putStrLn $ "The first argument is " ++ firstArg
        putStrLn $ "The second argument is " ++ secondArg
    _                       -> pure ()
```

Prefer `-XLambdaCase` extension when you perform pattern matching over the last
argument of the function:

```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe v = \case
    Nothing -> v
    Just x  -> x
```

### let expressions

Write `let`-bindings on the new line:

```haskell
isLimitedBy :: Integer -> Natural -> Bool
isLimitedBy n limit =
    let intLimit = toInteger limit
    in n <= intLimit
```

Put `let` before each variable inside a `do` block. In pure functions try to
avoid `let`. Instead, use `where`.

## General recommendations

Try to split code into separate modules.

Avoid abusing point-free style. For example, this is hard to read:

```haskell
f = (g .) . h  -- :(
```

Prefer `pure` over `return`.

Code should be compilable with the following ghc options without warnings:

* `-Wall`
* `-Wincomplete-uni-patterns`
* `-Wincomplete-record-updates`
* `-Wcompat`
* `-Widentities`
* `-Wredundant-constraints`
* `-Wmissing-export-lists`
* `-Wpartial-fields`

Enable `-fhide-source-paths` and `-freverse-errors` for cleaner compiler output.

Use `-XApplicativeDo` in combination with `-XRecordWildCards` to prevent
position-sensitive errors.
