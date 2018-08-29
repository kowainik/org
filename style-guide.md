# Haskell Style Guide

> Haskell Style guide created and adopted by Kowainik organization.

## Goals

This style guide aims to achieve the following goals:

1. Make code **easier to understand:** ideas for a solution shouldn't be hidden
   behind the complex and obscure code.
2. Make code **easier to read:** code structure should be immediately apparent
   after looking at existing code.
3. Make code **easier to write:** developers should think about code formatting
   layout rules as less as possible. The style guide should answer every question
   regarding how to format a specific piece of code.
4. Make code **easier to maintain:** style guide can change, code changes all
   the time, and it's managed via version control systems which are sensitive
   to massive code changes. This style guide aims to reduce the burden of
   maintaining packages unless this conflicts with previous points.

## Main rule for every style guide

The general rule is to stick to the same coding style as is already used in the
file you're editing. If you must make significant stylistic changes, commit them
separately from functional changes, so that someone looking back through the
change logs can easily distinguish them.

## General rules

### Line length

Maximum allowed line length is _90 characters_. If your line doesn't fit into
this limit, try to split code into smaller pieces or break long lines over
multiple shorter lines.

### Whitespaces

**No trailing whitespaces** (use some tools to automatically cleanup trailing whitespaces).

Surround binary operators with a single space on either side.

Always add space after the comma.

### Indentation

Indent your code blocks with _4 spaces_.

Indent `where` blocks with _2 spaces_ (and always put `where` keyword on its own line).

```haskell
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
    | p x       = dropWhile p xs'
    | otherwise = xs

greet :: IO ()
greet = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting :: String -> String
    greeting name = "Hello, " ++ name ++ "!"
```

### Alignment

Use _comma-leading_ style for formatting module exports, lists, tuples, records, etc.

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

Align multiple lines according to the same separator like `::`, `=>`, `->`, `,`.

```haskell
foo :: Num a
    => a
    -> (a, a)
    -> Integer
    -> a

data Foo = Foo
    { fooBar  :: Bar
    , fooBaz  :: Baz
    , fooQuux :: Quux
    }
```

* _Indentation of a line should not depend on the length of any identifier in preceding lines._

Try to follow this rule inside function definition but without fanatism:

```haskell
-- + Good
foo = Foo
    <$> veryLongBar
    <*> veryLongBaz

-- - Bad
foo = Foo <$> veryLongBar
          <*> veryLongBaz

-- - Also bad
foo =
    Foo  -- no need to put constructor on the separate line and have a extra line
    <$> veryLongBar
    <*> veryLongBaz

```

Basically, it's often possible to join consequent lines without introducing
alignment dependency. Try not to span multiple short lines without need.

If an application must spawn multiple lines to fit within the maximum line
length, then write one argument on each line following the head, indented by one
level:

```haskell
veryLongProductionName
    firstArgumentOfThisFunction
    secondArgumentOfThisFunction
    (DummyDatatype withDummyField1 andDummyField2)
    lastArgumentOfThisFunction
```

### Naming

#### Functions and variables

+ **lowerCamelCase** for function and variable names.
+ **UpperCamelCase** for data types, typeclasses and constructors.

Try not to create new operators.

```haskell
-- What does this 'mouse operator' mean? :thinking_suicide:
(~@@^>) :: Functor f => (a -> b) -> (a -> c -> d) -> (b -> f c) -> a -> f d
```

Don't use short names like `n`, `sk`, `f` unless types of variables are general enough.

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

Don't introduce unnecessary long names for variables.

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

For readability reasons, don't capitalize all letters when using an
abbreviation as a part of a longer name. For example, write `HttpServer` instead
of `HTTPServer`.

Unicode symbols are allowed only in modules that already use unicode symbols. If
you create unicode name, you should also create non-unicode name as an alias.

#### Data types

Creating data types is extremely easy in Haskell. It's usually a good idea to
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

If the data type has only one constructor, then this data type name should be same as
constructor name (also applies to `newtype`).

```haskell
data User = User Int String
```

Field name for `newtype` should start with `un` or `run` prefix followed by type
name (motivated by this
[discussion](https://www.reddit.com/r/haskell/comments/7rl9hx/newtype_field_naming_getx_vs_runx/)).

* `run` for wrappers with monadic semantic.
* `un` for wrappers introduced for type safety.

```haskell
newtype Size = Size { unSize :: Int }
newtype App a = App { runApp :: ReaderT Context IO a }
```

Field names for record data type should start with the full name of the data type.

```haskell
data HealthReading = HealthReading
    { healthReadingDate        :: UTCTime
    , healthReadingMeasurement :: Double
    }
```

### Comments

Separate end-of-line comments from the code with _2 spaces_.

Try to write Haddock documentation for top-level functions, function arguments
and data type fields. For functions, the documentation should give enough
information to apply the function without looking at its definition.

Use block comment style for Haddock for multiple line comments.

It's allowed to align trailing Haddock comments but only if they fit the line
length limit.

```haskell
-- + Good
{- | Send a message on a socket. The socket must be in a connected
state. Returns the number of bytes sent. Applications are
responsible for ensuring that all data has been sent.
-}
send
    :: Socket      -- ^ Connected socket
    -> ByteString  -- ^ Data to send
    -> IO Int      -- ^ Bytes sent, doesn't exceed length of data to send

-- - Bad
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send
    :: Socket                         -- ^ Connected socket
    -> SendDataMessageBodyByteString  -- ^ Data to send
    -> IO Int                         -- ^ Bytes sent, doesn't exceed length of data to send
```

## Guideline for module formatting

Allowed tools for automatic module formatting:

* [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell)
  (with according [`.stylish-haskell.yaml`](https://github.com/kowainik/org/blob/master/.stylish-haskell.yaml))
* [`smuggler`](https://github.com/kowainik/smuggler)

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

You can put commonly-used language extensions into `default-extensions` in
`.cabal` file. Here is the list of extensions this style guide allows to put in
`.cabal` file:

```haskell
* ConstraintKinds
* DeriveGeneric
* GeneralizedNewtypeDeriving
* LambdaCase
* OverloadedStrings
* RecordWildCards
* ScopedTypeVariables
* StandaloneDeriving
* TupleSections
* TypeApplications
* ViewPatterns
```

### Export lists

Format export lists as below:

```haskell
module Map
       ( -- * Data type
         Map
       , Key

         -- * Update
       , insert
       , insertWith
       , alter
       ) where
```

Specifically:

1. Always write the explicit export list.
2. Use _7 spaces_ indentation for the export list (so that bracket is below the
   first letter in module name).
3. You can split the export list into sections.
4. Classes, data types and type aliases should be written before functions in
   each section.

### Imports

Always use explicit import lists or qualified imports. Try to use qualified
imports only if the import list is big enough or there are conflicts in names. This
makes the code more robust against changes in the libraries.

* __Exception:__ modules that only reexport stuff from other modules.

Imports should be grouped in the following order:

1. Non-qualified imports from Hackage packages.
2. Non-qualified imports from the current project.
3. Qualified imports from Hackage packages.
4. Qualified imports from the current project.

Put a blank line between each group of imports.

Put _2 blank lines_ after import section.

The imports in each group should be sorted alphabetically by module name.

### Data declaration

Align the constructors in a data type definition. Write documentation for every
constructor before constructor definition.

```haskell
data AppError
    -- | Internal application error.
    = InternalError IError
    -- | External integrated API error.
    | ExternalError EError
    deriving (Show, Eq)
```

Format records as follows:

```haskell
data User = User
    { userId   :: Int
    , userName :: Text
    } deriving (Show, Eq, Ord)
```

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

Constructor fields should be strict, unless there's an explicit reason to make
them lazy. This helps to avoid space leaks and gives the error instead of the warning
when you forgot to initialize some fields.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }

-- Bad
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

If function type signature is very long then place type of each argument under
its own line with respect to alignment.

```haskell
putValueInState
    :: forall env m .
       ( MonadMeasure m
       , MonadLog m
       , MonadReader env m
       , Has ServerKey env
       , MonadIO m
       )
    => UserState
    -> Maybe Int
    -> AppConfig
    -> (Int -> m ())
    -> m ()
```

If the line with argument names is too big, then put each argument on its own line
and separate it somehow from the body section.

```haskell
putValueInState
    userState
    mValue@(Just x)
    Config{..}        -- { must go after constructor without space
    valueModificator
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
{-# SPECIALIZE putTextLn :: Text -> IO () #-}
{-# INLINE putTextLn #-}
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

### If-then-else clauses

Generally, guards and pattern matches should be preferred over _if-then-else_
clauses, where possible. Short expressions should usually be put on a single line
(when line length allows it).

When writing non-monadic code (i.e. when not using `do`) where guards
and pattern matches can't be used, you can align _if-then-else_ clauses
like you would normal expressions:

```haskell
someFunction =
    if ...
    then ...
    else ...
```

Or you can align _if-then-else_ in different style inside lambdas.

```haskell
foo = bar >>= \qux -> if predicate qux
    then doOneThing
    else doOtherThing
```

When writing monadic code in `do`-blocks, add indentation before `then` and `else`:

```haskell
foo = do
    qux <- bar
    if predicate qux
        then doOneThing
        else doOtherThing
```

### Case expressions

The alternatives in a `case` expression should be indented like this:

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

Align the `->` arrows when it helps readability.

Prefer `-XLambdaCase` extension when you perform pattern matching over the last
argument of the function:

```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe v = \case
    Nothing -> v
    Just x  -> x
```

### let expressions

Put `let` before each variable inside a `do` block. In pure functions try to
avoid `let`. Instead, use `where`.

## General recommendations

Avoid abusing point-free style.  For example, this is hard to read:

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
