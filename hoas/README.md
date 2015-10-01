## `how to $ not write a programming language`

Writing a programming language is hard:

- Lex input to tokens
- Choose a sensible syntax representation
- Parse tokens to that representation
- Choose a sensible type system
- Use that type system to check the program
- Optimize the code
- Generate code
- Report sensible errors

We can _avoid_ writing a full programming language by building our terms _inside_ Haskell, as an _embedded domain-specific language_.

We want the terms of our language to

- Have familiar syntax
- Be composable
- Be typed
- Support static analysis

### Warm up - Monoids

Start with "composable" - a simple type of composition is given by the `Monoid` type class.

**Exercise**: Define a DSL for laying out textual documents on the screen.

- We know we need to compose two documents vertically.
- We also know we need an empty document.

So documents will form a `Monoid`.

- The free monoid over a type `a` is `[a]`.
- We also know we want any `String` to be made into a document.
- The free monoid `[String]` looks like a good candidate.

Let's define a type class for things which will behave like documents:

```haskell
class (IsString a, Monoid a) => Document a where
  indent :: Int -> a -> a
  beside :: a -> a -> a
```

- We can write documents which are polymorphic in the `Document` implementation.
 
```haskell
columns :: (Document a) => a
columns = column1 `beside` " " `beside` column2
  where
  column1 = "Haskell"    <> "C"          <> "Prolog"
  column2 = "Functional" <> "Imperative" <> "Logic"
```

- We can write an instance for `[String]`.
- To implement `beside`, we also need to track the _document width_.

```haskell
data PlainText = PlainText 
  { docWidth :: Int
  , docLines :: [String]
  }
```

- We can write other instances. 
- We can ignore structure or add structure.

### Static Languages - `Applicative`

**Exercise**: Define a DSL for parsing routes in a web application.

- A route is something like `/foo/:bar/baz`.
- Routes are naturally typed: `/foo/:bar/baz` has content of type `String`, but `/foo/:bar/:baz` has content of type `(String, String)`.
- We should be able to apply static analysis to routes.

> Does the routing table contain redundant routes?

> Is a route a subroute of another?

- If `Applicative` functors are models of typed function application in other languages, then perhaps an `Applicative` functor is what we need.
- The extra type argument will represent the type of the _content_ of the route.
- As before, we create a type class for interpretations of routes.

```haskell
class (IsString (f ()), Applicative f) => Route f where
  match :: f String
```

- We can add more members to our class if we want to support more operations.
- We can write routes which are polymorphic in their interpretation.

```haskell
route :: (Route f) => f (String, String)
route (,) <$> ("foo" *> match) <*> match 
```

- We can interpret routes as parsers.
- We can also interpret routes by pretty-printing them, or by performing static analysis.
- Applicative functors are very good for static analysis, because the _shape_ of a computation is fixed.

### Dynamic Languages - `Monad`

**Exercise**: Define a DSL to give users restricted access to the filesystem.

- This time, we want users to be able to write _sequences of commands_, where commands depend on previous results.
- Data dependencies in our language suggest a `Monad`.
- This should not be surprising - `Monad`s are models of function composition in other languages.
- With more expressive power, we lose analytical power.
- As before, define a class for the filesystem commands we want to expose.

```haskell
class (Monad m) => MonadFileSystem m where
  cd :: FilePath -> m ()
  ls :: m [(FilePath, FileType)]
  cat :: [FilePath] -> m String
```

- We can interpret commands directly in `IO`.
- Users only have access to the commands we provide - the type system restricts access.
- We have the full power of Haskell to write shell scripts.

```haskell
joinFiles :: (MonadFileSystem m) => m String
joinFiles = do
  files <- ls
  cat (map fst files)
```

- This is the approach taken by the `mtl` library. Classes like `MonadState` model features of DSLs we might build.
- Type classes can be composed to combine various features.
- We can also interpret commands with a fake filesystem for testing!

```haskell
data FakeFS a = FakeFS (Zipper -> (a, Zipper))

instance MonadFileSystem FakeFS
```

### Models of Lambda Calculus - HOAS

**Exercise**: Define a DSL which embeds the full simply-typed lambda calculus.

- We can craft a functional programming language using a set of core functions, but allow users all the abstraction and composition afforded by the STLC.
- Again, we define a class, with a trick:

```haskell
class HOAS f where
  ($$) :: f (a -> b) -> f a -> f b
  lam :: (f a -> f b) -> f (a -> b)
```

- Compare `HOAS` with `Applicative`: we have `<*>` but not `pure`. `lam` looks like an inverse for `<*>`.
- We embed the (typed!) Haskell function space into our language.
- We can interpret `HOAS` as regular Haskell values.
- We can give other interpretations.
- We can build a typed, purely-functional compile-to-JS language in ~20 lines this way.
