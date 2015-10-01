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

### Static Languages - Applicative

**Exercise**: Define a DSL for parsing routes in a web application.

- A route is something like `/foo/:bar/baz`.
- Routes are naturally typed: `/foo/:bar/baz` has content of type `String`, but `/foo/:bar/:baz` has content of type `(String, String)`.
- We should be able to apply static analysis to routes.

> Does the routing table contain redundant routes?

> Is a route a subroute of another?
