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

### Warm up - Monoids

Start with "composable" - a simple type of composition is given by the `Monoid` type class.

**Exercise**: Define a DSL for laying out textual data on the screen.
