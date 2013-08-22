# A Very Quick Tour of Parsec

For the LA Haskell User Group

    :set +m
    :set -XNoMonomorphismRestriction
    import Text.Parsec
    import Control.Monad

## Types

    :i Parsec ParsecT

    :t runParser
    :t runParsecT
    :t parse
    :t parseTest

    parseTest (oneOf "abc") "a"
    parseTest (oneOf "abc") "ab"
    parseTest (oneOf "abc") "d"

## Simple Parsers

    :t oneOf
    :t letter
    :t digit
    :t space

## Do-notation

- `fmap f p` - Parse using `p` and apply `f` to the result
- `p >>= q` - Parse using `p` first, and then using `q`, possibly using the result of `p`.

## Modifying Parsers
  
    :t many
    :t chainl
    :t chainr

### Example - Parsing Phone Book Entries

    let tel = do 
      g1 <- count 3 digit;
      char '-'
      g2 <- count 3 digit
      char '-'
      g3 <- count 4 digit
      return (g1, g2, g3) 

    let name = many1 letter

    let fullName = do
      last <- name
      char ','
      space
      first <- name
      return (first, last)

    let phoneBook = do
      (first, last) <- fullName
      space
      char '('
      (g1, g2, g3) <- tel
      char ')'
      return (first, last, g1, g2, g3)

    parseTest phoneBook "Freeman, Phillip (555-555-5555)"

## Combining Parsers

    :t sepBy
    :t (<|>)

### Example - Handling Multiple First Names

    let nameSep = try $ do
      space
      notFollowedBy (char '(')

    let fullName = do
      last <- name
      char ','
      space
      rest <- sepBy1 name nameSep
      return $ rest ++ [last]

    let phoneBook = do
      names <- fullName
      space
      char '('
      (g1, g2, g3) <- tel
      char ')'
      return (names, g1, g2, g3)

    parseTest phoneBook "Freeman, Phillip Antony (555-555-5555)"

## Handling Failure

    :t (<?>)
    :t try

## State

    :t getState
    :t setState
    :t modifyState

### Example - Matching Parentheses

    let openParen = do
      char '('
      modifyState succ
      return ()

    let closeParen = do
      char ')'
      s <- getState
      (guard $ s > 0) <?> "Unexpected )"
      modifyState pred
      return ()

    let expr = do
      many $ choice [openParen, closeParen]
      s <- getState
      (guard $ s == 0) <?> "Expected )"
      return ()

    runPT expr 0 "" "(()())"

## Expression Parsers

    import Text.Parsec.Expr

    :t buildExpressionParser
    :i Operator OperatorTable Assoc

### Example - Parsing Numeric Expressions 

    let 
      number = fmap read $ many1 digit
      bracket = between (char '(') (char ')') numExpr
      atom = bracket <|> number
      numExpr = buildExpressionParser
        [ [ Infix (do { char '/'; return (/) }) AssocRight
          , Infix (do { char '*'; return (*) }) AssocRight ]
        , [ Infix (do { char '+'; return (+) }) AssocRight
          , Infix (do { char '-'; return (-) }) AssocRight ]
        , [ Prefix $ do { char '-'; return negate } ]
        ] atom
      
## Language Definitions
	
    import Text.Parsec.Token

    :i GenLanguageDef GenTokenParser

    let langDef = haskellStyle -- { ... }

    let
       parser = makeTokenParser langDef
       stringLiteral' = stringLiteral parser
       integer' = integer parser
       float' = float parser
       -- ...

    parseTest stringLiteral' "\"Test\""
