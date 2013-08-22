# Parsec

## Setup

    :set +m
    :set -XNoMonomorphismRestriction
    import Text.Parsec
    import Control.Monad

## Types

    :i Parsec ParsecT
    :i State
    
# Escaping the Monad

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
- `do { x ← … ; y ← … ; … }` - Combine several parse results

## Modifying Parsers
  
    :t many
    :t count
    :t between
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
    
## Handling Failure

    :t (<?>)
    :t try

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

## User State

Parsec(T) is a state monad, and will carry around user state as well as the parser''s internal state.

    :t getState
    :t setState
    :t modifyState

### Example - Non-Decreasing Sequences

    let
      atoi d   = read [d] :: Int
      number   = fmap atoi digit
      number'' = do
        n <- number
        modifyState $ max n
        return n
      monotone = many $ do
        max <- getState
        n <- number''
        if n < max
        then fail $ "Decreasing pair: " ++ show (max, n)
        else return n
        
    runParser monotone 0 "" "0123"
    runParser monotone 0 "" "0121"
    
### Example - Print Debugging

ParsecT allows us to lift monadic actions into the parser monad, so we can, for example, trace the steps taken during backtracking:

    import Control.Monad.Trans

    let
      char' c = lift (print c) >> char c
      chars = many $ (char' 'a') <|> (char' 'b')
      
    runParserT chars () "" "aabb"

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
        [ [ Infix (char '/' >> return (/)) AssocRight
          , Infix (char '*' >> return (*)) AssocRight ]
        , [ Infix (char '+' >> return (+)) AssocRight
          , Infix (char '-' >> return (-)) AssocRight ]
        , [ Prefix $ char '-' >> return negate ]
        ] atom
        
    parseTest numExpr "(1+2*4)/3"
      
## Language Definitions
	
    import Text.Parsec.Token

    :i GenLanguageDef GenTokenParser

    let langDef = haskellStyle -- { ... }

    let
       parser         = makeTokenParser langDef
       stringLiteral' = stringLiteral parser
       integer'       = integer parser
       float'         = float parser
       -- ...

    parseTest stringLiteral' "\"Test\""
    parseTest (lexeme' stringLiteral') "\"Test\" {-# Comment #-}"
