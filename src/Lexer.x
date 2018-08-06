{
{-|
Module      : Lexer
Description : Lexical analysis tool for the Rockstar programming language.
Copyright   : (c) Andrés Ignacio Torres, 2018
License     : MIT
Maintainer  : andresitorresm+github@gmail.com
Stability   : stable

This module allows for the main program to scan a given file,
analyse its content and determine which allowed tokens exist. It
also includes functions that 'pretty-print' the found tokens,
valid or not, to the standard output.

Note: You can find the current specification of the Rockstar 
      programming language, made by Dylan Beattie at the following 
      link:     
            https://github.com/dylanbeattie/rockstar
-}

module Lexer (alexScanTokens, printScannedTokens) where
}

-- We use the "posn" wrapper to keep track of a token's position
-- over the content.
%wrapper "posn"

-- Character sets used in the regular expressions
$digit = 0-9
$alpha = [a-zA-Z] 
$lowercase = [a-z]
$uppercase = [A-Z]
$space = \32
$lb = \10
$graphic    = $printable # $white

-- Macros used in the regular expressions
@string     = \" ($graphic # \")* \"
@uppercaseword = $space $uppercase $lowercase+

-- Token producing rules (regular expressions)
tokens :-
    -- Comments and linebreaks
    -- Note that comments are parsed as linebreaks (for an explanation, read the implementation details)
    "(" .* ")"                                                  { \p s -> TokenPosn (LineBreak) p }
    $lb+                                                        { \p s -> TokenPosn (LineBreak) p }

    -- Common variables
       "my" $space $lowercase+                                     
    |  "a" $space $lowercase+                                      
    |  "an" $space $lowercase+                                     
    |  "the" $space $lowercase+                                    
    |  "my" $space $lowercase+                                     
    |  "your" $space $lowercase+                                { \p s -> TokenPosn (CommonVariable s) p }

    -- Pronouns
      "it"                                                        
    | "he"                                                        
    | "she"                                                       
    | "him"                                                       
    | "her"                                                       
    | "them"                                                      
    | "they"                                                    { \p s -> TokenPosn (Pronoun s) p }
    
    -- Types
    "mysterious"                                                { \p s -> TokenPosn (Mysterious) p }
    "null" | "nothing" | "nowhere" | "nobody"                   { \p s -> TokenPosn (Null) p }
    "boolean"                                                   { \p s -> TokenPosn (Boolean) p }

    -- Boolean literals
    "right" | "yes" | "ok" | "true"                             { \p s -> TokenPosn (BooleanTrue) p }
    "wrong" | "no" | "lies" | "false"                           { \p s -> TokenPosn (BooleanFalse) p }
    "maybe" | "definitely maybe"                                { \p s -> TokenPosn (BooleanMaybe) p }

    -- Numeric literals
    -- Note that integers and floats have each their own, separate token (for an explanation, read the implementation details)
    $digit+                                                     { \p s -> TokenPosn (NumberInt (read s :: Int)) p }
    $digit+ "." $digit+                                         { \p s -> TokenPosn (NumberFloat (read s :: Float)) p }
    @string                                                     { \p s -> TokenPosn (StringLiteral (removeDoubleQuotes s)) p } 

    -- Assignments
    "Put" | "put"                                               { \p s -> TokenPosn (Put) p }
    "Into" | "into"                                             { \p s -> TokenPosn (Into) p }    

    -- Increments and decrements
    "Build" | "build"                                           { \p s -> TokenPosn (Build) p }
    "Knock" | "knock"                                           { \p s -> TokenPosn (Knock) p }
    "up"                                                        { \p s -> TokenPosn (Up) p }
    "down"                                                      { \p s -> TokenPosn (TokDown) p }

    -- Arithmetic
    "plus" | "with"                                             { \p s -> TokenPosn (Plus) p}
    "minus" | "without"                                         { \p s -> TokenPosn (Minus) p}
    "times" | "of"                                              { \p s -> TokenPosn (Times) p}
    "over" | "by"                                               { \p s -> TokenPosn (Over) p}

    -- Poetic tokens
    "is" | "was" | "were"                                       { \p s -> TokenPosn (Is) p }        
    "says"                                                      { \p s -> TokenPosn (Says) p }        
    "not"                                                       { \p s -> TokenPosn (Not) p }
    "ain't"                                                     { \p s -> TokenPosn (Aint) p }

    -- Comparisons
    "higher" | "greater" | "bigger" | "stronger"                { \p s -> TokenPosn (Greater) p }
    "lower" | "less" | "smaller" | "weaker"                     { \p s -> TokenPosn (Less) p }
    "than"                                                      { \p s -> TokenPosn (Than) p }
    "as high as" | "as great as" | "as big as" | "as strong as" { \p s -> TokenPosn (GreaterEq) p }
    "as low as" | "as little as" | "as small as" | "as weak as" { \p s -> TokenPosn (LessEq) p } 

    -- I/O flow
    "Listen"                                                    { \p s -> TokenPosn (Listen) p }
    "Say" | "Shout" | "Whisper" | "Scream"                      { \p s -> TokenPosn (Say) p }

    -- Control flow
    "If"                                                        { \p s -> TokenPosn (If) p }
    "Else"                                                      { \p s -> TokenPosn (Else) p }    
    "While"                                                     { \p s -> TokenPosn (While) p }
    "Until"                                                     { \p s -> TokenPosn (Until) p }
    "break" | "Break it down"                                   { \p s -> TokenPosn (Break) p }
    "continue" | "Take it to the top"                           { \p s -> TokenPosn (Continue) p }

    -- Functions
    "takes"                                                     { \p s -> TokenPosn (Takes) p }
    "and"                                                       { \p s -> TokenPosn (And) p }
    "Give back"                                                 { \p s -> TokenPosn (GiveBack) p }
    "taking"                                                    { \p s -> TokenPosn (Taking) p }        
    ","                                                         { \p s -> TokenPosn (Comma) p}


    -- Proper variables
    -- Note that proper variables such as "Mister Sandman" are parsed as two separate proper variables.
    -- For more information, read the known issues.
    $uppercase $lowercase+                                      { \p s -> TokenPosn (ProperVariable s) p }


    -- Whitespace to be ignored
    $white+                                                     ;

    -- Finally, a "match-it-all" character to report errors
    .                                                           { \p s -> TokenPosn (Unknown s) p }

{

{-
    DATA TYPES
-}

-- A data type to keep the tokens by themselves, as well as additional
-- information.
data Token
    -- Variables
    = ProperVariable String
    | CommonVariable String
    | Pronoun String

    -- Types and literals
    | Mysterious
    | Null
    | Boolean
    | BooleanTrue
    | BooleanFalse
    | BooleanMaybe
    | NumberInt Int
    | NumberFloat Float
    | StringLiteral String

    -- Assignments
    | Put
    | Into

    -- Increment/Decrement
    | Build
    | Up
    | Knock
    | TokDown

    -- Arithmetic
    | Plus
    | Minus
    | Times
    | Over

    -- Equality and initialization
    | Is
    | Not
    | Aint

    -- Poetic string literal
    | Says

    -- Comparison
    | Greater
    | GreaterEq
    | LessEq
    | Less
    | Than

    -- I/O flow
    | Listen
    | Say

    -- Control flow
    | If
    | Else
    | While
    | Until
    | Break
    | Continue

    -- Functions
    | Takes
    | And
    | Taking
    | GiveBack
    | Comma

    -- Other tokens
    | Unknown String
    | LineBreak
    deriving (Eq, Show)

-- A data type in order to keep a token and their associated
-- position within the file (offset, line and column)
data TokenPosn 
    = TokenPosn Token AlexPosn
    deriving (Eq)

-- We define a "pretty" way of showing unknown tokens, as well as valid tokens
instance Show TokenPosn where
    show (TokenPosn (Unknown s)  (AlexPn _ l c)) = 
        "Unknown token '" ++ s ++ "' on line " ++ show l ++ ", column " ++ show c 
    show (TokenPosn t (AlexPn _ l c)) = 
        show t ++ " (" ++ show l ++ ", " ++ show c ++ ")"  

{-
    MAIN PRINTING FUNCTIONS
    IO Functions to pretty-print the found tokens
-}

-- | The 'printScannedTokens' function receives a list of 
--  scanned tokens and prints (only) all the unknown tokens,
--  if they exist, or all the tokens.
printScannedTokens :: [TokenPosn] -> IO ()
printScannedTokens tokens
    | hasUnknownTokens tokens = printUnknownTokens tokens
    | otherwise = printTokens tokens

-- | The 'printTokens' function receives a list of tokens
--   and pretty-prints them.
printTokens :: [TokenPosn] -> IO ()
printTokens = putStr . formatGroupedTokens . deleteExtraLinebreaks . groupTokensByLine 

-- | The 'printTokens' function receives a list of tokens, drops
--   all but the unknown tokens, and pretty-prints them.
printUnknownTokens :: [TokenPosn] -> IO ()
printUnknownTokens = putStr . formatErrors . takeUnknownTokens

{-
    AUXILIARY PRINTING FUNCTIONS
    Miscelaneous functions to pretty-print the tokens
-}

-- | The 'isUnknownToken' takes a token and determines
--   if it's an Unknown token or not 
isUnknownToken :: TokenPosn -> Bool
isUnknownToken (TokenPosn (Unknown _)  _) = True
isUnknownToken x = False

-- | The 'hasUnknownTokens' function determines if at
--   least one of the tokens of a token list is Unknown
hasUnknownTokens            :: [TokenPosn] -> Bool
hasUnknownTokens x = elem True (map isUnknownToken x)

-- | The 'takeUnknownTokens' function takes a list of tokens
--   and returns all of said tokens that are Unknown
takeUnknownTokens        :: [TokenPosn] -> [TokenPosn]
takeUnknownTokens = filter isUnknownToken

-- | The 'groupTokensByLine' function takes a list of tokens
--   and returns a list of lists of tokens, each one with the
--   tokens that share a line (in order)
groupTokensByLine     :: [TokenPosn] -> [[TokenPosn]]
groupTokensByLine [] = []
groupTokensByLine all@((TokenPosn _ (AlexPn _ line _)):_) = grouped:(groupTokensByLine remaining)
  where grouped = takeWhile (\(TokenPosn _ (AlexPn _ l _)) -> (l == line)) all 
        remaining     = drop (length grouped) all

-- | The 'formatTokensInLine' function takes a list of tokens and returns
--   the concatenation of each token's string representation, in order,
--   separated by comma and ending in a line break
formatTokensInLine      :: [TokenPosn] -> String
formatTokensInLine tkpair = concatMap (\tk -> (show tk) ++ (separador tk)) tkpair
        where separador tk = if tk /= last tkpair then ", " else "\n"

-- | The 'formatGroupedTokens' function takes a list of lists of tokens and
--   returns the concatenation of each list's in-line representation, as 
--   determined by the 'formatTokensInLine' function
formatGroupedTokens :: [[TokenPosn]] -> String
formatGroupedTokens = concatMap (formatTokensInLine)

-- | The 'formatErrors' function takes a list of tokens (assuming it contains only
--   Unknown tokens) and returns a pretty string representation
formatErrors :: [TokenPosn] -> String
formatErrors [] = []
formatErrors (x:xs) = (formatGroupedTokens [[x]]) ++ (formatErrors xs)

{-
    LINEBREAK FUNCTIONS
    Functions that deal with certain special conditions
    due to the treatment of linebreaks and comments as valid tokens
    in the lexer 
-}

-- | The 'deleteExtraLinebreaks' function takes a list of lists of tokens and,
--   for each list, deletes all the unnecessary linebreaks
deleteExtraLinebreaks :: [[TokenPosn]] -> [[TokenPosn]]
deleteExtraLinebreaks = 
    map (\s -> 
        if (hasLinebreaks s) then 
            (if onlyHasLinebreaks s then 
                (take 1 s) 
            else dropLinebreaks s) 
        else s)

-- | The 'dropLinebreaks' function takes a list of tokens and
--   returns all the tokens it contains that are not Linebreaks
dropLinebreaks :: [TokenPosn] -> [TokenPosn]
dropLinebreaks = filter (isNotLinebreak)

-- | The 'hasLinebreaks' function takes a list of tokens and
--   determines if any of te tokens it contains is Linebreak
hasLinebreaks :: [TokenPosn] -> Bool
hasLinebreaks = any isLinebreak

-- | The 'onlyHasLinebreaks' function takes a list of tokens and 
--   determines if all of the tokens it contains are Linebreaks
onlyHasLinebreaks :: [TokenPosn] -> Bool
onlyHasLinebreaks = all (isLinebreak)

-- | The 'isLinebreak' function takes a token and determines if
--   it is a Linebreak or not.
isLinebreak :: TokenPosn -> Bool
isLinebreak (TokenPosn (LineBreak) _) = True
isLinebreak _ = False

-- | I won't write a comment for this one
isNotLinebreak :: TokenPosn -> Bool
isNotLinebreak = not . isLinebreak

{-
    OTHER FUNCTIONS
    Uncategorized functions that are helpful here or there
-}

-- | The 'removeDoubleQuotes' function receives a string and
--   returns the same string without doublequotes at start and end.
removeDoubleQuotes :: String -> String
removeDoubleQuotes [] = []
removeDoubleQuotes ('"':str) = removeDoubleQuotes str
removeDoubleQuotes str = reverse . drop 1 . reverse $ str 
}