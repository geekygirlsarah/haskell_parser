-- Sarah Withee
-- CS 441 Final Project
-- 
-- I had some assistance from www.seas.upenn.edu/~cis552/12fa/lectures/Parsers.html 
-- for help with the monads and the parser functions, and I also had help from 
-- what we came up with in class to help with the file input.
--
-- While a few of the functions appear the same as from the site, several of 
-- them didn't apply in my case, and I had to heavily modify several of them 
-- to get them to parse letters and numbers (which their code did not do).
-- I also had to integrate the grammar rules from the assignment as well.
--

-- Include it as a module called Parsers
module Parsers where
import Control.Monad
import Data.Char
import Data.String

-- Define a parser as a function type
-- Make it a parameterized type to parse anything as a string and return 
-- that structured object
newtype Parser a = P (String -> [(a, String)])

-- Make a parser function, returns parsed data in a list
doParse :: Parser a -> String -> [(a, String)]
doParse (P p) s = p s

-- Parse one character
-- Either an empty string, or a list of character pairs (cs)
oneChar :: Parser Char
oneChar = P (\cs -> case cs of
                         []     -> []
                         (x:xs) -> [(x,xs)])

-- Function that takes something and returns a parser of its value
returnP :: a -> Parser a
returnP x = P (\cs -> [(x, cs)])

-- Implement bind for a monad
bindP :: Parser a -> (a -> Parser b) -> Parser b
p1 `bindP` fp2 = P (\cs -> [ (b, cs'') | (a, cs') <- doParse p1 cs,
                                         (b, cs'') <- doParse (fp2 a) cs'])
                                         
-- Parser monad
instance Monad Parser where
    (>>=)  = bindP
    return = returnP
    
-- A parser for something that fails... it always fails. Used for satP
failP :: Parser a
failP = P (\_ -> [])

-- Satisfier functions if something satisifes some predicate p.
-- satP1 is a helper for satP
satP1 :: (Char -> Bool) -> Parser Char
satP1 p = P (\cs -> case doParse oneChar cs of
                         [(c,cs')] -> if p c then [(c, cs')] else [])

satP :: (Char -> Bool) -> Parser Char
satP p = do c <- oneChar
            if p c then return c else failP
            
-- Use the satisfier functions to parse letters and numbers and spaces
alphaChar, digitChar, spaceChar :: Parser Char
alphaChar = satP isAlpha
digitChar = satP isDigit
spaceChar = satP isSpace


-- See if a char matches a particular other char
char :: Char -> Parser Char
char c = satP (c ==)


-- Remove spaces from a string
removeSpaces :: String -> String
removeSpaces str = filter (/= ' ') str


-- Nondeterministic choice combinator
-- Basically takes in two parsers and returns one of them, lets you 
-- nondeterministically parse something if there is more than one way to do it
chooseP :: Parser a -> Parser a -> Parser a
p1 `chooseP` p2 = P (\cs -> (doParse p1 cs) ++ (doParse p2 cs))


-- Build a function that takes an alphanumeric parser (alpha or num)
alphaNumChar = alphaChar `chooseP` digitChar 


-- Determine the maximal choice from a set of choices by defining a pipe 
-- operator
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P (\cs -> let result = doParse p1 cs in
                      case result of
                           [] -> take 1 (doParse p2 cs)
                           ((a,cs') : _) -> [ (a,cs') ] )


-- Parser that grabs many things and returns them as a list of each possible 
-- combination
mmanyP :: Parser a -> Parser [a]
mmanyP p = liftM2 (:) p (mmanyP p) <|> return []


-- Use mmanyP and build together a whole integer/natural # out of it
instance Functor Parser where
    fmap = liftM


-- Make sure that fmap above doesn't error out by ensuring at least 1 digit 
-- before trying to read from the parser
many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (mmanyP p)


-- Parse out one natural number (as in one digit)
oneNat :: Parser Int
oneNat = read `fmap` many1 digitChar


-- Add/subtract parser that matches up a symbol, '+' or '-', then another symbol
-- Reducing it to '0' seems to work fine, as if it works, all the 0's cancel 
-- out
addOp :: Parser (Char)
addOp = plus `chooseP` minus
    where plus  = char '+' >> return '0'
          minus = char '-' >> return '0'

-- Same for mulitiplication/division
mulOp :: Parser (Char)
mulOp = times `chooseP` divide
    where times  = char '*' >> return '0'
          divide = char '/' >> return '0'


-- Chain left, as in compute left associative instead of right
-- 2 + 3 * 4 ->  add 2 (mul 3 4) instead of add (mul 2 3) 4
chainl :: Parser b -> Parser b -> Parser b
p `chainl` pop = p >>= rest
    where rest x = next x <|> return x
          next x = do o <- pop
                      y <- p
                      rest $ x

-- Parenthesis matching function, checks for a left and right char, then 
-- returns whatever is in between as a function.
parenP :: Char -> Parser b -> Char -> Parser b
parenP l p r = do char l
                  x <- p
                  char r
                  return x


-- Grammars!
expr = term `chainl` addOp
term = factor `chainl` mulOp
factor = parenP '(' expr ')' <|> alphaNumChar



-- parseOneLine will parse one line to determine if it's valid, then return 
-- "ACCEPT" or "REJECT"    
parseOneLine :: String -> String
parseOneLine "" = ""
parseOneLine line
    -- If it returns an empty list, it's not a valid expression
    -- Ex: "&"
    | doParse expr l == [] = "REJECT"
    -- If the second part of the pair is blank, then it parsed it right and 
    -- nothing is left over
    -- Ex: "a + b * c"
    | snd (head (doParse expr l)) == "" = "ACCEPT"
    -- Anything else can't be accepted
    -- Ex: "a + b )"
    | otherwise = "REJECT"
    -- This parser works, believe it or not, without spaces, so I remove them
    where l = removeSpaces line


-- parseLines takes a list of lines and calls parseOneLine on each line
parseLines [] = []
parseLines (head:tail) =
    parseOneLine(head): parseLines(tail)


-- Main starting function
-- The one function to rule them all!
main = do
    -- Read in file
    contents <- readFile "input.txt"
    -- Write out parsed results to screen
    putStr (unlines (parseLines (lines contents)))

