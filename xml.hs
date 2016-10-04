--FAIL VS FATAL TRIVIALPARSER FILE!!!

--David Kennedy
--Advanced Functional Programming Final Project
--This is an xml parser using parsec.  It uses some of
--the functions you included in the homework template 

module Parser where

import qualified System.IO
import Data.Char(digitToInt,isUpper)

-- These are for defining parsers
import Text.Parsec 
import Text.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
import qualified Text.Parsec.Token as Token
import Data.List(nub,(\\))
-- This is for possible Monads underlying the Parsing Monad
import Control.Monad.State
import Data.Functor.Identity(Identity(..))

-- This is to catch errors when Parsing
import qualified Control.Exception

-----------------------------------------------
-- running parsers

-- Extract a computation from the Parser Monad 
runMParser parser parserState name tokens =
  runIdentity (runParserT parser parserState name tokens)

parse1 file x = runMParser x tagNamesState file



-- Parse a string in an arbitray monad 
parseString x s =
  case parse1 s x s of
   Right(ans) -> return ans
   Left message -> fail (show message)

-- Parse a File in the IO monad 
parseFile parser file =
    do possible <- Control.Exception.try (readFile file)
       case possible of
         Right contents ->
            case parse1 file parser contents of
              Right ans -> return ans
              Left message -> error(show message)
         Left err -> error(show (err::IOError))


--This piece of state will be updated as a stack with open and closed tags to
-- make sure they are properly nested
tagNamesState = []


type MParser a = ParsecT
                    String                -- The input is a sequence of Char
                    [String]              -- The internal state for Layout tabs
                    (Identity)            -- The underlying monad is simple
                    a                     -- the type of the object being parsed

-- Based on Parsec's haskellStyle (which we can not use directly since
-- Parsec gives it a too specific type). 
lbStyle = Token.LanguageDef
                { Token.commentStart   = "{-"
                , Token.commentEnd     = "-}"
                , Token.commentLine    = "--"
                , Token.nestedComments = True
                , Token.identStart     = lower <|> upper
                , Token.identLetter    = alphaNum <|> oneOf "_'"
                , Token.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Token.caseSensitive  = True
                , Token.reservedOpNames =
                    []
                , Token.reservedNames  =
                  []
                }


haskellTP = Token.makeTokenParser lbStyle

idenT       = Token.identifier haskellTP


--we want a prolog by default even if no prolog in document so not maybe
--prolog
data XmlDoc = Document Prolog RootElement deriving Show

data Prolog = Prolog DocumentProperties deriving Show -- [PROC_IXS] [COMMENTS]


data DocumentProperties = DocProp {version::String, encoding::String,standalone::String} deriving Show

data RootElement = Root TagElement deriving Show

data TagElement = ElemTag Name [Attribute] [Element] deriving Show

data Element =   Tag TagElement | Text String | WhiteSpace String deriving Show

data Attribute = Attr {getName ::Name, getValue:: Value} deriving Show
type Name = String
type Value = String
--------------------------------------------
--ADD ATTRIBUTES WHICH CAN OCCUR IN MULTIPLE PLACES INCLUDING ATTRIBUTE VALUE
--MAKE IT SO JUST A SPACE IS NOT LEGAL
--ADD CDATA?
makeDefaultDocProp = DocProp {version ="1.0",encoding="UTF-8",standalone=""}
parseDocument :: MParser XmlDoc
parseDocument = do { docprops <- option makeDefaultDocProp parseXmlDecl;
                     xmldoc <- parseTagElement;
                     checkAllInputUsed;  --be sure consumed all input
                     return $ Document (Prolog docprops) (Root xmldoc)}               

checkAllInputUsed = eof <?> "Failure: Could not parse entire document"

--All configuration settings will be kept in document properties.  If a doc prop
--field has value empty string, then it was not given by the user
parseXmlDecl :: MParser DocumentProperties
parseXmlDecl = do {string "<?xml ";
                   v1 <- getAttr "version";
                   v2 <- option "" (getAttr "encoding");
                   v3 <- option "" (getAttr "standalone");
                   spaces;
                   char '>';
                   return DocProp {version=v1,encoding=v2,standalone=v3}}

getAttr :: String -> MParser String
getAttr st = do {spaces; string st;equalsSign; v <- quotedValue;return v}      
--makes it so that an equals sign can have one space before and after or not
equalsSign :: MParser ()
equalsSign = do { optional space; char '='; optional space}

--this function adapted from: http://book.realworldhaskell.org/read/using-parsec.html
quotedValue :: MParser String
quotedValue = do x <- singleOrDoubleQuote
                 content <- many $ noneOf "\'\"" 
                 char x 
                 return content


singleOrDoubleQuote :: MParser Char 
singleOrDoubleQuote =  try (char '\'') <|> char '\"' 

parseTagElement :: MParser TagElement
parseTagElement = (try parseEmptyTag)  <|> parseRegularTag

parseEmptyTag :: MParser TagElement
parseEmptyTag = do {char '<'; 
                    x <- idenT;
                    xs <- parseAttrs; 
                    char  '/';
                    spaces;
                    char '>';
                    return (ElemTag x [] [])}

parseRegularTag :: MParser TagElement
parseRegularTag = do {(nm,attrs) <- parseOpenTag;
                      xs <- parseElems;
                      parseCloseTag;
                      return (ElemTag nm attrs xs)}

parseOpenTag :: MParser  (Name, [Attribute])
parseOpenTag = do { try $ char '<' >> notFollowedBy (char '/');
                    x <- idenT;
                    modifyState ((:) x);
                    xs <- parseAttrs;
                    char '>';
                    return (x, xs)}

parseCloseTag :: MParser ()
parseCloseTag = do {string "</";
                    x <- idenT;
                    ys <- getState;
                    modifyState (tail);
                    char '>';
                    if x == (head ys) then return ()
                                      else 
                                        unexpected ("Start (" ++ show (head ys) ++
                                                        ") and end (" ++ show x ++
                                                                ") tag mismatch")}
parseElems :: MParser [Element]
parseElems = do {xs <- many parseElement;
                 return xs}
--If there are any repeat attribute names, returns a list
--of all repeats
checkForRepeats :: [Name] ->  MParser [Name]
checkForRepeats l@(x:xs) =  return  (nub (l \\ (nub l)))
checkForRepeats _      = return []
                                        
parseTag :: MParser Element
parseTag = do { x <- parseTagElement;
                return (Tag x)}

parseElement :: MParser Element
parseElement = parseTag  <|>parseSpace <|> parseText   

--This function is a little more complicated than the original version 
--below it.  This is so that I can seperate space as an element from space
--that is part of a text element by running the space parser first and then
--running the parseElement function repeatedly. 
parseSpace :: MParser Element
parseSpace = do {x <- twoSpaceOrOneTabNL;
                 y <- many space;
                 return (WhiteSpace (x++y))} <?> "parseSpace"

                      
twoSpaceOrOneTabNL :: MParser String
twoSpaceOrOneTabNL =  many1( oneOf "\t\n") <|> count 2 space 
--Text parser considers a tab a space or 2 or more spaces to be whitespace.
--Maybe change this because maybe a single newline is not its own whitespace
--element but just part of a text element unless there are two or more of them.
--But this approach seems right.Suspiciously a bit hard to find this out from the spec.
--noneOf stops at all boundary spots including one that is only a potential bounday
--(single space).  notTwoSpaces will then approve all single spaces but stop at any
--more than one
parseText :: MParser Element 
parseText = do {x <- many1 (noneOf "&<>\t\n " <|> try( notTwoSpaces));
                            return $ Text x} <?> "parseText"

--will parse a regular space only in the 
notTwoSpaces :: MParser Char
notTwoSpaces = do { x<-char ' '; notFollowedBy space; return x}

getNames ::  [Attribute] -> MParser [Name]
getNames xs = return (map getName xs)

parseAttrs :: MParser [Attribute]
--parseAttrs = do { xs <- manyTill parseAttr (char '>');
parseAttrs = do { xs <- manyTill parseAttr checkIfCloseOrBackslash;
                  ys <- getNames  xs;
                  zs <- checkForRepeats ys;
                  if zs == [] then return xs
                              else unexpected ("Repeat attribute name" ++ show zs)}
                 
checkIfCloseOrBackslash :: MParser Char 
checkIfCloseOrBackslash = lookAhead $ char '>' <|> char '/'
parseAttr :: MParser Attribute
parseAttr = do { name <- idenT;
                 equalsSign; 
                 val  <-  attrVal;
                 many $ space;
                 return (Attr name val)}
--Did another version of this up top.  Pick a version?  Or are they subtly different?
attrVal :: MParser String
--FIX THIS KIND OF HACK AND WILL NOT WORK FOR NESTED QUOTES OR SINGLE QUOTES
attrVal = do { x <- singleOrDoubleQuote;
               y <- many  $ noneOf "&<\"\'";
               z <- anyChar;
               if x == z then return y
                         else unexpected ("ILLEGAL CHAR IN ATTR VAL : " ++ show z)

             }


--------------------------------
-- The main functions here


main :: IO()
main = do{
         xs <- parseString parseTagElement "<hello a=\"a\"><you>  <dog/></you>  </hello>";
         print xs;
         return ()}

emptyQuotesAttrValGOOD = do {
         xs <- parseString parseTagElement "<hello a=\"\"><you>  <dog/></you>  </hello>";
         print xs;
         return ()}
repeatAttrsFAIL:: IO()

--FAILS AS OF NOW
nestedAttrValQuotesGOOD :: IO ()
nestedAttrValQuotesGOOD = do {
         xs <- parseString parseTagElement "<hello a=\"\"\"\"><you>  <dog/></you>  </hello>";
         print xs;
         return ()}

spacesBetweenAttributesGOOD :: IO ()
spacesBetweenAttributesGOOD = do {
         xs <- parseString parseTagElement "<hello a=\"b\"    b=\"12345\"c=\"xx\"\"\"   ><you>  <dog/></you>  </hello>";
         print xs;
         return ()}
--FAILS FOR THE WRONG REASON IM SURE
nestedAttrValQuotesNoMatchFAIL :: IO ()
nestedAttrValQuotesNoMatchFAIL = do {
         xs <- parseString parseTagElement "<hello a=\"\"\"\"\"><you>  <dog/></you>  </hello>";
         print xs;
         return ()}
repeatAttrsFAIL = do{
         xs <- parseString parseTagElement "<hello a=\"a\"a=\"b\"a=\"c\"><you>  <dog/></you>  </hello>";print xs;return ()}

attrsContainOpenTagFAIL :: IO ()
attrsContainOpenTagFAIL = do{

         xs <- parseString parseTagElement "<hello a=\"a<\"><you>  <dog/></you>  </hello>";print xs;return ()}
-- 
attrsContainAmpFAIL :: IO ()
attrsContainAmpFAIL = do{

         xs <- parseString parseTagElement "<hello a=\"a&<\"><you>  <dog/></you>  </hello>";print xs;return ()}


parseTextAndWhiteSpaceFAIL :: IO ()
parseTextAndWhiteSpaceFAIL = do {
         xs <- parseString parseElems "    Hello\t\t   you\t\t\twe did not do     this...!&stopbeforeamp";
         print xs;
         return ()}
parseTextAndWhiteSpaceGOOD :: IO ()
parseTextAndWhiteSpaceGOOD = do {
         xs <- parseString parseElems "   Hello you\t\t\n\twe did not do     this...!&stopbeforeamp";
         print xs;
         return ()}

shouldFailOnExtraInputFAIL :: IO ()
shouldFailOnExtraInputFAIL = do { 
         xs <- parseString parseDocument "<?xml version=\'AAA\'><a/>aaa";
         print xs;
         return ()}


complicatedExampleGOOD :: IO ()
complicatedExampleGOOD = do { 
         xs <- parseString parseDocument "<?xml version=\'AAA\'><dog><horse>aaaa<pig big =\"yes\"><cow/></pig>   </horse>  \t\t\nYESSS<a/>aaa</dog>";
         print xs;
         return ()}

complicatedExampleFAIL :: IO ()
complicatedExampleFAIL = do { 
         xs <- parseString parseDocument  "<?xml version=\'AAA\'><dog><horse>aaaa<pig big =\"yes\"></cow></pig>   </horse>  \t\t\nYESSS<a/>aaa</dog>";
         print xs;
         return ()}
