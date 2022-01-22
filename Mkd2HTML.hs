import Data.List
import Data.Char

-- This program is a markdown to HTML converter which takes a text document in markdown format and
-- outputs a .html file that is the markdown after being converted to HTML code.
-- Code by: Dominick Pulsone

data Mkd = Txt String | Head Mkd | Subhead Mkd | Bold Mkd | Italic Mkd | 
		   NumList [Mkd] | BulletList [Mkd] | Mono Mkd |
		   BR | HR | Block [Mkd] | Pg [Mkd] | Link Mkd Mkd
		deriving (Show,Eq)

data Token = Text String | Eq | Min | Under | BT | DblAsterisk | Para |
			 Asterisk | Greater | NumDot Integer | MD Mkd | Page [Mkd] | NumL [Mkd] | BulletL [Mkd] |
			 LBra | RBra | LPar | RPar
		deriving (Show,Eq)


-- Lexical Analysis

isNumDot :: String -> Bool
isNumDot "" = False
isNumDot (x:xs) =
  let q1 "" = False
      q1 (y:ys) = if isDigit y
                    then q1 ys
                    else if y == '.' then q2 ys else False
      q2 "" = True
      q2 _ = False
  in  isDigit x && q1 xs


addSpaces :: String -> String
addSpaces "" = ""
addSpaces ('=' : s) = " = " ++ addSpaces s
addSpaces ('-' : s) = " - " ++ addSpaces s
addSpaces ('_' : s) = " _ " ++ addSpaces s
addSpaces ('`' : s) = " ` " ++ addSpaces s
addSpaces ('*' : '*' : s) = " ** " ++ addSpaces s
addSpaces ('*' : s) = " * " ++ addSpaces s
addSpaces ('[' : s) = " [ " ++ addSpaces s
addSpaces (']' : s) = " ] " ++ addSpaces s
addSpaces ('(' : s) = " ( " ++ addSpaces s
addSpaces (')' : s) = " ) " ++ addSpaces s
addSpaces ('>' : s) = " > " ++ addSpaces s
addSpaces (' ' : ' ' : s) = " /p " ++ addSpaces s
addSpaces (x : s) = x : addSpaces s

classify :: String -> Token
classify "=" = Eq
classify "-" = Min
classify "_" = Under
classify "`" = BT
classify "**" = DblAsterisk
classify "*" = Asterisk
classify ">" = Greater
classify "[" = LBra
classify "]" = RBra
classify "(" = LPar
classify ")" = RPar
classify "/p" = Para
classify x | isNumDot x = NumDot (read (init x))
classify x = Text x

lexer :: String -> [Token]
lexer s = collapseTokens (map classify (words (addSpaces s)))

collapseTokens :: [Token] -> [Token]
collapseTokens [] = []
collapseTokens (Text x : Text y : ts) = collapseTokens ((Text (x ++ " " ++ y)) : ts)
collapseTokens (Eq : Eq : Eq : Eq : Eq : ts) = collapseTokens ([Eq,Eq,Eq,Eq]++ts)
collapseTokens (Min : Min : Min : Min : Min : ts) = collapseTokens ([Min,Min,Min,Min]++ts)
collapseTokens (Under : Under : Under : Under : ts) = collapseTokens ([Under,Under,Under]++ts)
collapseTokens (x:xs) = x : collapseTokens xs

-- Parsing

parser :: [Token] -> Mkd
parser l = sr l []

sr :: [Token] -> [Token] -> Mkd
-- text 
sr i (Para : ts) = sr i (MD BR : ts)
sr i (Text t : ts) = sr i (MD (Txt t) : ts)
-- header
sr i (Eq : Eq : Eq : Eq : MD m : ts) = sr i (MD (Head m) : ts)
-- subheader
sr i (Min : Min : Min : Min : MD m : ts) = sr i (MD (Subhead m) : ts)
-- bold
sr i (DblAsterisk : MD m : DblAsterisk : ts) = sr i (MD (Bold m) : ts)
-- italic
sr i (Under : MD m : Under : ts) = sr i (MD (Italic m) : ts)
-- horizontal rule
sr i (Under : Under : Under : ts) = sr i (MD (HR) : ts)
-- monospace
sr i (BT : MD m : BT : ts) = sr i (MD (Mono m) : ts)
-- links
sr i (RPar : (MD (Txt x)) : LPar : RBra : (MD (Txt y)) : LBra : ts) = sr i (MD (Link (Txt y) (Txt x)) : ts)
-- blockquotes
sr i (MD m : Greater : (MD (Block xs)) : ts) = sr i (MD (Block (xs++[m])) : ts)
sr i (MD m : Greater : ts) = sr i (MD (Block [m]) : ts)
-- numbered lists
sr i (MD m : NumDot x : (NumL xs) : ts) = sr i (NumL (xs++[m]) : ts)
sr i (MD m : NumDot x : ts) = sr i ((NumL [m]) : ts)
-- bulleted lists
sr i (MD m : Asterisk : (BulletL xs) : ts) = sr i ((BulletL (xs++[m])) : ts)
sr i (MD m : Asterisk : ts) = sr i ((BulletL [m]) : ts)
-- nested lists
sr i (BulletL b : NumL n : ts) = sr i (listHelper (BulletL b) (NumL n) : ts)
sr i (NumL n : BulletL b : ts) = sr i (listHelper (NumL n) (BulletL b) : ts)
-- shift
sr (i:is) xs = sr is (i:xs)
-- termination
sr [] (MD x : ts) = sr [] ((Page [x]) : ts)
sr [] (NumL n : ts) = sr [] (Page [NumList n] : ts)
sr [] (BulletL b : ts) = sr [] (Page [BulletList b] : ts)
sr [] (Page x : NumL n : ts) = sr [] (Page ([NumList n]++x) : ts)
sr [] (Page x : BulletL b : ts) = sr [] (Page ([BulletList b]++x) : ts)
sr [] (Page ms : MD x : ts) = sr [] (Page (x:ms) : ts)
sr [] [Page x] = Pg x
-- error
sr [] x = error $ "Parser error: " ++ show x

listHelper :: Token -> Token -> Token
listHelper (BulletL b) (NumL n) = if isBulletList (last n) then NumL (init n++[bulletNester (last n) (BulletL b)])
								  else (NumL (n++[BulletList b]))
listHelper (NumL n) (BulletL b) = if isNumList (last n) then BulletL (init b++[numNester (last b) (NumL n)])
								  else (BulletL (b++[NumList n]))								  

bulletNester :: Mkd -> Token -> Mkd
bulletNester (BulletList x) (BulletL y) = (BulletList (x++y))

numNester :: Mkd -> Token -> Mkd
numNester (NumList x) (NumL y) = (NumList (x++y))

isBulletList :: Mkd -> Bool
isBulletList (BulletList x) = True
isBulletList x = False

isNumList :: Mkd -> Bool
isNumList (NumList x) = True
isNumList x = False

-- Conversion to HTML

convertPage :: Mkd -> String
convertPage (Pg []) = []
convertPage (Pg (x:xs)) = (convert x) ++ convertPage (Pg xs)
convertPage x = error $ "Input must be a parsed expression."

convert :: Mkd -> String
convert (Txt x) = x
convert (Bold x) = " <strong>" ++ convert x ++ "</strong> "
convert (Italic x) = " <em>" ++ convert x ++ "</em> "
convert (BR) = "<br>"
convert (Head x) = "<h1>" ++ convert x ++ "</h1>"
convert (Subhead x) = "<h2>" ++ convert x ++ "</h2>"
convert (Mono x) = " <code>" ++ convert x ++ "</code> "
convert (HR) = "<hr>"
convert (Link (Txt t) (Txt l)) = "<p><a href=\"" ++ l ++ "\">" ++ t ++ "</a></p>"
convert (NumList x) = "<ol>" ++ convertLists x ++ "</ol>"
convert (BulletList x) = "<ul>" ++ convertLists x ++ "</ul>"
convert (Block x) = "<blockquote>" ++ convertBlock x ++ "</blockquote>"

convertLists :: [Mkd] -> String
convertLists [] = []
convertLists (x:(BulletList b):xs) = "<li>" ++ convert x ++ convert (BulletList b) ++ "</li>" ++ convertLists xs
convertLists (x:(NumList n):xs) = "<li>" ++ convert x ++ convert (NumList n) ++ "</li>" ++ convertLists xs
convertLists (x:xs) = "<li>" ++ convert x ++ "</li>" ++ convertLists xs

convertBlock :: [Mkd] -> String
convertBlock [] = []
convertBlock (x:xs) = "<p>" ++ convert x ++ "</p>" ++ convertBlock xs

-- User Interaction

main :: IO ()
main = do
	putStrLn "Enter the filename you’d like to convert"
	filename <- getLine
	input <- readFile filename
	let lexed = lexer input
	let parsed = parser lexed
	let output = convertPage parsed
	putStrLn "Enter the name of the output file (with no file extension)"
	newFile <- getLine
	writeFile (newFile ++ ".html") output -- Idea from https://stackoverflow.com/a/36227848/12011814
