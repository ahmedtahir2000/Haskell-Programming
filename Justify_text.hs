-- CS 300 Exam 1 on 21 Feb 2021 from 11am to 11pm

-- Before proceeding, press Alt-Z in Visual Studio Code so lines are wrapped

-- HONOR CODE:  I have taken the exam completely alone.  I have not consulted with any other person regarding the exam or any material from the course for the duration of the exam.  Except the six resources listed below, I have not consulted any other resource including searching the internet, accessing shared documents, or accessing any prior code I had.  I have reported any violation of the honor code that I knew and will report any that I may become aware of.  I understand that academic misconduct can result in disciplinary action.  

-- 

-- Write your full name and roll number to accept the honor code. (REQUIRED)
-- Ahmed Tahir Shekhani - 2023-10-0197

-- INSTRUCTIONS

-- * A textual description is REQUIRED with each part about how you solved it.
-- * You may only email the instructor about the exam (junaid.siddiqui@lums.edu.pk).  TAs will not be available.
-- * You can make any number of helper functions to solve your problem.  Most parts come with detailed hints and suggested helper functions but you are not required to follow them.  However, the provided main functions must work without any changes.
-- * You can work on the exam till Saturday 20 Feb 11 pm.  Submit on LMS.  Remember that internet outage is not a valid reason for extension since the prescribed setup works completely offline.
-- * The following six resources are allowed during the exam:
--      * Haskell Programming from first principles (http://haskellbook.com/)
--      * Learn You a Haskell for Great Good! (http://learnyouahaskell.com/chapters)
--      * Happy Learn Haskell Tutorial (http://www.happylearnhaskelltutorial.com/contents.html)
--      * Haskell (https://en.wikibooks.org/wiki/Haskell)
--      * Hoogle (https://hoogle.haskell.org/)
--      * The eight lecture summary emails sent from the instructor

import Data.List
import Data.Char

-- High level description

-- A common type of text alignment in print media is "justification," where the spaces between words, are stretched or compressed to align both the left and right ends of each line of text.  I​n this problem we’ll be implementing a text justification function for a monospaced terminal, i.e., fixed width font where every letter has the same width.  The alignment is achieved by inserting blanks and hyphenating the words.  For example, given this excerpt from George Orwell's famous novel 1984:

text :: String
text = "He who controls the past controls the future. He who controls the present controls the past."

-- We want to be able to align it like this to a specific width, say 15 columns:

-- He who controls
-- the  past cont‐ 
-- rols  the futu‐ 
-- re.  He who co‐ 
-- ntrols the pre‐ 
-- sent   controls 
-- the past.

-- We will implement this in three parts.


-- Part 1: Text fitting (8 points)

-- In this part, we want to fit the text within given width without justification or hyphenation.  The fitText function takes the column width, a list of words, and returns a list of lines where each line is a list of words.  See the main function and expected output below.  You may assume that no single word is longer than the given line length.  

-- Rest of the description below contains HINTS and you may choose not to follow it.
-- The test main uses the functions 'words' which splits a string into words, 'unwords' which joins words with spaces to form a single lines, and 'unlines' which joins lines with newline characters to form a paragraph.  If needed, you may use these functions in your code and remember that a 'length' function exists which can give you the length of any string.
-- Break the problem into two parts.  First, write the fitLine function that breaks a given list of words into two parts where the first part will fit into the given length, and everything else is in the second part.  You can use (length (unwords x)) to find the length occupied by a list of words stored in 'x'.  Remember the recursive thought process while writing this function.  Second, write the fitText function as a recursive function where you assume that the recursive function can correctly form the paragraph if only you can split out the first line correctly.  Both functions end up to be less than five lines of code.

removeList :: [] String -> [] String -> [] String
removeList = \list1 -> \list2 ->
    case list1 of
        [] -> list2
        x:xs -> removeList (remove_one list1 x) (remove_one list2 x)

remove_one :: [String] -> String -> [String]
remove_one = \list -> \v -> 
    case list of 
        [] -> []
        x:xs | v==x -> xs
        x:xs -> x:remove_one xs v

first:: [] String ->Int -> [] String
first = \list -> \v ->
    case list of
        [] -> []
        [x] | (length x) <= v -> [x]
        x:xs | length x <= v -> x:first xs (v-(length x)-1)
        _ -> []

fitLine :: Int -> [] String -> ([] String, [] String) -- Suggested
fitLine = \y -> \list -> 
    case list of
        [] -> ([],[])
        [x] | (length x) <= y -> ([x],[])
        [x] -> ([],[x])
        x:xs -> ((first list y),removeList ((first list y)) list)

fitText :: Int -> [] String -> [] ([] String) -- Required
fitText = \v -> \list ->
    case list of
        [] -> []
        x:xs -> let (p,q) = fitLine v list
                in p:(fitText v q)

-- main = putStr (unlines (map unwords (fitText 30 (words text)))) 

-- IMPORTANT: Type here a detailed description of your part 1 solution
-- In order to divide the text according to coloumn width we need to fit first line. Using 'first' function we will take out first line according to width in first element of tuple in fitLine function. Other lines in second element. Then used fitLine recursively to completely separate text in form of line according to column width. 

-- main = putStr (unlines (map unwords (fitText 30 (words text))))

-- Expected Output:

-- He who controls the past
-- controls the future. He who
-- controls the present controls
-- the past.


-- Part 2: Hyphenation (7 points)

-- In this part, we will add support for hyphenation, that will allow us to fit a bit more text in each line.  Our hyphenation dictionary is given as a list of tuples where the first member is a word which is allowed to be hyphenated and the second member of the tuple is a list of hyphenation points.  See expected output below.

-- Rest of the description below contains HINTS and you may choose not to follow it.
-- Start by copying the code of fitText in fitHyphenatedText accomodating for the extra argument.  Change the call to fitLine to the findHyphenatedLine function.  The fitHyphenatedLine function can use the fitLine function to find an initial split and then try hyphenating the first token not included in the line (if any) using the hyphenateWord function which should be passed the remaining space in the line.  Test your code till this point by making hyphenateWord always return hardcoded 'Just ("cont-", "rols")'.
-- Now you can write hyphenateWord which has to do a few things: 1) separate any punctuation (you may use 'span isAlpha wordsArray' that will split an array into a tuple of two arrays with the word in the first array and punctuation in the second, or you may write your own code). 2) Use the 'find' function (or your own code) to lookup if the word exists in the hyphenation dictionary. The 'find' function takes a predicate function and returns the first matching element inside 'Just', or 'Nothing' if no element matches.  3) Use findHyphenationPoint function (can be hardcoded for testing) to find the correct word split (e.g., 'cont' and 'rols').  4) Add a '-' to the first part and add back any punctuation removed earlier to the second part. 
-- Note that hyphenateWord has to connect find and findHyphenationPoint that both return Maybe and it has to itself return a Maybe.  If you understand the bind operator (>>=), using it will reduce your code for this function to a few lines.
-- Finally write findHyphenationPoint that has to find the largest part until a valid hyphenation point that can fit in the allowed length.  Find a smaller problem for the recursive call e.g. by removing a hyphenation point.  Choose among the removed hyphenation point and the best one returned from the recursive call.  The 'concat' function may be used to concatenate an array of strings together in to a single string.  Remember to return 'Nothing' if no point can fit in the allowed space.

joinType :: Maybe (String, String) -> Maybe (String, String) -> Maybe (String, String)
joinType = \x -> \y ->
    case x of
        Nothing -> Nothing
        Just (p,q) -> 
            case y of
                Nothing -> Just (p, q)
                Just (r,s) -> Just ((p ++ r),(q ++ s))

findHyphenationPoint :: [] String -> Int -> Maybe (String, String) -- Suggested
findHyphenationPoint = \list -> \v -> 
    case list of
        [] -> Nothing
        x:xs | length (x) < v ->  joinType (Just (x,"")) (findHyphenationPoint xs (v-length(x)))
        x:xs ->  joinType (Just ("",x)) (findHyphenationPoint xs (v-length(x)))

findInDict:: [(String, [] String)] -> String -> Maybe (String, [] String)
findInDict = \hyphenList -> \word ->
    case hyphenList of 
        [] -> Nothing
        x:xs -> let (p,q) = x
                in case p of
                    _ | p == word -> Just (p,q)
                    _ -> findInDict xs word

hyphenateWord :: [(String, [] String)] -> Int -> String -> Maybe (String, String) -- Suggested
hyphenateWord = \hyphenList -> \v -> \word -> let (w, p)= span isAlpha word
                                                in case (findInDict hyphenList w) of
                                                    Nothing -> Just (word, "")
                                                    Just y ->  let (g,h) = y
                                                                in case (findHyphenationPoint h v) of
                                                                    Nothing -> Just (word, "")
                                                                    Just(m,n) -> Just (m++"-", n++p)
                            


fitHyphenatedLine :: [(String, [] String)] -> Int -> [] String -> ([]String, []String)  -- Suggested
fitHyphenatedLine = \hyphenList -> \v -> \line -> 
    case line of
        [] -> ([],[])
        _ -> let (p,q) = fitLine v line
                in case q of
                    l:ls -> 
                        case (hyphenateWord hyphenList (v - length (unwords p)-1) l) of
                            Just (m,n) -> case n of
                                            _| n /= ""->(p++[m],[n] ++ (remove_one q l) )
                                            _ -> (p,q)
                    _ -> (p,q)

fitHyphenatedText :: [(String, [] String)] -> Int -> [] String -> [[] String] -- Required
fitHyphenatedText = \hyphenList-> \v -> \list ->
    case list of
        [] -> []
        x:xs -> let (p,q) = fitHyphenatedLine hyphenList v list
                in p:(fitHyphenatedText hyphenList v q)

-- IMPORTANT: Type here a detailed description of your part 2 solution
-- In order to fitHyphenatedLine, We have to fit fitHyphenated word, so for that we have to pass the word in the next and check whether we can convert that into hyphenated word or not using the dictionary. In function findInDict, we are finding that is the word feasible for hyphenation. If yes, then we have to find the words that can be fitted in left spaces. For that we find the words making findHyphenationPoint function which returned us a tuple where first part can be fitted in first line and second part in second line. To bring word that fit in the exact left spaces of the first line we have to use every value of the key for which hyphenation is possible using hyphenated dictionary and join them if they can fit in left spaces using function joinType. Now we will return that hyphenated word with hyphen and punctuation (if any). Then we will remove that hyphenated word from other line so to avoid repition and concatenate the list.

hyphenationsPart2 :: [(String, [] String)]
hyphenationsPart2 = [("controls", ["cont", "ro", "ls"]), ("past", ["pa", "st"])]

--main = putStr (unlines (map unwords (fitHyphenatedText hyphenationsPart2 31 (words text))))

-- Expected Output:

-- He who controls the past cont-
-- rols the future. He who contro-
-- ls the present controls the pa-
-- st.


-- Part 3: Justified Text (5 points)

-- We now want extra spaces to be inserted such that the text is fully justified, except the last line.  We also want the spaces to be spread out so it looks good.  We will use a formula (2^n-1) where n is the number of consecutive extra spaces inserted to calculate the line 'badness' and then choose the line that is least bad.  A line with three extra spaces at different places will have 3 badness whereas if two of the extra spaces are consecutive it will have 4 badness and 7 badness if all three are consecutive.  Note that your code must be based on calculating and using line badness even if you deviate from the hints below.

-- Rest of the description below contains HINTS and you may choose not to follow it.
-- The justifyText function can use the output of fitHyphenatedText and then apply the insertSpaces function on all except the last line.  You may use 'init' which returns all except last element of an array and 'last' which returns the last element, or you may write your own code.  We can use "" (empty strings) to represent extra spaces.  When combined with 'unwords' these will be joined with real spaces.
-- The insertSpaces function should find all possible ways to insert spaces using getSpacingVariations.  It should then calculate the badness of each line using lineBadness and finally find the one with the minimum badness.  If you put badness and the spaced line in a tuple, the standard minimum function will find the minimum correctly.  You may hard-code badness to a constant and getSpacingVariations to always returned the unchanged line (as a one element array) to test till this point.
-- The lineBadness function takes a list of words and a sequence parameter that tells how many extra spaces were there 'before' the start of this part of line.  If the first word is not a blank (i.e., "") you need to add the cost of the sequence of spaces till this point before passing 0 to the recursive call to start a new sequence.
-- Next is getSpacingVariations, which is a recursive function that gives one less space to be inserted to the recursive call and uses getSingleSpaceVariations to insert that one space in the results on the recursive call.  Remember that you have to insert that one space in each of the recursive call results, and even adding a single space has many variants so you need to carefully 'map' and 'concat', or write your own code, or you can use the bind operator here as well.  You may hard-code getSingleSpaceVariations to always insert a space after the first word to test this part.
-- Finally, getSingleSpaceVariations is a recursive function that attempts to insert a space at all possible points except at the very start.

helper ::  String -> [] String -> [] String -> [] String
helper = \s -> \line -> \list ->
    case line of
        [] -> []
        x:xs -> list ++ x : [""] ++ xs 
 
mapType :: (String -> [] String -> [] String -> [] String) -> [] String -> [] String -> [] ([] String)
mapType = \fn -> \list -> \list2 ->
    case list of
        [] -> []
        x:xs -> fn x list list2 : mapType fn xs (list2 ++ [x])

getSingleSpaceVariations :: [] String -> [] ([] String) -- Suggested
getSingleSpaceVariations = \line -> 
    case line of
        [] -> []
        x:xs -> mapType helper line []

m :: (Int -> [] String -> [] ([] String)) -> [] ([] String) -> Int -> [] ([] String) 
m = \fn -> \list -> \y ->
    case list of
        [] -> []
        x:xs -> fn y x ++ m fn xs y

getSpacingVariations :: Int -> [] String -> [] ([] String) -- Suggested
getSpacingVariations = \x -> \line ->
    case x of
        0 -> []
        1 -> getSingleSpaceVariations line
        _ -> m getSpacingVariations (getSingleSpaceVariations line) (x-1)
        
                

lineBadness :: [] String -> Int -> Int -- Suggested
lineBadness = \line -> \y ->
    case line of
        [] -> 0
        x:xs | x=="" -> lineBadness xs (y+1) 
        _:xs -> ((2^y)-1) + lineBadness xs 0


firstElement :: [] ([] String) -> [] String
firstElement = \list ->
    case list of
        [] -> []
        x:xs -> x

insertSpaces :: Int -> [] String -> [] String -- Suggested
insertSpaces = \v -> \line -> let p = getSpacingVariations v line
                                in case p of
                                    x:xs -> let (q , r) = (lineBadness x 0, x)
                                        in r
    

justifyText :: [(String, [] String)] -> Int -> [] String -> [] ([] String) -- Required
justifyText = \hyphen_dict -> \v -> \text -> 
    case text of
        [] -> []
        _ -> insertSpaces v (init (fitHyphenatedText hyphen_dict v text))  

-- IMPORTANT: Type here a detailed description of your part 3 solution
-- 
-- 
-- 
-- 

hyphenationsPart3 :: [(String, [] String)]
hyphenationsPart3 = [("controls", ["co", "nt", "ro", "ls"]), ("future", ["fu", "tu", "re"]), ("present", ["pre", "se", "nt"])]

-- main= print(insertSpaces 3 ["co", "nt", "ro"])
main = putStr (unlines (map unwords (justifyText hyphenationsPart3 15 (words text))))

-- Expected output is given on Line 38 above
-- He who controls
-- the  past cont‐ 
-- rols  the futu‐ 
-- re.  He who co‐ 
-- ntrols the pre‐ 
-- sent   controls 
-- the past.