-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Data.List
import Data.Char
import Parser.Instances

-- ==============================( Main Functions )==============================
-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
pickCard :: ActionFunc
pickCard top (p1,p2) memory action hand = (draw action, edit memory action)
    where
        draw (Nothing) = Stock -- Beginning of the game
        draw (Just _)
            | dwCount (bestMeld (hand ++ [top])) <= dwCount (bestMeld (hand)) = Discard -- If top discard has a good card
            | potentialMeld top hand = Discard -- If the discard has potential to form a meld with another discard (Missing 1 card to form meld)
            | otherwise = Stock 
        edit (Just mem) (Just Stock) 
            | toScore mem /= fromScore (p1 + p2) = reset-- If the score changes, reset the memory (Indicating its a start of a new round and opponent starts first)
            | otherwise = fromMemory ( unique ([top : head (toMemory mem)]) ++ [head (head (toMemory mem)) : top : head (tail (toMemory mem))] -- Updates the discards and cards opponent rejects in memory
            ++ [last (toMemory mem)]) ++ fromScore (p1 + p2)
        edit (Just mem) (Just Discard)
            | toScore mem /= fromScore (p1 + p2) = reset-- If the score changes, reset the memory (Indicating its a start of a new round and opponent starts first)
            | otherwise = fromMemory ( unique ([top : head (toMemory mem)]) ++ [top : head (tail (toMemory mem))]   -- Updates the discards, cards opponent rejects and cards opponent wants in memory
            ++ [head (head (toMemory mem)) : last (toMemory mem)]) ++ fromScore (p1 + p2)
        edit (Nothing) (Just _) = fromMemory [[top],[top],[]] ++ fromScore (p1 + p2)-- Initialize memory when second to start (Indicates a start of a new round and the player starts first)
        edit _ _ = reset -- Initalize memory when first to start
        reset = fromMemory [[top],[],[]] ++ fromScore (p1 + p2) -- New memory with addition of score (To check if a new round has started)

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard new _ memory uCards = (act (includeTop) (toMemory memory), edit memory)
    where 
        act hand mem
            | length (head (mem)) <= 2 = 
            Action Drop (bestDrop (filterWants (extractN (leastDW) uCards hand) (new)) (mem))  -- To avoid calling in first turn
            | leastDW == 0 = Action Gin (bestDrop (extractN 0 uCards hand) (mem)) -- If a Gin was found
            | extractK uCards hand /= [] = Action Knock (bestDrop (extractK uCards hand) (mem)) -- If a knock is possible
            | otherwise = Action Drop (bestDrop (filterWants (extractN (leastDW) uCards hand) (new)) (mem))  -- Drop
        edit = addDiscard (act (includeTop) (toMemory memory)) -- Returns the memory after updating via addDiscard
        addDiscard (Action _ card) mem = fromMemory ([card : head (toMemory mem)] ++ (tail (toMemory mem))) ++ toScore mem -- Updates memory
        leastDW = bestDW uCards includeTop -- The minimum number of deadwood cards in all possible meld combinations
        includeTop = uCards++[new] -- List of cards in hand with the drawed card included

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds _ _ cards = bestMeld cards 

-- ==============================( Helper Functions (Meld) )==============================
-- ++++++++++++++++++++( Best meld )++++++++++++++++++++
-- | Function which sorts a list of cards by suit first then rank
sortRS :: [Card] -> [Card]
sortRS hand = sortOn getRank $ sortOn getSuit hand 

-- | Function which finds the combination of melds with the least deadwood cards
bestMeld :: [Card] -> [Meld] 
bestMeld hand = choose (findMeldsA (sortRS hand)) (findMeldsB (sortRS hand)) 
    (findMeldsC (sortRS hand)) (findMeldsD (sortRS hand)) (findMeldsE (sortRS hand))
    where
        choose a b c d e
            | dwCount a == foldl1 min (map dwCount [a,b,c,d,e]) = a
            | dwCount b == foldl1 min (map dwCount [a,b,c,d,e]) = b
            | dwCount c == foldl1 min (map dwCount [a,b,c,d,e]) = c
            | dwCount d == foldl1 min (map dwCount [a,b,c,d,e]) = d
            | otherwise = e

-- | Function which counts the number of deadwood cards in a meld
dwCount :: [Meld] -> Int
dwCount melds = foldl1 (+) $ map (dwcheck) melds
    where
        dwcheck (Deadwood _) = 1 
        dwcheck _ = 0

-- ++++++++++++++++++++( Meld scenarios )++++++++++++++++++++
-- | Scenario A (When finding straights first is the better option) #Largest meld
findMeldsA :: [Card] -> [Meld]
findMeldsA [] = []
findMeldsA (x:[]) = arrayToMelds [[x]] 
findMeldsA cards
    | checkStraight (suitF 5 cards) = arrayToMelds ([suitF 5 cards]) ++ findMeldsA (suitFT 5 cards) 
    | checkStraight (suitF 4 cards) = arrayToMelds ([suitF 4 cards]) ++ findMeldsA (suitFT 4 cards) 
    | checkStraight (suitF 3 cards) = arrayToMelds ([suitF 3 cards]) ++ findMeldsA (suitFT 3 cards) 
    | checkSet (rankF 4 cards) = arrayToMelds ([rankF 4 cards]) ++ findMeldsA (rankFT 4 cards)
    | checkSet (rankF 3 cards) = arrayToMelds ([rankF 3 cards]) ++ findMeldsA (rankFT 3 cards)
    | otherwise = arrayToMelds ([[head cards]]) ++ findMeldsA (tail cards)

-- | Scenario B (When finding sets first is the better option) #Largest meld
findMeldsB :: [Card] -> [Meld]
findMeldsB [] = []
findMeldsB (x:[]) = arrayToMelds [[x]] 
findMeldsB cards
    | checkSet (rankF 4 cards) = arrayToMelds ([rankF 4 cards]) ++ findMeldsB (rankFT 4 cards)
    | checkSet (rankF 3 cards) = arrayToMelds ([rankF 3 cards]) ++ findMeldsB (rankFT 3 cards)
    | checkStraight (suitF 5 cards) = arrayToMelds ([suitF 5 cards]) ++ findMeldsB (suitFT 5 cards) 
    | checkStraight (suitF 4 cards) = arrayToMelds ([suitF 4 cards]) ++ findMeldsB (suitFT 4 cards) 
    | checkStraight (suitF 3 cards) = arrayToMelds ([suitF 3 cards]) ++ findMeldsB (suitFT 3 cards) 
    | otherwise = arrayToMelds ([[head cards]]) ++ findMeldsB (tail cards)

-- | Scenario C (When the first/second straight of 5 holds a card the second/third meld requires)
findMeldsC :: [Card] -> [Meld]
findMeldsC [] = []
findMeldsC (x:[]) = arrayToMelds [[x]] 
findMeldsC cards
    | checkStraight (suitF 4 cards) = arrayToMelds ([suitF 4 cards]) ++ findMeldsA (suitFT 4 cards) 
    | checkStraight (suitF 3 cards) = arrayToMelds ([suitF 3 cards]) ++ findMeldsA (suitFT 3 cards) 
    | checkSet (rankF 4 cards) = arrayToMelds ([rankF 4 cards]) ++ findMeldsC (rankFT 4 cards)
    | checkSet (rankF 3 cards) = arrayToMelds ([rankF 3 cards]) ++ findMeldsC (rankFT 3 cards)
    | otherwise = arrayToMelds ([[head cards]]) ++ findMeldsC (tail cards)

-- | Scenario C (When the first/second straight of 4 holds a card the second/third meld requires)
findMeldsD :: [Card] -> [Meld]
findMeldsD [] = []
findMeldsD (x:[]) = arrayToMelds [[x]] 
findMeldsD cards
    | checkStraight (suitF 3 cards) = arrayToMelds ([suitF 3 cards]) ++ findMeldsA (suitFT 3 cards) 
    | checkSet (rankF 4 cards) = arrayToMelds ([rankF 4 cards]) ++ findMeldsD (rankFT 4 cards)
    | checkSet (rankF 3 cards) = arrayToMelds ([rankF 3 cards]) ++ findMeldsD (rankFT 3 cards)
    | otherwise = arrayToMelds ([[head cards]]) ++ findMeldsD (tail cards)

-- | Scenario C (When the first/second suit of 4 holds a card the second/third meld requires)
findMeldsE :: [Card] -> [Meld]
findMeldsE [] = []
findMeldsE (x:[]) = arrayToMelds [[x]] 
findMeldsE cards
    | checkSet (rankF 3 cards) = arrayToMelds ([rankF 3 cards]) ++ findMeldsB (rankFT 3 cards)
    | checkStraight (suitF 5 cards) = arrayToMelds ([suitF 5 cards]) ++ findMeldsE (suitFT 5 cards) 
    | checkStraight (suitF 4 cards) = arrayToMelds ([suitF 4 cards]) ++ findMeldsE (suitFT 4 cards) 
    | checkStraight (suitF 3 cards) = arrayToMelds ([suitF 3 cards]) ++ findMeldsE (suitFT 3 cards) 
    | otherwise = arrayToMelds ([[head cards]]) ++ findMeldsE (tail cards)

-- ++++++++++++++++++++( Filter Suit/Rank )++++++++++++++++++++
-- | Function which filters and retrieve the first n cards which has the same rank as the first card
rankF :: Int -> [Card] -> [Card]
rankF _ [] = []
rankF n cards = take n $ filter (rankPred c) cards ++ filter (not . rankPred c) cards
    where c = head cards

-- | Function which retrieve the cards not filtered by rankF
rankFT :: Int -> [Card] -> [Card]
rankFT _ [] = []
rankFT n cards = sortRS $ drop n $ filter (rankPred c) cards ++ filter (not . rankPred c) cards
    where c = head cards

-- | The predicate used in rankF and rankFT
rankPred :: Card -> Card -> Bool
rankPred (Card _ r1) (Card _ r2) = r1 == r2

-- | Function which filters and retrieve the first n cards which has the same suit as the first card
suitF :: Int -> [Card] -> [Card]
suitF _ [] = []
suitF n cards = take n $ filter (suitPred c) cards ++ filter (not . suitPred c) cards
    where c = head cards

-- | Function which retrieve the cards not filtered by suitF
suitFT :: Int -> [Card] -> [Card]
suitFT _ [] = []
suitFT n cards = sortRS $ drop n $ filter (suitPred c) cards ++ filter (not . suitPred c) cards
    where c = head cards

-- | The predicate used in suitF and suitFT
suitPred :: Card -> Card -> Bool
suitPred (Card s1 _) (Card s2 _) = s1 == s2

-- ++++++++++++++++++++( Check melds )++++++++++++++++++++
-- | Function which checks if the cards form a valid straight
-- (All cards in the input must be part of this straight to be valid)
checkStraight :: [Card] -> Bool
checkStraight cards
    | length cards >= 3 && length cards <= 5 = matchMeld cards nextCard
    | otherwise = False

-- | Function which checks if the cards form a valid set 
-- (All cards in the input must be part of this set to be valid)
checkSet :: [Card] -> Bool
checkSet cards
    | length cards >= 3 && length cards <= 4 = matchMeld cards sameRank
    | otherwise = False

-- ++++++++++++++++++++( Form melds )++++++++++++++++++++
-- | Function which transforms arrays of cards which have been validated as
-- being either melds or deadwood
arrayToMelds :: [[Card]] -> [Meld]
arrayToMelds = map (toMeld)

-- | This function will assume the Cards in place are sorted and are VALID melds
-- and will convert the array of cards into the appropriate meld type
toMeld :: [Card] -> Meld
toMeld [a] = Deadwood a
toMeld [a,b,c] = if matchMeld [a,b,c] (sameRank) then Set3 a b c else Straight3 a b c
toMeld [a,b,c,d] = if matchMeld [a,b,c,d] (sameRank) then Set4 a b c d else Straight4 a b c d
toMeld [a,b,c,d,e] = Straight5 a b c d e
toMeld _ = undefined

-- ++++++++++++++++++++( Check cards )++++++++++++++++++++
-- | Function which validates from left to right if all cards meet the criteria provided
-- (left card and right card meet meets a provided condition)
matchMeld :: [Card] -> (Card -> Card -> Bool) -> Bool
matchMeld [] _ = True
matchMeld (_:[]) _ = True
matchMeld (x:xs) predi = length (x:xs) <= 5 && (predi x (head xs)) && matchMeld xs predi

-- | Cards which fit in a set will has all cards when compare with its adjacent card meet
-- the condition below where the next card has the same rank
sameRank :: Card -> Card -> Bool
sameRank a b = getRank a == getRank b

-- | Cards which fits in a straight has all cards when compare with the adjacent card meet
-- the condition below where the next card has the same suit and is 1 rank higher
nextCard :: Card -> Card -> Bool
nextCard a b = case getSuit a == getSuit b of
    True -> (getRank b) > (getRank a) && pred (getRank b) == (getRank a)
    _ -> False

-- ++++++++++++++++++++( Extract card data )++++++++++++++++++++
-- | Function which extracts the suit of a card
getSuit :: Card -> Suit
getSuit (Card s _) = s

-- | Function which extracts the rank of a card
getRank :: Card -> Rank
getRank (Card _ r) = r

-- ==============================( Helper functions (Pick) )==============================
-- ++++++++++++++++++++( Potential melds )++++++++++++++++++++
-- | Function which check if a card has potential to form melds with a deadwood card
-- (potential as in 1 card away from a meld)
potentialMeld :: Card -> [Card] -> Bool
potentialMeld new hand = elem True $ map (twoMeld new) (map (dwCard) (filter (dwOnly) (bestMeld hand))) 
    where
        dwOnly (Deadwood _) = True -- If meld is deadwood (to extract the deadwood cards)
        dwOnly _ = False
        dwCard (Deadwood card) = card -- Extract card in deadwood
        dwCard _ = undefined
    
-- | Function which check if a card is part of a meld
twoMeld :: Card -> Card -> Bool
twoMeld a b
    | sameRank a b = True
    | nextCard a b || nextCard b a = True
    | otherwise = False
-- ++++++++++++++++++++( Parser )++++++++++++++++++++
-- | Parser for suit
parseSuit :: Parser Suit
parseSuit = (is 'D' >> pure Diamond) ||| (is 'C' >> pure Club) 
    ||| (is 'H' >> pure Heart) ||| (is 'S' >> pure Spade) 

-- | Parser for rank
parseRank :: Parser Rank
parseRank = (is 'A' >> pure Ace) ||| (oneof "23456789" >>= pure . toEnum . subtract 1 . digitToInt) 
    ||| (is 'T' >> pure Ten) ||| (is 'J' >> pure Jack) 
    ||| (is 'Q' >> pure Queen) ||| (is 'K' >> pure King)

-- | Parser for card
parseCard :: Parser Card
parseCard = do 
    s <- parseSuit
    r <- parseRank
    pure (Card s r)

-- | Parser for array of cards
parseArray :: Parser [Card]
parseArray = betweenSepbyComma '[' ']' parseCard

-- | Parser for the memory which is of the following format:
-- [[Discarded cards],[Cards opponent rejects],[Cards opponent accepts]]
parseMemory :: Parser [[Card]]
parseMemory = betweenSepbyComma '[' ']' parseArray

-- ++++++++++++++++++++( Conversion )++++++++++++++++++++
-- | Function which converts a string to a card it represents
toCard :: String -> Card
toCard str = case parse parseCard str of
    (Result _ c) -> c
    _ -> undefined -- This is to prevent the terminal warning

-- | Function which converts a string to an array of cards it represents
toArray :: String -> [Card]
toArray str = case parse parseArray str of
    (Result _ c) -> c
    _ -> undefined -- This is to prevent the terminal warning

-- | Function which reads and converts the string representation of the player's memory
toMemory :: String -> [[Card]]
toMemory str = case parse parseMemory str of
    (Result _ c) -> c
    _ -> undefined -- This is to prevent the terminal warning

-- | Function which retrieves the stored score in player's memory
toScore :: String -> String
toScore str = case parse parseMemory str of
    (Result c _) -> c
    _ -> undefined -- This is to prevent the terminal warning

-- | Function which converts a suit to its associated string representation
fromSuit :: Suit -> String
fromSuit Diamond = "D"
fromSuit Heart = "H"
fromSuit Club = "C"
fromSuit Spade = "S" 

-- | Function which converts a rank to its associated string representation
fromRank :: Rank -> String
fromRank Ace = "A"
fromRank Two = "2"
fromRank Three = "3"
fromRank Four = "4"
fromRank Five = "5"
fromRank Six = "6"
fromRank Seven = "7"
fromRank Eight = "8"
fromRank Nine = "9"
fromRank Ten = "T"
fromRank Jack = "J"
fromRank Queen = "Q"
fromRank King = "K"

-- | Function which converts a card to its associated string representation
fromCard :: Card -> String
fromCard (Card s r) = (fromSuit s)++(fromRank r)

-- | Function which converts an array of cards to its associated string representation
fromArray :: [Card] -> String
fromArray [] = "[]"
fromArray cards = "[" ++ (foldl1 (concatComma) (map (fromCard) (cards))) ++ "]"
    where 
        concatComma a b = a ++ "," ++ b

-- | Function which converts the player's memory to its associated string representation
fromMemory :: [[Card]] -> String
fromMemory [] = "[]"
fromMemory memory = "[" ++ (foldl1 (concatComma) (map (fromArray) (memory))) ++ "]"
    where 
        concatComma a b = a ++ "," ++ b

-- | Function which converts a score to its associated string representation
fromScore :: Int -> String
fromScore = show 

-- ==============================( Helper functions (Play) )==============================
-- ++++++++++++++++++++( Find from cards )++++++++++++++++++++
-- | Function which extracts the cards that produces a valid hand for knocking when discarded
extractK :: [Card] -> [Card] -> [Card]
extractK uCards hand = filter (checkKnock . (\x -> (delete (x) (hand)))) uCards

-- | Function which extracts the cards that produces a hand with n number of deadwood cards when discarded
extractN :: Int -> [Card] -> [Card] -> [Card]
extractN n uCards hand = filter ((==) n . (\x -> dwCount( bestMeld( (delete (x) (hand)))))) uCards

-- | Function which check if a hand is valid for knocking (based on the deadwood total value)
checkKnock :: [Card] -> Bool
checkKnock cards = (foldl1 (+)  (map (dwcheck) (bestMeld cards))) < 10
    where
        dwcheck (Deadwood (Card _ r))
            | (fromEnum r) + 1 >= 10 = 10
            | (fromEnum r) + 1 < 10 = (fromEnum r) + 1
        dwcheck _ = 0

-- ++++++++++++++++++++( Find the extreme values for deadwood count )++++++++++++++++++++
-- | Function which counts the minimum number of deadwood cards produced when a card is discarded
-- from hand (Best case)
bestDW :: [Card] -> [Card] -> Int
bestDW uCards hand = minimum $ map (\x -> dwCount( bestMeld( (delete (x) (hand))))) uCards

-- | Function which counts the maximum number of deadwood cards produced when a card is discarded
-- from hand (Worst case)
worstDW :: [Card] -> [Card] -> Int
worstDW uCards hand = maximum $ map (\x -> dwCount( bestMeld( (delete (x) (hand))))) uCards

-- ++++++++++++++++++++( Drop selection )++++++++++++++++++++
-- | Function which finds the appropriate card to discard based on the options and the memory
-- this function ultilizes the information stored in the player's memory to select the best option
bestDrop :: [Card] -> [[Card]] -> Card
bestDrop [oneCard] _ = oneCard
bestDrop (x:xs) [d,r,s]
    | intersect (x:xs) r /= [] = head $ intersect (x:xs) r -- If there is a card the opponent doesn't want
    | intersect (x:xs) s /= [] && (x:xs)\\s /= [] = bestDrop ((x:xs)\\s) [d,r,s] -- If there are cards the opponent wants
    | intersect (x:xs) s /= [] = x -- If all cards are cards the opponent wants
    | any (\y -> sameRank x y) r = x -- If the card is close to a card the opponent rejects (set)
    | any (\y -> nextCard x y || nextCard y x) r = x -- If the card is close to a card the opponent rejects (straight)
    | otherwise = bestDrop xs [d,r,s]
bestDrop _ _ = undefined

-- | Function filters cards which the player wants (deadwood cards which are 1 card away from a meld)
filterWants :: [Card] -> Card -> [Card]
filterWants [oneCard] _  = [oneCard]
filterWants cards new
    |  any (\x -> potentialMeld x ((cards\\[x])++[new])) cards && filteredCards /= [] 
    = filteredCards
    |  otherwise = cards
    where
        filteredCards = filter (\x -> not (potentialMeld x ((cards\\[x])++[new]))) cards

-- | Function which only extracts unique elements in a list
unique :: Ord a => [a] -> [a]
unique = map head . group . sort
-- ==============================( Addtional code )==============================
-- --------------------( Code completed in tutorial 11 )--------------------
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = (>>=) character (\x -> if f x then pure x else unexpectedCharParser x)

oneof :: String -> Parser Char
oneof s = satisfy (\x -> if (elem x s) then True else False)

list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= \v ->
        list p >>= \vs ->
        pure (v:vs)

tok :: Parser a -> Parser a
tok p = p       >>= \x ->
        list (satisfy isSpace) >>= \_ ->
        pure x

charTok :: Char -> Parser Char
charTok c = tok (is c)

commaTok :: Parser Char
commaTok =  charTok ','

between :: Parser o -> Parser c -> Parser a -> Parser a
between p1 p2 p3 = do
  _ <- p1
  x <- p3
  _ <- p2
  return x

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 lp rp = do
        leftRes <- lp
        rightRes <- list((>>) rp lp)
        pure (leftRes:rightRes)

sepby :: Parser a -> Parser s -> Parser [a]
sepby fp sp = sepby1 fp sp ||| pure []

betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok fc sc = between (charTok fc) (charTok sc)

betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma fc sc p = betweenCharTok fc sc (sepby (p) (commaTok))