module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck
import System.Random

--Load many modules
-- Task A
{- Task a
By using the constructor for Hand, you can split the hand in two parts:
 one card plus the rest of the hand. The one card is not used by the function itself,
 meanwhile the rest is used to run the function recursively again and again until 
 the remaining hand is Empty. 
size aHand1
	  = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
	  = 1 + size(Add (Card Jack Spades) Empty)
	  = 1 + (1 + size(Empty))
	  = 1 + 1 + (0)
	  = 2
-}
aCard1 :: Card
aCard1 = Card Ace Spades

aCard2 :: Card
aCard2 = Card Ace Hearts

aCard3 :: Card
aCard3 = Card King Diamonds

aHand :: Hand
aHand = Add aCard1 (Add aCard3 Empty)

aHand1 :: Hand
aHand1 = Add (Card { rank = Numeric 2, suit = Hearts })
       (Add (Card { rank = Jack, suit = Spades }) Empty)

-- A Hand that will make the player lose since the value is <22, by stating that Ace = 11
aHand2 :: Hand
aHand2 = Add aCard1 (Add aCard2 Empty)

-- A Hand that will make the player have the maximum value of a hand = 21
aHand3 :: Hand
aHand3 = Add aCard1 (Add aCard3 Empty)

empty :: Hand
empty = Empty
-- Cards Value







-- valueRank defines the ranks for our cards we're work with
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank King = 10
valueRank Queen = 10
valueRank Jack = 10
valueRank (Numeric x) = x

-- valueCard takes a card and returns the rank of the given card
valueCard :: Card -> Integer
valueCard (Card rank suit) = valueRank rank

-- valueHand calculates the total rank of a hand by going through each card's rank
valueHand :: Hand -> Integer
valueHand Empty  	  = 0
valueHand (Add card hand) = valueCard card + valueHand hand

-- gameOver checks a given hand if it's rank is more than 21 
gameOver :: Hand -> Bool
gameOver hand = value hand > 21


numberOfAces :: Hand -> Integer
numberOfAces (Add (Card Ace suit) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand )      = numberOfAces hand
numberOfAces  Empty 	                = 0

-- (oopsie)
-- value calculates the REAL value of the hand, taking the
-- number of aces in consideration
value :: Hand -> Integer
value h1 
 | valueHand h1 > 21 = valueHand h1 - (numberOfAces h1 ) * 10
 | valueHand h1 <= 21 = valueHand h1

-- Calculates which hand is the winner. h1 is the guest's hand and h2 is the bank's hand
winner :: Hand -> Hand -> Player
winner h1 h2
 | valueHand h1 > 21 		   = Bank
 | valueHand h2 > 21 		   = Guest
 |valueHand h1 <= valueHand h2 = Bank
 |valueHand h1 >  valueHand h2 = Guest

-- infix operator to merge hands
(<+) :: Hand -> Hand -> Hand
Empty <+ h2 = h2 -- If hand's empty, return h2 
h1 <+ Empty = h1
(Add card hand) <+ h2 = (Add card (hand <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc h1 h2 h3 = h1 <+ (h2 <+ h3) == (h1 <+ h2) <+ h3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

-- functions puts a complete deck of cards together. Since I created a listToHand function, 
-- I used it in the upper fullSuit to gain a full hand in a simplier way. 
fullSuit :: Suit -> Hand
fullSuit s = listToHand [Card r s | r <- [Ace , Jack , Queen, King ,
  Numeric 2, Numeric 3, Numeric 4, Numeric 5, Numeric 6, Numeric 7, Numeric 8, Numeric 9]] 
										 --, s <- [Hearts, Diamonds, Clubs, Spades]

{-
fullSuit :: Suit -> Hand
fullSuit s = (Add (Card Ace s) 
			(Add (Card King s) 
			(Add (Card Queen s) 
			(Add (Card Jack s) 
			(Add (Card (Numeric 10) s) 
			(Add (Card (Numeric 9) s) 
			(Add (Card (Numeric 8) s) 
			(Add (Card (Numeric 7) s) 
			(Add (Card (Numeric 6) s) 
			(Add (Card (Numeric 5) s) 
			(Add (Card (Numeric 4) s) 
			(Add (Card (Numeric 3) s) 
			(Add (Card (Numeric 2) s) 
			Empty )))))))))))))
-}

fullDeck :: Hand
fullDeck = fullSuit Hearts 
		<+ fullSuit Diamonds 
		<+ fullSuit Spades 
		<+ fullSuit Clubs

sFD :: Hand
sFD = shuffle (mkStdGen 12312321) fullDeck

-- function that is used to draw the first card from a given deck to a given hand 
-- and then it returns both the deck minus the card and the hand with the drawn card
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, ((Add card Empty) <+ hand))

playBank :: Hand -> Hand
playBank d = playBank' d Empty

playBank' :: Hand -> Hand -> Hand
playBank' d b 	
  | gameOver b 	= b
  | value b >= 16 	= b  
  | otherwise 		= playBank' (fst (d', b'))  (snd (d', b'))
  where (d', b') 	= draw d b 

shuffle :: StdGen -> Hand -> Hand
shuffle g d | d == Empty = Empty
			| otherwise = (Add (pickC d n') (rmvC d n'))
	where (n', g' ) = randomR (1,52) g

 
rmvC :: Hand -> Integer -> Hand
rmvC (Add c h) 1 = h
rmvC (Add c h) n = (Add c (rmvC h (n-1)))
 
pickC :: Hand -> Integer -> Card
pickC (Add c h) 1 = c
pickC (Add c h) n = pickC h (n-1)
 
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty    = False
c `belongsTo` Add c' h = c == c' || c `belongsTo` h

handToList :: Hand -> [Card]
handToList Empty  = []
handToList (Add c hand) = c: (handToList hand)

listToHand :: [ Card ] -> Hand 
listToHand [] = Empty
listToHand (c:rest) = (Add c (listToHand rest)) 

-- these don't work.. any clues?
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)


implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation

{-
--(((((((((((
shuffledFullDeck = handToList fullDeck

shuffleList :: StdGen -> [Card] -> [Card]
shuffleList _ [] = []
shuffleList g lc = ((lc !! (i-1)):(shuffleList newG aulc)) 
 	where  
 i = fst (randomR (1, (length lc)) g)
 newG = snd (randomR (1, (length lc)) g)
 aulc = (take (i-1) lc) ++ (drop i lc)

shuffledDeckList = shuffleList (mkStdGen 1239712) (handToList fullDeck)



-- prop_size_shuffle :: StdGen -> Hand -> Bool

    --))))))))))))))))
-}