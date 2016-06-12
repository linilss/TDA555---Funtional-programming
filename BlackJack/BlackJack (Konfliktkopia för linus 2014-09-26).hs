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
aCard :: Card
aCard = Card Ace Spades

aCard1 :: Card
aCard1 = Card Ace Hearts

aCard2 :: Card
aCard2 = Card King Diamonds

aHand :: Hand
aHand = Add aCard1 (Add aCard2 Empty)

aHand1 :: Hand
aHand1 = Add (Card { rank = Numeric 2, suit = Hearts })
        (Add (Card { rank = Jack, suit = Spades }) Empty)

aHand2 :: Hand 
aHand2 = Add aCard1 (Add aCard2 Empty)

empty :: Hand
empty = Empty

valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank King = 10
valueRank Queen = 10
valueRank Jack = 10
valueRank (Numeric x) = x

valueCard :: Card -> Integer
valueCard (Card rank suit) = valueRank rank

valueHand :: Hand -> Integer
valueHand Empty  	  = 0
valueHand (Add card hand) = valueCard card + valueHand hand

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

numberOfAces :: Hand -> Integer
numberOfAces (Add (Card Ace suit) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand )       	    = numberOfAces hand
numberOfAces  Empty 	                = 0

value :: Hand -> Integer
value h1 
 | valueHand h1 >  21  = valueHand h1 - (numberOfAces h1 ) * 10
 | valueHand h1 <= 21 = valueHand h1

winner :: Hand -> Hand -> Player
winner h1 h2
 | valueHand h1 > 21 		    = Bank
 | valueHand h2 > 21 		    = Guest
 | valueHand h1 <= valueHand h2 = Bank
 | valueHand h1 >  valueHand h2 = Guest

(<+) :: Hand -> Hand -> Hand
Empty <+ h2 = h2
h1 <+ Empty = h1
Add card hand <+ h2 = Add card (hand <+ h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc h1 h2 h3 = h1 <+ (h2 <+ h3) == (h1 <+ h2) <+ h3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

-- functions puts a complete deck of cards together. Since I created a listToHand function, 
-- I used it in the upper fullSuit to gain a full hand in a simplier way. 
fullSuit :: Suit -> Hand
fullSuit s = listToHand [Card r s | r <- [Ace , Jack , Queen, King ]++[Numeric n | n <- [2..10]]]

listToHand :: [ Card ] -> Hand 
listToHand [] = Empty
listToHand (c:rest) = (Add c (listToHand rest)) 

fullDeck :: Hand
fullDeck = fullSuit Hearts 
		<+ fullSuit Diamonds 
		<+ fullSuit Spades 
		<+ fullSuit Clubs

sFD :: Hand
sFD = shuffle (mkStdGen 12312321 ) fullDeck

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)	

playBank :: Hand -> Hand
playBank d = playBank' d Empty

playBank' :: Hand -> Hand -> Hand
playBank' d b 	
  | value b >= 16 	= b  
  | otherwise 		= playBank' d' b'
  where (d', b') 	= draw d b 

shuffle :: StdGen -> Hand -> Hand
shuffle g d 
	| d == Empty = Empty
	| otherwise = (Add (pickC d n')( shuffle g' (rmvC d n')))	
		where 
      (n', g') = randomR (1, size d) g

rmvC :: Hand -> Integer -> Hand
rmvC (Add c h) 1 = h
rmvC (Add c h) n = (Add c (rmvC h (n-1)))

pickC :: Hand -> Integer -> Card
pickC (Add c h) 1 = c
pickC (Add c h) n = pickC h (n-1)
 
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty    = False
c `belongsTo` Add c' h = c == c' || c `belongsTo` h

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)


implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = sFD
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation
