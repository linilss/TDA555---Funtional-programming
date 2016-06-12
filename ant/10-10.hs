import Haste

main1 :: IO ()
main1 = alert "Hello world"

addchildren :: Elem -> [Elem] -> IO ()
addchildren parent children = sequence_ [addChild c parent | c -> children]

row :: Elem -> [Elem] -> IO ()
row = addchildren

wrapDiv :: Elem -> IO Elem
wrapDiv e = do
	div <- newElem "div"
	addChild e div 
	return div

column :: Elem -> [Elem] -> IO ()
column parent children = do
	cs <- sequence [wrapDiv c | c <- children]
	addchildren parent cs

main2 = do
	text1 <- newTextElem "Some text"
	text2 <- newTextElem "More text"
	text3 <- newTextElem "More text"
	text4 <- newTextElem "More text"
	top <- newElem "div"
	bot <- newElem "div"
	row top [text1,text2]
	row bot [text3,text4]
	column documentBody [top,bot]

main3 = do 
	inp <- mkInput 20 ""
	out <- newElem "div"
	column documentBody [inp,out]
	onEvent inp OnKeyUp $ \c -> do
		text <- getProp inp "value"
		setProp out "innerHTML" text

-- haste.graphics.canvas
-- klistra in mkCanvas från pages
main = do 
	canvas <- mkCanvas 500 500
	addChild canvas documentBody
    
    can <- getCanvas canvas

    render can $ stroke $ line

-- line tar två pkter
-- (sudoku s) mönstermatcha