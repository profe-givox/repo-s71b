data Frame = Open Int Int
	| Spare Int Int
	| Strike Int Int						
	deriving (Eq, Show)

toFrames :: [Int] -> Maybe [Frame] 
toFrames pins = go 1 pins
	where
		go 10 [x, y]
			| x + y < 10 = Just [Open x y]
			| otherwise = Nothing
		go 10 [x, y, z]
			| x == 10 = Just [Strike y z]
			| x + y == 10 = Just [Spare x z]
			| otherwise = Nothing
		go n (x:y:z:ys)
			| x == 10 = fmap (Strike y z :) $ go (n+1) (y:z:ys)
			| x + y == 10 = fmap (Spare x z :) $ go (n+1) (z:ys)
			| x + y < 10 = fmap (Open x y :) $ go (n+1) (z:ys)
			| otherwise = Nothing
		go _ _ = Nothing		




