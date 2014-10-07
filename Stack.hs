module Stack where

type Offset = Int

data Stack a = Stack 
    { stackSize :: Int
    , stackItems :: [a] }

empty :: Stack a
empty = Stack 
        { stackSize = 0
        , stackItems = [] }

isEmpty :: Stack a -> Bool
isEmpty = (==) 0 . stackSize

offset :: (Eq a) => a -> Stack a -> Maybe Offset
offset a s = off (stackItems s)
    where off [] = Nothing
          off (i:items)
              | i == a    = return 0
              | otherwise = do { n <- off items; return $ n+1 }

push :: a -> Stack a -> Stack a
push a s = s { stackSize = 1 + stackSize s
             , stackItems = a : stackItems s }

pop :: Stack a -> Stack a
pop s = s { stackSize = stackSize s - 1
          , stackItems = tail (stackItems s) }
