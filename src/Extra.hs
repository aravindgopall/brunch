module Extra where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mB m1 m2 = do
  b <- mB
  if b
    then m1
    else m2
