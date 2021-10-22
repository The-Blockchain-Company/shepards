-- | Utility functions
module Test.Shepard.Util where


-- | `<$>` through nested functors.
(<$$>) :: (Functor f, Functor s)
       => (a -> b) -> f (s a) -> f (s b)
(<$$>) = fmap . fmap

-- | `<*>` through nested functors.
(<**>) :: (Applicative f, Applicative s)
       => f (s (a -> b)) -> f (s a) -> f (s b)
(<**>) f x = (\s y -> s <*> y) <$> f <*> x
