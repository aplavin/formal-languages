{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where

import Data.List

thrd (_, _, a) = a
			
class FunnyShow a b where
    surround :: a -> a -> b -> String

instance FunnyShow Char Char where
    surround prefix suffix char = [prefix, char, suffix]

instance FunnyShow Char String where
    surround prefix suffix str = [prefix] ++ str ++ [suffix]

instance FunnyShow String Char where
    surround prefix suffix char = prefix ++ [char] ++ suffix

instance FunnyShow String String where
    surround prefix suffix str = prefix ++ str ++ suffix