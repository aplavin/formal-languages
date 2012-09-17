{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where

import Data.List

thrd (_, _, a) = a

justifyRight n c s = replicate (n - length s) c ++ s

justifyLeft n c s = replicate (n - length s) c ++ s

justifyCenter n c s = replicate l c ++ s ++ replicate r c where
	l = (n - length s) `quot` 2
	r = n - l - length s
			
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