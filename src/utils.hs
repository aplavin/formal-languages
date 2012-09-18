{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where

import Data.List
import Data.List.Split

splitOn x = split (dropBlanks . dropDelims $ oneOf [x])
splitOneOf xs = split (dropBlanks . dropDelims $ oneOf xs)
splitWhen f = split (dropBlanks . dropDelims $ whenElt f)

stripOneOf xs = lstripOneOf xs . rstripOneOf xs
lstripOneOf xs = dropWhile (`elem` xs)
rstripOneOf xs = reverse . lstripOneOf xs . reverse


strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

thrd (_, _, a) = a

cartesianProduct cnt xs = sequence (replicate cnt xs)

count x xs = length $ filter (==x) xs

allEqual [] = True
allEqual (x:xs) = all (==x) xs

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