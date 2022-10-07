module RecList where

(+++) :: [a] -> [a] -> [a]

[] +++ x = x
(x:xs) +++ y = x : xs +++ y
