module ListaInt where

data Lista a = Empty | Cons a (Lista a)

instance Show a => Show (Lista a) where
    show x = "[" ++ show' x ++ "]"

show' :: Show a => Lista a -> [Char]
show' Empty          = ""
show' (Cons x Empty) = show x
show' (Cons x xs)    = show x ++ "," ++ show' xs

ltail :: Lista a -> Lista a
ltail Empty       = error "List is empty."
ltail (Cons _ xs) = xs

lhead :: Lista a -> a
lhead Empty      = error "List is empty."
lhead (Cons x _) = x
