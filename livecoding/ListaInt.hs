module ListaInt where

data ListaInt = Empty | Cons Int ListaInt

instance Show ListaInt where
    show x = "[" ++ show' x ++ "]"

show' :: ListaInt -> [Char]
show' Empty          = ""
show' (Cons x Empty) = show x
show' (Cons x y)     = show x ++ "," ++ show' y

ltail :: ListaInt -> ListaInt
ltail Empty      = error "List is empty."
ltail (Cons x y) = y

lhead :: ListaInt -> Int
lhead Empty      = error "List is empty."
lhead (Cons x y) = x
