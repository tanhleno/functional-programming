module ListaInt where

data ListaInt = Empty | ListaInt Int (ListaInt)

instance Show (ListaInt) where
    show Empty              = "[]"
    show (ListaInt x Empty) = "[" ++ (show x) ++ "]"
    show (ListaInt x y)     = "[" ++ (show x) ++ ", " ++ (tail $ show y) 

ltail :: ListaInt -> ListaInt
ltail Empty          = error "List is empty."
ltail (ListaInt x y) = y

lhead :: ListaInt -> Int
lhead Empty          = error "List is empty."
lhead (ListaInt x y) = x
