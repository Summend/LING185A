module Assignment02 where

data Numb = Z | S Numb deriving Show

add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}

one, two, three, four, five, six :: Numb
one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five

data NumbList = EmptyNL | NonEmptyNL Numb NumbList deriving Show

list0, list1, list2 :: NumbList
list0 = NonEmptyNL one (NonEmptyNL two (NonEmptyNL three EmptyNL))
list1 = NonEmptyNL four (NonEmptyNL Z (NonEmptyNL two EmptyNL))
list2 = NonEmptyNL six (NonEmptyNL one (NonEmptyNL three (NonEmptyNL four EmptyNL)))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

sumUpTo :: Numb -> Numb
sumUpTo n =
	case n of
		Z -> Z
		S n' -> add (S n') (sumUpTo n')

equal :: Numb -> Numb -> Bool
equal n m = 
	case m of
		Z -> case n of
			Z -> True
			S n' -> False
		S m' -> case n of
			Z -> False
			S n' -> equal m' n'

difference :: Numb -> Numb -> Numb
difference n m =
	case m of
		Z -> n
		S m' -> case n of
			Z -> m
			S n' -> difference n' m'

size :: NumbList -> Numb
size list =
	case list of
		EmptyNL -> Z
		NonEmptyNL num list' -> S (size list')

lastElement :: NumbList -> Numb
lastElement list = 
	case list of
		EmptyNL -> Z
		NonEmptyNL n list' -> case list' of
							NonEmptyNL n' list2 -> lastElement list'
							EmptyNL -> n

total :: NumbList -> Numb
total list =
	case list of
		EmptyNL -> Z
		NonEmptyNL num list' -> add num (total list')

incrementAll :: Numb -> NumbList -> NumbList
incrementAll n list =
	case n of
		Z -> list
		S n' -> case list of
			EmptyNL -> EmptyNL
			NonEmptyNL num list' -> NonEmptyNL (add (S n') num) (incrementAll n list')

contains :: (Numb -> Bool) -> NumbList -> Bool
contains f list =
	case list of
		EmptyNL -> False
		NonEmptyNL num list' -> case (f num) of
							True -> f num
							False -> contains f list'

remove :: (Numb -> Bool) -> NumbList -> NumbList
remove f list =
	case list of
		EmptyNL -> EmptyNL
		NonEmptyNL num list' -> case (f num) of
							True -> remove f list'
							False -> NonEmptyNL num (remove f list')

append :: NumbList -> NumbList -> NumbList
append list_1 list_2 = 
	case list_1 of
		EmptyNL -> list_2
		NonEmptyNL num1 list_1' -> case list_1' of
							EmptyNL -> NonEmptyNL num1 (list_2)
							NonEmptyNL num1' list_1'' -> NonEmptyNL num1 (append list_1' list_2)

prefix :: Numb -> NumbList -> NumbList
prefix n list =
	case n of
		Z -> EmptyNL
		S n' -> case list of
			EmptyNL -> EmptyNL
			NonEmptyNL num list' -> NonEmptyNL num (prefix n' list')


