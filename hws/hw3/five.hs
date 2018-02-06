-- Problem 5

-- bonus problem is after line 61

--- DFA definition

data DFA = DFA Int [Int] (Int -> Char -> Int)

-- getNumOfStates :: DFA -> Int
-- getNumOfStates (DFA s _ _) = s
--
-- getFiniteStates :: DFA -> [Int]
-- getFiniteStates (DFA _ s _) = s
--
-- getTransitionFunction :: DFA -> (Int -> Char -> Int)
-- getTransitionFunction (DFA _ _ f) = f

match :: DFA -> String -> Bool
match _ [] = error "Pass the string"
match (DFA numberOfStates finiteStates transition) str = helper str 0 -- zero is the initial state
  where
    helper _ (-1) = False -- спираме автомат
    helper [] state = elem state finiteStates
    helper (x:xs) state = helper xs (transition state x)

--
-- tests
--

trans :: Int -> Char -> Int
trans 0 'a' = 1
trans 0 'b' = 0
trans 0 'c' = 0
trans 1 'a' = 0
trans 1 'b' = 1
trans 1 'c' = 1
trans _  _  = -1 -- невалидното състояние
                 -- с тази клауза гарантираме тоталност на функцията

dfa1 :: DFA
dfa1 = DFA 2 [0] trans -- 2 състояния (с номера 0 и 1), измежду които само 0 е крайно

checkDfa1 = all (==True)
            [match dfa1 "abcbcab" == True
            , match dfa1 "baba" == True
            , match dfa1 "baobab" == False
            , match dfa1 "aaaaa" == False] -- :: [Bool]

---------
---------

dfa2 :: DFA
dfa2 = DFA 5 [4] (const . min 4 . (+1)) -- кой език разпознава този автомат?

checkDfa2 = all (==True)
            [match dfa2 "iei" == False,
             match dfa2 "ieei" == True] -- :: [Bool]


---
--- BONUS:
---

data NFA = NFA Int [Int] (Int -> Char -> [Int])

match' :: NFA -> String -> Bool
match' _ [] = error "Pass the string"
match' (NFA numberOfStates finiteStates transition) str = helper str [0] -- zero is the initial state
  where
    helper :: [Char] -> [Int] -> Bool
    helper _ [-1] = False -- спираме автомат
    helper [] states = any (`elem` finiteStates) states
    helper (x:xs) states = any (==True) $ map (\state -> helper xs (transition state x)) states -- if we have at least 1 way to go to the finit state
    -- helper (x:xs) states = any (==True) $ foldr (\state acc -> (helper xs (transition state x)):acc) [] states


--
-- test NFA
--

trans' :: Int -> Char -> [Int]
trans' 0 '0' = [0,1]
trans' 0 '1' = [1]
trans' 1 '1' = [2]
trans' 2 '0' = [2]
trans' 2 '1' = [2]
trans' _  _  = [(-1)]

nfa1 :: NFA
nfa1 = NFA 3 [2] trans' -- 2 състояния (с номера 0 и 1), измежду които само 0 е крайно
-- -> (0):  0 -> (0)
--        0,1 -> (1) : 1 -> (2) : 0 -> (2)
--                                1 -> (2)

checkNfa1 = all (==True)
            [match' nfa1 "0011" == True,
             match' nfa1 "000000000" == False,
             match' nfa1 "0000000001" == True,
             match' nfa1 "01" == True,
             match' nfa1 "01000000000" == True,
             match' nfa1 "01111111111" == True,
             match' nfa1 "01010101010" == True] -- :: [Bool]
