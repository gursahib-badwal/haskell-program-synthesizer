{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Synthesizer 
    (numberSplit
    ,baseExpressionsAtSize
    ,varExpressionsAtSize
    ,notExpressionsAtSize
    ,andExpressionsAtSize
    ,orExpressionsAtSize
    ,expressionsAtSize
    ,expressionSatisfiesExamples
    ,generator
    )
     where

import Language
import Data.List
import Data.Maybe

numberSplit :: Int -> [(Int,Int)]
-- numberSplit = error "Unimplemented"
numberSplit num = [(x, num-x) | x <- numbers]
  where 
    numbers = [1..(num-1)]

{-  This should only return a nonempty list at size 1.
    At size 1, it should return a list consisting of the two base expressions
-}
baseExpressionsAtSize :: Int -> [Expression]
-- baseExpressionsAtSize = error "Unimplemented"
baseExpressionsAtSize size 
    | size == 1 = [EBase True, EBase False]
    | otherwise = []


{-  This should only return a nonempty list at size 1.
    At size 1, it should return a list consisting of the variable expressions

    HINT: fmap will be useful here.
-}
varExpressionsAtSize :: Context -> Int -> [Expression]
-- varExpressionsAtSize = error "Unimplemented"
varExpressionsAtSize (Context context) 1 = create_variable_expressions context
                                where
                                    create_variable_expressions:: [String] -> [Expression]
                                    create_variable_expressions context = create_variable <$> context
                                     where 
                                        create_variable::String -> Expression
                                        create_variable string = EVariable string
varExpressionsAtSize _ _ = []



{-  At size 0, it should return an empty list.
    At other sizes, it should call the provided function to get expressions of
    a given size. The resulting expression size should be n and should be a
    "not" expression.

    HINT: fmap will be useful here.
-}
notExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
-- notExpressionsAtSize = error "Unimplemented"
notExpressionsAtSize _ 0 = []
notExpressionsAtSize func n = ENot <$> func (n-1)

{-  At size 0, it should return an empty list.
    At other sizes, it should call the provided function to get expressions of
    given sizes. The resulting expression size should be n and should be a
    "and" expression.

    TO GET FULL CREDIT, YOU MUST USE DO SYNTAX WITH THE LIST MONAD.

    HINT: numbersplit will be useful here.
-}
andExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
andExpressionsAtSize f 0 = []
andExpressionsAtSize f n = do
        -- error "Unimplemented"
    (lsize, rsize) <- numberSplit(n-1)
    lexpression <- f lsize 
    rexpression <- f rsize  
    return (EAnd (lexpression, rexpression))  

    

{-  At size 0, it should return an empty list.
    At other sizes, it should call the provided function to get expressions of
    given sizes. The resulting expression size should be n and should be an
    "or" expression.

    TO GET FULL CREDIT, YOU MUST USE DO SYNTAX WITH THE LIST MONAD.

    HINT: numbersplit will be useful here.
-}
orExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
orExpressionsAtSize f 0 = []
orExpressionsAtSize f n = do
    -- error "Unimplemented"
    (lsize, rsize) <- numberSplit(n-1) 
    lexpression <- f lsize 
    rexpression <- f rsize  
    return (EOr (lexpression, rexpression)) 


{-  This should simply call andExpressionsAtSize, orExpressionsAtSize,
    notExpressionsAtSize, varExpressionsAtSize, and baseExpressionsAtSize,
    with the appropriate arguments, and concatenate the results.
-}
expressionsAtSize :: Context -> Int -> [Expression]
-- expressionsAtSize = error "Unimplemented"
expressionsAtSize context 0 = []
expressionsAtSize context 1 = baseExpressionsAtSize 1 ++ varExpressionsAtSize context 1
expressionsAtSize context size = concat [not, or, and]
                                 where 
                                    not = notExpressionsAtSize (expressionsAtSize context) size
                                    or = orExpressionsAtSize (expressionsAtSize context) size
                                    and = andExpressionsAtSize (expressionsAtSize context) size

{-  Check whether a given expression satisfies the provided examples.

    HINT: the "all" function will be useful here.
-}
expressionSatisfiesExamples :: Examples -> Expression -> Bool
-- expressionSatisfiesExamples = error "Unimplemented"
expressionSatisfiesExamples (Examples examples) expression =
    all (check_assignment expression) examples
    where 
        check_assignment:: Expression -> (Assignment, Bool) -> Bool
        check_assignment  expression (assignment, expected_bool) = evaluate assignment expression == expected_bool


{-  Generate an expression that satisfies the examples. Check if there are 
    examples at size 1, then at size 2, ... until either there are no 
    expressions at size max or until an expression is found that satisfies the
    examples.

    HINT: Use a helper function
    HINT: The "find" function will be useful here
    HINT: The "evaluate" function will be useful here
-}
generator :: Context -> Examples -> Int -> Maybe Expression
-- generator = error "Unimplemented"
generator context examples max = 
    generate context examples 1 
    where
        generate:: Context -> Examples -> Int -> Maybe Expression
        generate context examples_list current_size
            | current_size > max = Nothing
            | otherwise = let result = find (expressionSatisfiesExamples examples_list) (expressionsAtSize context current_size)
                          in case result of 
                                Just expression -> Just expression 
                                _ -> generate context examples (current_size + 1) 

 









