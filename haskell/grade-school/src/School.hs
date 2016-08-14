module School (School(..), add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List (sort)

-- The task is to create the data type `School` and
-- implement the functions below.

type School = M.Map Grade [Student]
type Grade = Int
type Student = String

add :: Grade -> Student -> School -> School
add g s = M.insertWith (++) g [s]

empty :: School
empty = M.empty

grade :: Grade -> School -> [Student]
grade = M.findWithDefault []

sorted :: School -> [(Grade, [Student])]
sorted s = map f s' where
  f (g, ss) = (g, sort ss)
  s' = M.toAscList s
