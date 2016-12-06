module Robot (Robot, mkRobot, resetName, robotName) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import System.Random (newStdGen, randomRs)

-- The task is to create the data type `Robot`, as a
-- mutable variable, and implement the functions below.

type Robot = IORef String

mkRobot :: IO Robot
mkRobot = mkRobotName >>= newIORef

mkRobotName :: IO String
mkRobotName = do
  letters <- (take 2 . randomRs ('A','Z')) <$> newStdGen
  numbers <- (take 3 . randomRs ('0', '9')) <$> newStdGen
  return (letters ++ numbers)

resetName :: Robot -> IO ()
resetName rr = do
  newName <- mkRobotName
  modifyIORef rr $ const newName

robotName :: Robot -> IO String
robotName = readIORef
