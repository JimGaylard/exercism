module Robot (Robot, mkRobot, resetName, robotName) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import System.Random (newStdGen, randomRs)

-- The task is to create the data type `Robot`, as a
-- mutable variable, and implement the functions below.

newtype Robot = Robot { name :: String }

mkRobot :: IO (IORef Robot)
mkRobot = Robot <$> mkRobotName >>= newIORef

mkRobotName :: IO String
mkRobotName = do
  letters <- (take 2 . randomRs ('A','Z')) <$> newStdGen
  numbers <- (take 3 . randomRs ('0', '9')) <$> newStdGen
  return (letters ++ numbers)

resetName :: IORef Robot -> IO ()
resetName rr = do
  newName <- mkRobotName
  modifyIORef rr (\r -> r { name = newName })

robotName :: IORef Robot -> IO String
robotName rr = name <$> readIORef rr
