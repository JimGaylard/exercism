module Robot (Robot, mkRobot, resetName, robotName) where

import qualified System.Random as R
import Data.Char (toUpper, intToDigit)

data Robot = Robot { name :: String } deriving Show

mkRobot :: IO Robot
mkRobot = do
  name' <- mkName
  return $ Robot name'

mkName :: IO String
mkName = do
  gen <- R.newStdGen
  let letters = take 2 $ map toUpper $ R.randomRs ('a', 'z') gen :: String
      numbers = take 3 $ R.randomRs (0, 9) gen :: [Int] in
        return $ letters ++ map intToDigit numbers

resetName :: Robot -> IO ()
resetName x = do
  name' <- mkName
  pure x { name = name' }
  return ()

robotName :: Robot -> IO String
robotName r = return $ name r
