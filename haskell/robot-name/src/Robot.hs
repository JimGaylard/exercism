module Robot (Robot, mkRobot, resetName, robotName) where

import qualified System.Random as R
import Data.Char (toUpper, intToDigit)

data Robot = Robot String deriving Show

mkRobot :: IO Robot
mkRobot = do
  gen <- R.newStdGen
  let letters = take 2 $ map toUpper $ R.randomRs ('a', 'z') gen :: String
      numbers = take 3 $ R.randomRs (0, 9) gen :: [Int] in
        return $ Robot (letters ++ map intToDigit numbers)

resetName :: Robot -> IO ()
resetName = undefined

robotName :: Robot -> IO String
robotName (Robot s) = return s
