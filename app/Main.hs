module Main (main) where

import System.Console.Haskeline

type MatureTime = Int
type InterestRate = Double
type RateChangePerDay = Double
type BillData = (MatureTime, InterestRate, RateChangePerDay)
type RateComparison = (MatureTime, InterestRate)


weeksToMaturity :: [MatureTime]
weeksToMaturity = [4, 8, 13, 26, 52]

someData = [(4,1.1,3.0e-2),(8,1.2,2.5e-2),(13,1.4,2.6e-2),(26,1.6,2.3e-2),(52,1.9,2.0e-2)]


main :: IO ()
main = do
  billData <- getBillData weeksToMaturity
  print billData
  printData billData



getBillData :: [MatureTime] -> IO( [BillData] )
getBillData matureTimes = do
  let noBillData = pure [] :: IO([BillData])
  foldr (\matTime billData -> (fmap (:) (getBillDatum matTime)) <*> billData) noBillData matureTimes



getBillDatum :: Int -> IO (BillData)
getBillDatum matTime = do 
  runInputT defaultSettings querry 
    where querry = do outputStrLn ("For " ++ show matTime ++ "-week t-bill enter values for")
                      Just inpt <- getInputLine "\nInterest rate:  "
                      let intRate = read inpt :: Double
                      Just inpt2 <- getInputLine "Change in interest rate per day:  "
                      let dailyRateChange = read inpt2 :: Double
                      return (matTime, intRate, dailyRateChange)
                              


printData :: [BillData] -> IO ()
printData theData = do putStrLn "The current T-Bill interest rates and change in interest rates per day are:\n:"
                       putStrLn "Bill      Rate      Rate Change per Day"
                       printEachDatum theData
  where printEachDatum [] = putStrLn "" 
        printEachDatum ((w, rate, deltaRate) : moreData) = do putStrLn (show w ++ "-week   " ++ show rate ++ "      " ++ show deltaRate)
                                                              printEachDatum moreData
                                  


interestWithReinvestment:: BillData -> Int -> Double
interestWithReinvestment (weeks, intRate, deltaRate) reinvestWeeks = 100 * ( factorForWholePeriods * factorForFracPeriods - 1.0) * 365.0 / fromIntegral (reinvestWeeks * 7)
  where  wholePeriods = fromIntegral (reinvestWeeks `div` weeks)
         fracPeriod = fromIntegral reinvestWeeks / fromIntegral weeks - wholePeriods 
         factorAtMaturity = 0.01 * intRate * ( fromIntegral weeks * 7.0 / 365.0 ) + 1.0   
         factorChangeAtMaturity = 0.01 * (7.0 * fromIntegral weeks * deltaRate ) *  ( fromIntegral weeks * 7.0 / 365.0 ) 
         factorForWholePeriods = foldr ( \n fact -> fact * (factorAtMaturity + n * factorChangeAtMaturity))  1 [0 .. wholePeriods -1] 
         factorForFracPeriods = ( (factorAtMaturity + wholePeriods * factorChangeAtMaturity - 1)* fracPeriod + 1)
                         

tryHaskeline :: IO(String)
tryHaskeline = do
  runInputT defaultSettings querry
    where querry = do
            outputStr "\n\nEnter your name:  "
            Just name <- getInputLine ""
            return name
            --outputStrLn ("Hello, " ++ name ++ ".")


                                              
