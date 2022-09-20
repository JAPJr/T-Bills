module Main (main) where

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
{--
  billData <- getBillData weeksToMaturity
  print billData
  printData billData
--}
  printData someData


getBillData :: [MatureTime] -> IO( [BillData] )
getBillData matureTimes = do
  let noBillData = pure [] :: IO([BillData])
  foldr (\matTime billData -> (fmap (:) (getBillDatum matTime)) <*> billData) noBillData matureTimes



getBillDatum :: Int -> IO ( BillData )
getBillDatum matTime = do
  putStrLn ("For " ++ show matTime ++ "-week t-bill enter values for:")
  putStrLn "Interest rate:"
  intRate <- fmap read getLine :: IO (InterestRate)
  putStrLn "Change in interest rate per day:"
  dailyRateChange <- fmap read getLine :: IO (RateChangePerDay)
  return (matTime, intRate, dailyRateChange)



getRates :: [Int] -> IO([(Int,Double)])
getRates weeks = do
  let noRates = pure [] :: IO([(Int, Double)])
  foldr (\w rates -> (fmap (:) (getRate w)) <*> rates ) noRates weeks

getRate :: Int -> IO((Int, Double))
getRate period = do
  putStrLn ("Enter the interest rate for a " ++ show period ++ "-week t-bill") 
  r <- fmap read getLine :: IO(Double)
  return (period, r)
 
printRates :: [(Int, Double)] -> IO()
printRates theRates = do putStrLn "The current T-Bill rates are:\n"
                         printEachRate theRates
  where printEachRate [] = putStrLn ""
        printEachRate ((w,r):moreRates) = do putStr (show w ++ "-week " ++ show r ++ "   ")
                                             printEachRate moreRates

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
                                                                       
