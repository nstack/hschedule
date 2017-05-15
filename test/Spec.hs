import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (defaultMainWithIngredients)
import Test.Tasty.Runners.AntXML

import Data.Time.Schedule.Cron.Parser
import Data.Time.Schedule.Cron.Types

parseError :: String -> Cron
parseError = either error id . parseEither

assertParse :: Cron -> String -> Assertion
assertParse c = assertEqual "parse mismatch" c . parseError

main :: IO ()
main = defaultMainWithIngredients (antXMLRunner:defaultIngredients) $ testGroup "parser-test"
  [ testCase "cron-all-wilds" $ assertParse cronAllWilds "* * * * * *"
  , testCase "cron-optional-year" $ assertParse cronAllWilds "* * * * *"

  , testCase "cron-ranges" $ assertParse cronRanges "0-60 0-23 1-31 1-12 1-7 2017-2019"
  , testCase "cron-ranges-names" $ assertParse cronRanges "0-60 0-23 1-31 Jan-Dec Mon-Sun 2017-2019"
  , testCase "cron-ranges-mixed" $ assertParse cronRanges "0-60 0-23 1-31 1-Dec Mon-7 2017-2019"
  , testCase "cron-all-interval" $ assertParse cronAllIntervals "*/2 */2 */2 */2 */2 */2"
  , testCase "cron-ranges-interval" $ assertParse cronRangeIntervals "0-60/2 0-23/2 1-31/2 1-12/2 1-7/2 2017-2019/2"
  , testCase "cron-ranges-names-interval" $ assertParse cronRangeIntervals "0-60/2 0-23/2 1-31/2 Jan-Dec/2 Mon-Sun/2 2017-2019/2"
  , testCase "cron-lists" $ assertParse cronLists "0,2,10 0,10,12,22 1,15,20 1,Apr,6,9 Mon,3 2017,2018,2019"
  ]
  where cronAllWilds = let a = Exp All in Cron a a a a a a
        cronRanges = Cron (Exp $ Range 0 60) (Exp $ Range 0 23) (Exp $ Range 1 31) (Exp $ Range 1 12)
                          (Exp $ Range 1 7) (Exp $ Range 2017 2019)
        cronAllIntervals = let a = Exp (Interval All 2) in Cron a a a a a a
        cronRangeIntervals = Cron (Exp $ Interval (Range 0 60) 2)
                                  (Exp $ Interval (Range 0 23) 2)
                                  (Exp $ Interval (Range 1 31) 2)
                                  (Exp $ Interval (Range 1 12) 2)
                                  (Exp $ Interval (Range 1 7) 2)
                                  (Exp $ Interval (Range 2017 2019) 2)
        cronLists = Cron (List [0, 2, 10]) (List [0, 10, 12, 22]) (List [1,15,20]) (List [1,4,6,9])
                         (List [1, 3]) (List [2017,2018,2019])
