import HTMLHUnitTests
import MDHUnitTests
import QCTests
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "RUNNING MARKDOWN TESTS"
    test_md
    putStrLn "**********************"
    putStrLn "RUNNING HTML TESTS"
    test_html
    putStrLn "**********************"
    putStrLn "RUNNING QUICKCHECK TESTS"
    qc