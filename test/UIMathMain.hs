module Main where
import Test.Hspec
import qualified Control.UIMathSpec

main :: IO ()
main = hspec Control.UIMathSpec.spec
