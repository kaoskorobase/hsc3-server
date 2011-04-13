import Test.Framework (Test, defaultMain)
import qualified Sound.SC3.Server.Allocator.Range.Test
import qualified Sound.SC3.Server.Allocator.Test

tests :: [Test]
tests =
    Sound.SC3.Server.Allocator.Range.Test.tests
 ++ Sound.SC3.Server.Allocator.Test.tests

main :: IO ()
main = defaultMain tests
