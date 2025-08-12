import System.Exit (ExitCode (..))
import System.FilePath
import System.Process
import Test.HUnit

testDir :: FilePath
testDir = "test"

buildDir :: FilePath
buildDir = testDir </> "build"

compilerTestDir :: FilePath
compilerTestDir = testDir </> "compiler"

as :: String
as = "gcc"

asFlags :: [String]
asFlags = ["-g"]

-- | Whole compiler tests
data CompilerTest = ExitCodeTest FilePath ExitCode

instance Testable CompilerTest where
  test (ExitCodeTest filePath expectedCode) =
    TestLabel (dropExtension filePath) $
      TestCase $ do
        let filePath' = compilerTestDir </> filePath
        let asmFilePath = replaceExtension (buildDir </> filePath) "s"
            outputPath = dropExtension asmFilePath
        callProcess "cabal" ["run", "aricc", "--", filePath', "-o", asmFilePath]
        callProcess as $ asFlags ++ [asmFilePath, "-o", outputPath]
        processHandle <- spawnProcess outputPath []
        exitCode <- waitForProcess processHandle
        assertEqual "" expectedCode exitCode

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [compilerTests]

compilerTests :: Test
compilerTests =
  test
    [ -- Week 1
      ExitCodeTest "return_0.c" ExitSuccess,
      ExitCodeTest "return_2.c" (ExitFailure 2),
      -- Week 2
      ExitCodeTest "bitwise.c" (ExitFailure 243),
      ExitCodeTest "bitwise_zero.c" (ExitFailure 255),
      ExitCodeTest "unary_negation.c" (ExitFailure 251),
      ExitCodeTest "logical_negation.c" ExitSuccess,
      ExitCodeTest "logical_negation_zero.c" (ExitFailure 1),
      ExitCodeTest "nested_unary_ops_1.c" ExitSuccess,
      ExitCodeTest "nested_unary_ops_2.c" (ExitFailure 1),
      -- Week 3
      ExitCodeTest "addition.c" (ExitFailure 141),
      ExitCodeTest "unary_op_add.c" ExitSuccess,
      ExitCodeTest "subtraction.c" (ExitFailure 247),
      ExitCodeTest "subtract_neg.c" (ExitFailure 3),
      ExitCodeTest "multiplication.c" (ExitFailure 6),
      ExitCodeTest "division.c" (ExitFailure 2),
      ExitCodeTest "parenthesis.c" (ExitFailure 14),
      ExitCodeTest "unary_op_parenthesis.c" (ExitFailure 253),
      ExitCodeTest "associativity_1.c" (ExitFailure 252),
      ExitCodeTest "associativity_2.c" (ExitFailure 1),
      ExitCodeTest "arithmetic_precedence.c" (ExitFailure 14),
      -- Week 4
      ExitCodeTest "and_false.c" ExitSuccess,
      ExitCodeTest "and_true.c" (ExitFailure 1),
      ExitCodeTest "or_false.c" ExitSuccess,
      ExitCodeTest "or_true.c" (ExitFailure 1),
      ExitCodeTest "eq_false.c" ExitSuccess,
      ExitCodeTest "eq_true.c" (ExitFailure 1),
      ExitCodeTest "ne_false.c" ExitSuccess,
      ExitCodeTest "ne_true.c" (ExitFailure 1),
      ExitCodeTest "gt_false.c" ExitSuccess,
      ExitCodeTest "gt_true.c" (ExitFailure 1),
      ExitCodeTest "ge_false.c" ExitSuccess,
      ExitCodeTest "ge_true.c" (ExitFailure 1),
      ExitCodeTest "lt_false.c" ExitSuccess,
      ExitCodeTest "lt_true.c" (ExitFailure 1),
      ExitCodeTest "le_false.c" ExitSuccess,
      ExitCodeTest "le_true.c" (ExitFailure 1),
      ExitCodeTest "and_or_precedence.c" (ExitFailure 1),
      ExitCodeTest "eq_or_precedence.c" (ExitFailure 1)
    ]
