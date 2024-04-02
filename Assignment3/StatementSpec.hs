{-# LANGUAGE QuasiQuotes #-}
module StatementSpec where

import Test.Hspec
import Data.String.Here

import Statement
import Dictionary


simpleIfTest = [here|
begin
  read p;
  read q;
  if p - q then
    write p;
  else
    write q;
end
|]

simpleWhileTest = [here|
begin
  read p;
  while p do
    begin
      write p;
      p := p - 1;
    end
end
|]

simpleFactorial = [here|
begin
  read n;
  fac := 1;
  while n do
    begin
      fac := fac*n;
      n := n-1;
    end
  write fac;
end
|]

factorialWithComments = [here|
begin
  read n;   -- just reading n...
  -- this line should just be ignored
  fac := -- initialize fac
         -- to 1:
         1;
  while n do
    begin
      fac := fac*n;
      n := n-1;
    end
  write -- woops
  fac;
end
|]

sample = [here|
  read k;
  read n;
  m := 1;
  while n-m do
    begin
      if m - m/k*k then
        skip;
      else
        write m;
      m := m + 1;
    end  
|]


dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.insert ("n", 4) $
       Dictionary.empty


check stmts input = exec (map fromString stmts) dict input


display :: Statement.T -> IO ()
display = do
  putStrLn . toString


statementSpec :: Spec
statementSpec = do
  describe "Statement" $ do
    it "skip should do nothing" $ do
      check ["skip;"] [] `shouldBe` []

    it "read and write should work" $ do
      check ["read count;", "write count + 1;"] [1] `shouldBe` [2]

    it "assignment should work" $ do
      check ["value := 42;", "write value;"] [] `shouldBe` [42]

    it "read, write, and assignment should work together" $ do
      check ["read count;", "count := count + 1;" , "write count;"] [1] `shouldBe` [2]

    it "a simple block should work" $ do
      check ["begin write 1; write 2; end"] [] `shouldBe` [1,2]

    it "a simple if statement should work" $ do
      check [simpleIfTest] [3,4] `shouldBe` [4]

    it "a simple while statement should work" $ do
      check [simpleWhileTest] [3] `shouldBe` [3,2,1]

    it "should be able to calculate factorials" $ do
      check [simpleFactorial] [4] `shouldBe` [24]

    it "should be able to calculate factorials with comments" $ do
      check [factorialWithComments] [4] `shouldBe` [24]
