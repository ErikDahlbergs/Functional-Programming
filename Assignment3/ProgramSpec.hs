{-# LANGUAGE QuasiQuotes #-}
module ProgramSpec where

import Test.Hspec
import Data.String.Here
import Data.Char (isSpace)

import Program


s1 = [here|
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

s2 = [here|
read n;
read b;
m := 1;
s := 0;
p := 1;
while n do
  begin
    q := n/b;
    r := n - q*b;
    write r;
    s := p*r+s;
    p := p*10;
    n :=q;
  end
write s;
|]

s3 = [here|
read a;
read b;
-- a simple comment
s := 3;
while a do
  begin
    c := a^s;
    d := 2^a;
    write c;
    write d;
    a := a-1;
  end
write a;
|]

s3b = [here|
read
     a;
read b;
s
:=
3;
while
a
do begin c := a^s;d := 2^a;write c;write d;a := a-1;end write a;
|]


s4 = [here|
read a;
read -- input a value
     -- into variable b:
     b;
s := 3;
while a -- i.e., as long as a is > 0
do
  begin
    c := a^s;
    d := 2^a;
    write c;
    write d;
    a := a-1;
  end
write a;
|]


ts :: Program.T -> String
ts = Program.toString

fs :: String -> Program.T
fs = Program.fromString

backAndForth :: String -> String
backAndForth =
  ts . fs

display :: String -> IO ()
display = do
  putStrLn

run =
  Program.exec . fs

runAfterBackAndForth =
  run . backAndForth


programSpec :: Spec
programSpec = do
  describe "Program" $ do
    it "s1 [2,8] should return [2,4,6]" $ do
      -- display s1
      -- display (backAndForth s1)
      run s1 [2,8] `shouldBe` [2,4,6]

    it "s1 [2,8] should return [2,4,6] after translation back and forth" $ do
      -- display s1
      -- display (backAndForth s1)
      runAfterBackAndForth s1 [2,8] `shouldBe` [2,4,6]

    it "s1 [3,16] should return [3,6,9,12,15]" $ do
      run s1 [3,16] `shouldBe` [3,6,9,12,15]

    it "s1 [3,16] should return [3,6,9,12,15] after translation back and forth" $ do
      runAfterBackAndForth s1 [3,16] `shouldBe` [3,6,9,12,15]

    it "s2 [2,8] should return [2,2]" $ do
      run s2 [2,8] `shouldBe` [2,2]

    it "s2 [2,8] should return [2,2] after translation back and forth" $ do
      runAfterBackAndForth s2 [2,8] `shouldBe` [2,2]

    it "s2 [1024,2] should return [0,0,0,0,0,0,0,0,0,0,1,10000000000]" $ do
      run s2 [1024,2] `shouldBe` [0,0,0,0,0,0,0,0,0,0,1,10000000000]

    it "s2 [1024,2] should return [0,0,0,0,0,0,0,0,0,0,1,10000000000] after translation back and forth" $ do
      runAfterBackAndForth s2 [1024,2] `shouldBe` [0,0,0,0,0,0,0,0,0,0,1,10000000000]

    it "s3 [4,4] should return [64,16,27,8,8,4,1,2,0]" $ do
      run s3 [4,4] `shouldBe` [64,16,27,8,8,4,1,2,0]

    it "s3 [4,4] should return [64,16,27,8,8,4,1,2,0] after translation back and forth" $ do
      runAfterBackAndForth s3 [4,4] `shouldBe` [64,16,27,8,8,4,1,2,0]

    it "s3b [4,4] should return [64,16,27,8,8,4,1,2,0]" $ do
      run s3b [4,4] `shouldBe` [64,16,27,8,8,4,1,2,0]

    it "s3b [4,4] should return [64,16,27,8,8,4,1,2,0] after translation back and forth" $ do
      runAfterBackAndForth s3b [4,4] `shouldBe` [64,16,27,8,8,4,1,2,0]

    it "s4 [4,4] should return [64,16,27,8,8,4,1,2,0]" $ do
      run s4 [4,4] `shouldBe` [64,16,27,8,8,4,1,2,0]

    it "s4 [4,4] should return [64,16,27,8,8,4,1,2,0] after translation back and forth" $ do
      runAfterBackAndForth s4 [4,4] `shouldBe` [64,16,27,8,8,4,1,2,0]
