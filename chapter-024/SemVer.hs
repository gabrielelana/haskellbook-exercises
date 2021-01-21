module SemVer where

import Test.Hspec
import Text.Trifecta
import Text.Read
import Control.Applicative

data Version = Version { major :: Integer
                       , minor :: Integer
                       , patch :: Integer
                       , release :: [String]
                       , build :: [String]
                       } deriving (Eq, Show)

instance Ord Version where
  compare (Version m1 n1 p1 r1 _) (Version m2 n2 p2 r2 _) =
    case compare (m1, n1, p1) (m2, n2, p2) of
      EQ -> case (r1, r2) of
              ([], []) -> EQ
              ([], _ ) -> GT
              (_ , []) -> LT
              (_ , _ ) -> compareReleases r1 r2
      r -> r
    where compareReleases [] [] = EQ
          compareReleases [] _  = LT
          compareReleases _  [] = GT
          compareReleases (h1:t1) (h2:t2) =
            case (readMaybe h1 :: Maybe Int, readMaybe h2 :: Maybe Int) of
              (Just n1, Just n2) -> case compare n1 n2 of
                                      EQ -> compareReleases t1 t2
                                      r -> r
              (Nothing, Nothing) -> case compare h1 h2 of
                                      EQ -> compareReleases t1 t2
                                      r -> r
              (Just _ , Nothing) -> LT
              (Nothing, Just _ ) -> GT

alphanumericIdentifierP :: Parser String
alphanumericIdentifierP = some (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-")

numericIdentifierP :: Parser String
numericIdentifierP =  string "0" <|> some (oneOf "0123456789")

majorP :: Parser Integer
majorP = read <$> numericIdentifierP <* char '.'

minorP :: Parser Integer
minorP = read <$> numericIdentifierP <* char '.'

patchP :: Parser Integer
patchP = read <$> numericIdentifierP

releaseP :: Parser [String]
releaseP = option [] (char '-' *> identifierP `sepBy` char '.')
  where identifierP = try numericIdentifierP <|> alphanumericIdentifierP

buildP :: Parser [String]
buildP = option [] (char '+' *> identifierP `sepBy` char '.')
  where identifierP = alphanumericIdentifierP

semverP :: Parser Version
semverP = Version
  <$> majorP
  <*> minorP
  <*> patchP
  <*> releaseP
  <*> buildP
  <* eof

parse :: String -> Maybe Version
parse = maybeSuccess . parseString semverP mempty
  where
    maybeSuccess (Success a) = Just a
    maybeSuccess _           = Nothing

check :: IO ()
check = hspec $ do
  describe "parse" $ do
    it "with only version numbers" $ do
      parse "1.1.1" `shouldBe` Just (Version 1 1 1 [] [])
      parse "1.1.0" `shouldBe` Just (Version 1 1 0 [] [])
      parse "0.0.1" `shouldBe` Just (Version 0 0 1 [] [])
      parse "v0.0.1" `shouldBe` Nothing
    it "with pre-release" $ do
      parse "1.1.1-alpha" `shouldBe` Just (Version 1 1 1 ["alpha"] [])
      parse "1.1.1-alpha.beta" `shouldBe` Just (Version 1 1 1 ["alpha", "beta"] [])
      parse "1.1.1-alpha.0" `shouldBe` Just (Version 1 1 1 ["alpha", "0"] [])
      parse "1.1.1-alpha.01" `shouldBe` Nothing
    it "with build" $ do
      parse "1.1.1+B1" `shouldBe` Just (Version 1 1 1 [] ["B1"])
      parse "1.2.3+test.01" `shouldBe` Just (Version 1 2 3 [] ["test", "01"])
    it "with pre-release and build" $
      parse "1.2.0-x.Y.0+metadata-with-hypen" `shouldBe` Just (Version 1 2 0 ["x", "Y", "0"] ["metadata-with-hypen"])
  describe "order" $ do
    it "with only version numbers" $ do
      compare (parse "1.0.0") (parse "0.9.9") `shouldBe` GT
      compare (parse "2.0.0") (parse "1.99.99") `shouldBe` GT
      compare (parse "2.0.0") (parse "1.2.3") `shouldBe` GT
      compare (parse "0.10.0") (parse "0.9.0") `shouldBe` GT
      compare (parse "0.99.0") (parse "0.10.0") `shouldBe` GT
      compare (parse "0.0.1") (parse "0.0.0") `shouldBe` GT
    it "with pre-release" $ do
      compare (parse "1.0.0") (parse "1.0.0-alpha") `shouldBe` GT
      compare (parse "0.0.0") (parse "0.0.0-foo") `shouldBe` GT
      compare (parse "0.0.0-foo") (parse "0.0.0-5") `shouldBe` GT
      compare (parse "0.0.0-a.10") (parse "0.0.0-a.5") `shouldBe` GT
    it "build doesn't count" $ do
      compare (parse "1.0.0") (parse "1.0.0+B1") `shouldBe` EQ
    it "acceptance" $ do
      parse "1.0.0-alpha" < parse "1.0.0-alpha.1" `shouldBe` True
      parse "1.0.0-alpha.1" < parse "1.0.0-alpha.beta" `shouldBe` True
      parse "1.0.0-alpha.beta" < parse "1.0.0-beta" `shouldBe` True
      parse "1.0.0-beta" < parse "1.0.0-beta.2" `shouldBe` True
      parse "1.0.0-beta.2" < parse "1.0.0-beta.11" `shouldBe` True
      parse "1.0.0-beta.11" < parse "1.0.0-rc.1" `shouldBe` True
      parse "1.0.0-rc.1" < parse "1.0.0" `shouldBe` True
