{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts,
             FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Time.Schedule.Cron.Parser (module Data.Time.Schedule.Cron.Parser) where
import Control.Applicative
import Data.Bifunctor
import Data.Char hiding (Space)
import Data.Foldable
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Loc as L
import Data.Void (Void)
import Language.Lexer.Applicative (Lexer, longest, token)
import qualified Language.Lexer.Applicative as L
import qualified Text.Megaparsec as P
import Text.Regex.Applicative (RE, sym, psym)

import Data.Time.Schedule.Cron.Types

data Sym = Star | Div | Comma | Hyphen
  deriving (Eq, Ord, Enum, Show)

data CronToken
 = Digit Integer
 | WeekDay DayOfWeek
 | Month Month
 | Symbol Sym
 | Space String
 deriving (Eq, Ord, Show)

printTokens :: [CronToken] -> String
printTokens = unwords . fmap showToken
  where showToken (Digit     i) = show i
        showToken (WeekDay dow) = show dow
        showToken (Month     m) = show m
        showToken (Space     s) = '\'' : s ++ "'"
        showToken (Symbol smbl) = case smbl of
                                    Star   -> "*"
                                    Comma  -> ","
                                    Div    -> "/"
                                    Hyphen -> "-"

cronLexer :: Lexer CronToken
cronLexer = mconcat
  [ token . longest $ asum [ Digit . read <$> many (psym isDigit)
                           , WeekDay <$> asum [ cistring "Mon" $> Mon
                                              , cistring "Tue" $> Tue
                                              , cistring "Wed" $> Wed
                                              , cistring "Thu" $> Thu
                                              , cistring "Fri" $> Fri
                                              , cistring "Sat" $> Sat
                                              , cistring "Sun" $> Sun
                                              ]
                           , Month <$> asum [ cistring "Jan" $> Jan
                                            , cistring "Feb" $> Feb
                                            , cistring "Mar" $> Mar
                                            , cistring "Apr" $> Apr
                                            , cistring "May" $> May
                                            , cistring "Jun" $> Jun
                                            , cistring "Jul" $> Jul
                                            , cistring "Aug" $> Aug
                                            , cistring "Sep" $> Sep
                                            , cistring "Oct" $> Oct
                                            , cistring "Nov" $> Nov
                                            , cistring "Dec" $> Dec
                                            ]
                           , Symbol <$> asum [sym '*' $> Star,
                                              sym ',' $> Comma,
                                              sym '/' $> Div,
                                              sym '-' $> Hyphen]
                           , Space <$> many (psym isSpace)
                           ]
  ]

newtype Toks t = Toks { unToks :: [L.L t] }
  deriving (Eq, Ord)

instance Ord t =>
         P.Stream (Toks t) where
  type Token (Toks t) = (L.L t)
  type Tokens (Toks t) = Toks t
  tokensToChunk _ = Toks
  chunkToTokens _ (Toks tokens) = tokens
  chunkLength _ (Toks tokens) = length tokens
  chunkEmpty _ (Toks []) = True
  chunkEmpty _ (Toks (_:_)) = False
  positionAt1 _ _ (L.L (L.Loc (L.Pos fn sl sc _) _) _) =
    P.SourcePos fn (P.mkPos sl) (P.mkPos sc)
  positionAt1 _ p (L.L L.NoLoc _) = p
  positionAtN _  a (Toks []) = a
  positionAtN pr p (Toks (a:_)) = P.positionAt1 pr p a
  advance1 _ _ _ (L.L (L.Loc _ (L.Pos fn el ec _)) _) =
    P.SourcePos fn (P.mkPos el) (P.mkPos $ ec + 1)
  advance1 _ _ p (L.L L.NoLoc _) = p
  advanceN _ _ c (Toks []) = c
  advanceN a b c (Toks ls) = P.advance1 a b c (last ls)
  take1_ (Toks []) = Nothing
  take1_ (Toks (t:r)) = Just (t, Toks r)
  takeN_ n (Toks []) | n <= 0 = Just (Toks [], Toks [])
                     | otherwise = Nothing
  takeN_ n (Toks ls) = let (t, r) = splitAt n ls in Just (Toks t, Toks r)
  takeWhile_ f (Toks ls) = let (t, r) = break f ls in (Toks t, Toks r)

instance P.ShowToken (L.L CronToken) where
  showTokens = printTokens . fmap L.unLoc . NE.toList

type MonadParser = P.MonadParsec Void (Toks CronToken)

satisfy' :: MonadParser m => (CronToken -> Maybe a) -> m a
satisfy' f = P.token go Nothing
  where go tok = case f (L.unLoc tok) of
                   Just  a -> Right a
                   Nothing -> Left (Just (P.Tokens (tok NE.:| [])), mempty)

satisfy :: MonadParser m => (CronToken -> Bool) -> m CronToken
satisfy f = satisfy' (\a -> if f a then Just a else Nothing)

digit :: MonadParser m => m Integer
digit = satisfy' (\case Digit i -> Just i; _ -> Nothing)

weekDay :: MonadParser m => m DayOfWeek
weekDay = satisfy' (\case WeekDay d -> Just d; _ -> Nothing)

month :: MonadParser m => m Month
month = satisfy' (\case Month m -> Just m; _ -> Nothing)

symbol :: MonadParser m => m Sym
symbol = satisfy' (\case Symbol s -> Just s; _ -> Nothing)

sym' :: MonadParser m => Sym -> m Sym
sym' s = satisfy' (\case Symbol s' -> if s == s' then Just s else Nothing; _ -> Nothing)

range :: MonadParser m => Range -> m Range
range r = (sym' Div *> digit <&> Interval r) <|> pure r

selection :: (MonadParser m) => m Integer -> (Integer -> Bool) -> m Selection
selection m g = pv >>= \i -> sym' Comma *> P.sepBy1 m (sym' Comma) <&> List . (i:)
                   <|> (sym' Hyphen *> m >>= fmap Exp . range . Range i)
                   <|> pure (List [i])
  where pv = m >>= \v -> if g v then pure v else fail "invalid value for field"

selectall :: MonadParser m => m Selection
selectall = sym' Star $> All >>= fmap Exp . range

cron :: MonadParser m => m Cron
cron = Cron <$> (selectall <|> selection digit (within 0 60)) <* spaces
            <*> (selectall <|> selection digit (within 0 23)) <* spaces
            <*> (selectall <|> selection digit (within 1 31)) <* spaces
            <*> (selectall <|> selection (fmap enum1 month <|> digit) withinMonths) <* spaces
            <*> (selectall <|> selection (fmap enum1 weekDay <|> digit) withinDays)
            <*> P.option (Exp All) (spaces *> (selectall <|> selection digit (within 1900 3000)))
  where enum1 :: Enum a => a -> Integer
        enum1 = fromIntegral . (+1) . fromEnum
        spaces = P.skipSome (satisfy $ \case Space _ -> True; _ -> False)
        within b e v = v >= b && v <= e
        withinMonths = within 1 12
        withinDays = within 1 7

parseEither :: String -> Either String Cron
parseEither s = let toks = first show . fmap Toks . L.streamToEitherList $ L.runLexer cronLexer "" s
                in toks >>= first show . P.runParser cron ""

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 4 <&>

cisym :: Char -> RE Char Char
cisym i = psym $ \j -> toUpper i == toUpper j

cistring :: String -> RE Char String
cistring = traverse cisym
