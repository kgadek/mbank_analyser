{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Combinator as ATC
--import Control.Monad
import Control.Applicative ((<*))
--import Data.DateTime as D


readValue :: AT.Parser T.Text
readValue = ATC.manyTill AT.anyChar (AT.try $ AT.char ';') >>= (return . T.pack)

readValues :: AT.Parser [T.Text]
readValues = ATC.manyTill readValue (AT.try AT.endOfLine)

skipLine :: AT.Parser T.Text
skipLine = (AT.takeTill AT.isEndOfLine) <* (AT.takeWhile AT.isEndOfLine)

-- Nagłówek pliku .cvs zawiera dodatkowe informacje w postaci:
--   #Klucz
--   wartość
parseHeaderData :: T.Text -> AT.Parser [T.Text]
parseHeaderData name = do
    AT.string name
    AT.takeWhile AT.isEndOfLine
    res <- readValues
    AT.takeWhile AT.isEndOfLine
    return res

---- #Data operacji; #Data księgowania; #Opis operacji;                #Tytuł;           #Nadawca/Odbiorca;                           #Numer konta;                 #Kwota; #Saldo po operacji;
---- 2010-09-13;     2010-09-13;        PRZELEW WEWNĘTRZNY WYCHODZĄCY; "TYTUŁ PRZELEWU"; "IMIĘ NAZWISKO  UL ICA 12/34 56-789 MIASTO"; '12345678901234567890123456'; -10,00; 90,00;
--newtype Entry = Entry { data_operacji :: D.DateTime,
--                        data_ksiegowania :: D.DateTime,
--                        opis :: T.Text,
--                        tutul :: T.Text,
--                        kto :: T.Text,
--                        nr_konta :: T.Text,
--                        kwota :: } deriving (Eq, Show)

oneCSVLine :: AT.Parser [T.Text]
oneCSVLine = do
    res <- readValues
    AT.takeWhile AT.isEndOfLine
    return res

mbankCSVParser :: AT.Parser T.Text
mbankCSVParser = do
        ATC.count 6 skipLine    -- nagłówek
        [_klient]                  <- parseHeaderData "#Klient;"
        skipLine                -- śmieć
        [_okres_od, _okres_do]     <- parseHeaderData "#Za okres:;"
        [_rodzaj_rachunku]         <- parseHeaderData "#Rodzaj rachunku;"
        [_waluta]                  <- parseHeaderData "#Waluta;"
        [_nr_rachunku]             <- parseHeaderData "#Numer rachunku;"
        [_data_nast_kapitalizacji] <- parseHeaderData "#Data następnej kapitalizacji;"
        [_oprocentowanie_rachunku] <- parseHeaderData "#Oprocentowanie rachunku;"
        [_limit_kredytu]           <- parseHeaderData "#Limit kredytu;"
        [_oprocentowanie_kredytu]  <- parseHeaderData "#Oprocentowanie kredytu;"
        [_, _uznania_liczba,    _uznania_wartosc]    <- parseHeaderData "#Podsumowanie obrotów na rachunku;#Liczba operacji;#Wartość operacji"
        [_, _obciazenia_liczba, _obciazenia_wartosc] <- parseHeaderData ""
        [_, _lacznie_liczba,    _lacznie_wartosc]    <- parseHeaderData ""
        ["#Saldo początkowe",   _saldo_poczatkowe]   <- parseHeaderData ""
        _headers                                     <- parseHeaderData ""
        _res <- oneCSVLine
        return _nr_rachunku

main :: IO ()
main = do
    file <- Data.Text.IO.readFile "ekonto.csv"
    let res = AT.parseOnly mbankCSVParser file
    --either putStrLn putStrLn res
    either putStrLn Data.Text.IO.putStrLn res