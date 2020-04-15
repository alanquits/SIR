module IO where

import qualified SIR
import System.IO
import Text.ParserCombinators.Parsec
import qualified Transient as Tr

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

double :: Parser Double
double = lexeme $ do 
    fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Double
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer

fromFile :: String -> IO (Maybe SIR.SIRModel)
fromFile fname = do
    contents <- readFile fname
    case readSIR contents of
        Left msg -> putStrLn msg >> return Nothing
        Right model -> return $ Just model

integer :: Parser String
integer = plus <|> minus <|> number

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

minus :: Parser String
minus = char '-' <:> number

number :: Parser String
number = many1 digit

plus :: Parser String
plus = char '+' *> number

parseParam :: String -> Parser Double
parseParam name = lexeme $ do
    string name >> spaces
    p <- double
    return p

parseNumberPair :: Parser (Double, Double)
parseNumberPair = lexeme $ do
    n1 <- double
    n2 <- double
    return (n1, n2)

parsePopulation :: Parser (Double, Double, Double)
parsePopulation = lexeme $ do
    string "population" >> spaces
    string "initial" >> spaces
    s <- parseParam "s"
    i <- parseParam "i"
    r <- parseParam "r"
    return (s, i, r)

readSIR :: String -> Either String SIR.SIRModel
readSIR input = case parse parseSIR "SIR Model" input of
    Left err -> Left $ "Syntax error: " ++ show err
    Right model -> Right model

parseTransient :: String -> Parser Tr.Transient
parseTransient name = lexeme $ do
    string name >> spaces
    v0 <- parseParam "initial"
    rest <- (option [] $ many parseNumberPair)
    return $ (0, v0) : rest

parseSIR :: Parser SIR.SIRModel
parseSIR = do
    time <- parseParam "time"
    step <- parseParam "step"
    (s, i, r) <- parsePopulation
    tBeta <- parseTransient "beta"
    tGamma <- parseTransient "gamma"
    eof
    return $ SIR.SIRModel (SIR.make s i r 0.0 0.0) (SIR.SIRTrans tBeta tGamma) time step

