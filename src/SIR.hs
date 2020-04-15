module SIR where

import Data.List
import System.Exit
import qualified Transient as Tr

data SIR = SIR {
    susceptible :: Double
  , infected :: Double
  , removed :: Double
  , beta :: Double
  , gamma :: Double
  , sirN :: Double
  , sirTime :: Double
} deriving (Show)

data SIRTrans = SIRTrans {
    betaTransient :: Tr.Transient
  , gammaTransient :: Tr.Transient
} deriving (Show)

data SIRModel = SIRModel SIR SIRTrans Double Double deriving (Show)

make :: Double -> Double -> Double -> Double -> Double -> SIR
make s i r beta gamma = SIR s i r beta gamma (s + i + r) 0.0

dS' :: Double -> Double -> Double -> Double -> Double
dS' beta s i n = (-beta) * (s*i)/n

dI' :: Double -> Double -> Double -> Double -> Double -> Double
dI' beta gamma s i n = beta * (s*i)/n - gamma*i

dR' :: Double -> Double -> Double
dR' gamma i = gamma * i

rk4 :: Double -> (Double -> Double -> Double) -> Double -> Double -> Double
rk4 step f tn vn = vn + (1.0/6.0)*step*(k1 + 2*k2 + 2*k3 + k4)
    where 
        k1 = f tn vn
        k2 = f (tn + step/2.0) (vn + step*(k1/2.0))
        k3 = f (tn + step/2.0) (vn + step*(k2/2.0))
        k4 = f (tn + step) (vn + step*k3)

sirStep :: Double -> SIR -> SIR
sirStep step (SIR s i r beta gamma n t) = SIR s_next i_next r_next beta gamma n (t+step)
    where 
        s_next = rk4 step (\t s -> dS' beta s i n) t s
        i_next = rk4 step (\t i -> dI' beta gamma s i n) t i
        r_next = rk4 step (\t r -> dR' gamma i) t r

csvLine :: SIR -> String
csvLine (SIR s i r beta gamma n t) = intercalate "," (map show [t, s, i, r, beta, gamma, n])

run :: SIRModel -> IO ()
run model = do
    putStrLn "Time,S,I,R,Beta,Gamma,N"
    runLoop model
    
runLoop :: SIRModel -> IO ()
runLoop (SIRModel sir sirTrans tf step) = do
    if (sirTime sir < tf) then do
        let (sir', sirTrans') = updateTransient sir sirTrans
        putStrLn $ csvLine sir'
        let model' = SIRModel sir' sirTrans' tf step
        runLoop $ SIRModel (sirStep step sir') sirTrans' tf step
    else
        return ()

updateTransient :: SIR -> SIRTrans -> (SIR, SIRTrans)
updateTransient sir (SIRTrans tBeta tGamma) = (sirNext, sirTransNext)
    where
        t = sirTime sir
        (tBetaNext, vBeta) = Tr.valueAtT t tBeta
        (tGammaNext, vGamma) = Tr.valueAtT t tGamma
        sirTransNext = SIRTrans tBetaNext tGammaNext
        sirNext = sir { beta = vBeta, gamma = vGamma }