module Transient where

type Point = (Double, Double)
type Transient = [Point]

tIsAfter :: Double -> Point -> Bool
tIsAfter t (pTime, _) = (t >= pTime)

tIsBefore :: Double -> Point -> Bool
tIsBefore t (pTime, _) = (t < pTime)

valueAtT :: Double -> Transient -> (Transient, Double)
valueAtT t [] = error "Programming error. valueAtT Transient value should contain at least one tuple at time 0"
valueAtT t (pt: []) = ((pt:[]), snd pt)
valueAtT t (pt1:pt2:rest)
    | (tIsAfter t pt1 && tIsBefore t pt2) = ((pt1:pt2:rest), snd pt1)
    | (tIsAfter t pt2)                    = valueAtT t (pt2:rest)
    | otherwise                           = error "Transient parameter not in chronological order"

