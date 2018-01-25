module MusicBuilder where

import Euterpea
import Data.List
import Data.Ratio


bassDrum = perc AcousticBassDrum
snareDrum = perc AcousticSnare
hiHat = perc ClosedHiHat

-- drum phrase definition

{- |
Implementacja prostych perkusyjnych �cie�ek.
-}
drumPhrase note = b :=: h :+: h :+: s :=: h :+: h
        where b = bassDrum note
              s = snareDrum note
              h = hiHat note
              
{- |
Lista wszystkich d�wi�k�w
-}
notes :: [PitchClass]
notes = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]


{- |
Definicja sposobu tworzenia skali durowej.
-}
majorScalePattern = [True, False, True, False, True, True, False, True, False, True, False, True]

{- |
Definicja sposobu tworzernia akordu Dur.
-}
minorChordPattern = [True, False, False, True, False, False, False, True]


{- |
Definicja sposobu tworzenia skali skali Mol.
-}
minorScalePattern = [True, False, True, True, False, True, False, True, True, False, True, False]


{- |
Definicja sposobu akordu Dur.
-}
majorChordPattern = [True, False, False, False, True, False, False, True]

type PureNote = Octave -> Dur -> Music Pitch


{- |
Funkcja filtruj�ca d�wi�ki.
-}
dropNotes :: (Integral a) => Maybe a -> [PitchClass] -> [PitchClass]
dropNotes Nothing xs = xs
dropNotes (Just 0) xs = xs
dropNotes (Just n) [] = []
dropNotes (Just n) (x:xs) = dropNotes (Just (n-1)) xs

{- |
Funkcja filtruj�ca d�wi�ki.
-}
takeNotes :: (Integral a) => Maybe a -> [PitchClass] -> [PitchClass]    
takeNotes Nothing xs = xs
takeNotes (Just 0) _ = []
takeNotes (Just n) [] = []
takeNotes (Just n) (x:xs) = x : (takeNotes (Just (n-1)) xs)

{- |
Funkcja tworz�ca skal� durow� jako list� nazw d�wi�k�w
-}
majorScale :: PitchClass -> [PitchClass]
majorScale note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled majorScalePattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)
{- |
Funkcja tworz�ca akord Durowy jako list� nazw d�wi�k�w
-} 			  
majorChord :: PitchClass -> [PitchClass]
majorChord note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled majorChordPattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)
			  
{- |
Funkcja tworz�ca skal� molow� jako list� nazw d�wi�k�w
-}            
minorScale :: PitchClass -> [PitchClass]
minorScale note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled minorScalePattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)
{- |
Funkcja tworz�ca akord Molowy jako list� nazw d�wi�k�w
-} 			  
minorChord :: PitchClass -> [PitchClass]
minorChord note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled minorChordPattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)

{- |
Funkcja dodaj�ca do listy d�wi�k�w oktawy dla ka�dego d�wi�ku.
-} 			  			  
scalePitched :: Octave -> [PitchClass] -> [Pitch]
scalePitched _ [] = []
scalePitched octave (x:[]) = [(x,octave)]
scalePitched octave (x:xs) = [(x,octave)] ++ (if(head xs < x) then (scalePitched (octave+1) xs) else (scalePitched octave xs))


scalePitchedN :: Int -> [PitchClass] -> [Pitch]
scalePitchedN n notes
        | n <= 0 = []
        | otherwise = (scalePitchedN (n-1) notes) ++ (scalePitched n notes)


{- |
Funkcja dodaj�ca do listy d�wi�k�w d�ugos� dla ka�dego d�wi�ku.
-} 			  		
scaleNotes :: Dur -> [Pitch] -> [Music Pitch]
scaleNotes duration notes = [note duration n | n <- notes]


{- |
Funkcja scalaj�ca d�wi�ki do formatu doost�pnego dla odtwarzacza z biblioteki Euterpea.
-} 			  		
bindNotes :: [Music Pitch] -> Music Pitch
bindNotes (x:[]) = x
bindNotes (x:xs) = x :+: (bindNotes xs)

bindChord :: [Music Pitch] -> Music Pitch
bindChord (x:[]) = x
bindChord (x:xs) = x :=: (bindChord xs)


{- |
Typ danych definiuj�cy typy skal
-} 			  		
data ScaleType = Min | Maj


{- |
Funkcja tworz�ca skale podanego typu jako listy d�wiek�w.
-} 			  		
scale :: ScaleType -> PitchClass -> [PitchClass]
scale Min ton = minorScale(ton)
scale Maj ton = majorScale(ton)


{- |
Funkcja buduj�ca akord dla podanych atrybut�w.
-}
buildChord :: ScaleType -> PitchClass -> Dur -> Octave -> Music Pitch
buildChord Min ton duration pit = bindChord $ scaleNotes duration $ scalePitched pit (minorChord ton)
buildChord Maj ton duration pit = bindChord $ scaleNotes duration $ scalePitched pit (majorChord ton)