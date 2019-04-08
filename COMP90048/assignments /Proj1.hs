--  File     : Proj1.hs
--  ID       : 955986
--  Author   : Peiyong Wang
--  LMS ID   : peiyongw 
--  Purpose  : This is a program that plays the game of "Musician", which implements the performer and the composer part of the game

module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where

import qualified Data.Set as Set
import Data.List

-- The data type representing Note of the Pitches
data Note = A | B | C | D | E | F | G deriving (Eq, Ord, Show, Enum)

-- The data type representing the Pitch
data Pitch = Pitch {note::Note, octave::Int} deriving Eq

-- Chord, which is made up by three pitches
-- Every pitch will not appear more than once and also the order of pitches in the
-- target Chord is not relevant
type Chord = Set.Set Pitch

-- List of potential Chords
type GameState = [Chord]

-- Instance for showing the pitches by making Pitch member of Show
instance Show Pitch where show (Pitch note octave) = show note ++ show octave

-- Instance to make the Pitch type comparable by making Pitch member of Ord
instance Ord Pitch where
    compare (Pitch n1 o1) (Pitch n2 o2) =
        let noteOrder = compare n1 n2
        in if noteOrder == EQ then compare o1 o2 else noteOrder

-- Convert a list of Pitch to a list of pitch String
pitchListToStringList :: [Pitch] -> [String]
pitchListToStringList pL = map(\p -> show p) pL

-- Convert Char to Note
charToNote :: Char -> Note
charToNote char
    | char == 'A' = A
    | char == 'B' = B
    | char == 'C' = C
    | char == 'D' = D
    | char == 'E' = E
    | char == 'F' = F
    | char == 'G' = G
    | otherwise = error "Invalid Note"

-- Conver Note to Char
noteToChar :: Note -> Char
noteToChar note
    | note == A = 'A'
    | note == B = 'B'
    | note == C = 'C'
    | note == D = 'D'
    | note == E = 'E'
    | note == F = 'F'
    | note == G = 'G'
    | otherwise = error "Invalid Note"

-- Convert Char to Octave (Int)
charToOctave :: Char -> Int
charToOctave char
    | char == '1' = 1
    | char == '2' = 2
    | char == '3' = 3
    | otherwise = error "Invalid Octave"

-- Convert Octave (Int) to Char
ocatveToChar :: Int -> Char
ocatveToChar o 
    | o == 1 = '1'
    | o == 2 = '2'
    | o == 3 = '3'
    | otherwise = error "Invalid Octave"

-- List of all valid Pitches
allPitches :: [Pitch]
allPitches = [Pitch note octave | note <- [A, B, C, D, E, F, G], octave <- [1, 2, 3]]

-- List of all valid Chords, linear time to create Set from List if the List is ordered,
-- also the ordering makes sure that there is no duplicate Pitch in a Chord
allChords :: [Chord]
allChords = [Set.fromList [p1, p2, p3] | p1 <- allPitches, p2 <- allPitches, p3 <- allPitches, p1 < p2 && p2 < p3]

-- Convert String of Pitches to Pitch data type
toPitch :: String -> Maybe Pitch
toPitch pString
    | checkValid = Just (Pitch note octave)
    | otherwise = Nothing
    where 
        -- Check whether the input is valid pitch
        checkValid = ((head pString) `elem` ['A'..'G']) && ((head (tail pString)) `elem` ['1', '2', '3'])
        note = charToNote (head pString)
        octave = charToOctave (head (tail pString))

-- Pick the middle one from all possible chords as the initial guess
initialGuess :: ([Pitch],GameState)
initialGuess = ([pitch1, pitch2, pitch3], allChords)
    where
        numPossibleChords = length allChords
        initPick = allChords !! (numPossibleChords `quot` 2) -- The middle one of all possible chords
        initGuess = Set.toList initPick
        pitch1 = initGuess !! 0
        pitch2 = initGuess !! 1
        pitch3 = initGuess !! 2

-- Takes a target and a guess and returns how many pitches in the guess are included in the target (correct pitches)
correctPitches :: [Pitch] -> [Pitch] -> Int
correctPitches chord1 chord2 = (length (chord1 ++ chord2)) - (Set.size (Set.fromList (chord1 ++ chord2)))

-- Find whether two chords have same pitches
whetherSamePitches :: Chord -> Chord -> Bool
whetherSamePitches target guess
    | correctPitches (Set.toList target) (Set.toList guess) /= 0 = True
    | otherwise = False

-- Takes a target and a guess and returns how many pitches have the right note but the wrong octave (correct notes)
correctNotes :: [Pitch] -> [Pitch] -> Int
correctNotes target guess = sndo
    where
        sp = correctPitches target guess
        target_stringList = pitchListToStringList target
        guess_stringList = pitchListToStringList guess
        guess_note    = map (\x -> [x !! 0]) guess_stringList
        target_note   = map (\x -> [x !! 0]) target_stringList
        sndo = (length guess) - length (target_note \\ guess_note) - sp

-- Find whether two chords have the same note but different octaves
whetherSameNotes :: Chord -> Chord -> Bool
whetherSameNotes target guess 
    | correctNotes (Set.toList target) (Set.toList guess) /= 0 = True
    | otherwise = False

-- Takes a target and a guess and returns how many pitches have the right octave but the wrong note (correct octaves)
correctOctaves :: [Pitch] -> [Pitch] -> Int
correctOctaves target guess = sodn
    where 
        sp = correctPitches target guess
        target_stringList = pitchListToStringList target
        guess_stringList = pitchListToStringList guess
        guess_octave  = map (\x -> [x !! 1]) guess_stringList
        target_octave = map (\x -> [x !! 1]) target_stringList
        sodn = (length guess) - length (target_octave \\ guess_octave) - sp

-- Find whether two chords have same octave but different note
whetherSameOctave :: Chord -> Chord -> Bool
whetherSameOctave target guess 
    | correctOctaves (Set.toList target) (Set.toList guess) /= 0 = True
    | otherwise = False

-- Takes a target and a guess, respectively, and returns the appropriate feedback
-- sp: same pitch; sndo: same note different octave; sodn: same ocatve different note
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (sp, sndo, sodn)
    where 
        sp = correctPitches target guess
        sndo = correctNotes target guess
        sodn = correctOctaves target guess
        
-- Calculate similiarity score based on feed back
simScore :: Chord -> Chord -> Int
simScore target guess = sp - sndo - sodn
    where (sp, sndo, sodn) = feedback (Set.toList target) (Set.toList guess)
        

-- Takes as input a pair of the previous guess and game state, and the feedback to this
-- guess as a triple of correct pitches, notes, and octaves, and returns a pair of the next
-- guess and game state.
-- sp: same pitch; sndo: same note different octave; sodn: same ocatve different note
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess ([prev_p1, prev_p2, prev_p3], prev_state) (sp, sndo, sodn) = ([next_p1, next_p2, next_p3], next_state)
    where
        -- prev_score = sp - sndo - sodn
        prev_guess = Set.fromList [prev_p1, prev_p2, prev_p3]
        next_state
            -- | sp == 0 = [chord | chord <- prev_state, Set.null (Set.intersection prev_guess chord)]
            | (sp == 0 && sndo == 0 && sodn == 0) = filter (\x -> whetherSameNotes x prev_guess == False) (filter (\x -> whetherSameOctave x prev_guess == False) (filter (\x -> whetherSamePitches x prev_guess== False) prev_state))
            | (sp == 0 && sndo /= 0 && sodn /= 0) = filter (\x -> whetherSameNotes x prev_guess == True) (filter (\x -> whetherSameOctave x prev_guess == True) (filter (\x -> whetherSamePitches x prev_guess== False) prev_state))
            | (sp == 0 && sndo /= 0 && sodn == 0) = filter (\x -> whetherSameNotes x prev_guess == True) (filter (\x -> whetherSameOctave x prev_guess == False) (filter (\x -> whetherSamePitches x prev_guess== False) prev_state))
            | (sp == 0 && sndo == 0 && sodn /= 0) = filter (\x -> whetherSameNotes x prev_guess == False) (filter (\x -> whetherSameOctave x prev_guess == True) (filter (\x -> whetherSamePitches x prev_guess== False) prev_state))
            | (sp /= 0 && sndo == 0 && sodn == 0) = filter (\x -> whetherSameNotes x prev_guess == False) (filter (\x -> whetherSameOctave x prev_guess == False) (filter (\x -> whetherSamePitches x prev_guess== True) prev_state))
            | (sp /= 0 && sndo /= 0 && sodn /= 0) = filter (\x -> whetherSameNotes x prev_guess == True) (filter (\x -> whetherSameOctave x prev_guess == True) (filter (\x -> whetherSamePitches x prev_guess== True) prev_state))
            | (sp /= 0 && sndo == 0 && sodn /= 0) = filter (\x -> whetherSameNotes x prev_guess == False) (filter (\x -> whetherSameOctave x prev_guess == True) (filter (\x -> whetherSamePitches x prev_guess== True) prev_state))
            | (sp /= 0 && sndo /= 0 && sodn == 0) = filter (\x -> whetherSameNotes x prev_guess == True) (filter (\x -> whetherSameOctave x prev_guess == False) (filter (\x -> whetherSamePitches x prev_guess== True) prev_state))
            -- When sp is not zero, discard possible chords without common pitches as the previous guess
            -- | otherwise = [chord | chord <- prev_state, (Set.null (Set.intersection prev_guess chord)) == False && chord /= prev_guess]
        -- next_state = [chord | chord <- new_state, ] -- keep the chords similiarity score 
        state_space = length next_state
        [next_p1, next_p2, next_p3] =Set.toList (next_state !! (state_space `quot` 2))
