--module TutTwo (Font,Font_specifier, factorial) where

-- question 3
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

-- quetion2
data Font_colour = Colour_name String | Hex Int | RGB Int Int Int
data Font_specifier = Font_size Int | Font_face String | 


-- or
-- data Font_specifier_2 = Font_specifier_2 (Maybe)
-- question 6
