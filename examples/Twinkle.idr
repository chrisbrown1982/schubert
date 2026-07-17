module Twinkle

import Scales
import Duration
import Melody

||| A phrase of six crotchets followed by a closing minim --
||| the rhythm of every line in Twinkle Twinkle Little Star.
phrase : List Degree -> Degree -> Melody
phrase crotchets closing = line Crotchet crotchets ++ [MkMelodyNote closing Minim]

||| Twinkle Twinkle Little Star, first verse, in scale degrees.
||| Form is AABBAA: the opening and "how I wonder" phrases bookend two
||| repeats of the "up above the world" phrase.
export
twinkle : Melody
twinkle = opening ++ wonder ++ upAbove ++ upAbove ++ opening ++ wonder
  where
    opening : Melody
    opening = phrase [Tonic, Tonic, Dominant, Dominant, Submediant, Submediant] Dominant

    wonder : Melody
    wonder = phrase [Subdominant, Subdominant, Mediant, Mediant, Supertonic, Supertonic] Tonic

    upAbove : Melody
    upAbove = phrase [Dominant, Dominant, Subdominant, Subdominant, Mediant, Mediant] Supertonic
