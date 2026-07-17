module Melody

import Scales
import Duration

||| A note in a melody, named by scale degree rather than by pitch, so the
||| same melody can be resolved against any Scale to transpose it -- exactly
||| how a musician reads a tune by degree ("1 2 3 . 5 . .") rather than by
||| fixed pitch. `duration` is a symbolic note value (Crotchet, Minim, ...);
||| it only becomes a tick count once written to a MIDI file at some
||| particular division (see Midi.writeMelody).
public export
data MelodyNote : Type where
  MkMelodyNote : (degree : Degree) -> (duration : Duration) -> MelodyNote

public export
Melody : Type
Melody = List MelodyNote

||| Build a melody where every note has the same duration.
export
line : (duration : Duration) -> List Degree -> Melody
line duration = map (\d => MkMelodyNote d duration)

||| The Pitch a Degree names within a given Scale, dispatching to the
||| degree accessors already defined in Scales.
export
degreePitch : Degree -> Scale -> Pitch
degreePitch Tonic       = tonic
degreePitch Supertonic  = supertonic
degreePitch Mediant     = mediant
degreePitch Subdominant = subdominant
degreePitch Dominant    = dominant
degreePitch Submediant  = submediant
degreePitch Leading     = leadingtone

||| Resolve a Melody against a concrete major scale, producing the
||| (Pitch, Duration) pairs ready to become MIDI events.
export
resolve : Scale -> Melody -> List (Pitch, Duration)
resolve scale = map (\(MkMelodyNote d dur) => (degreePitch d scale, dur))
