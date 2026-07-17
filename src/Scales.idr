module Scales

import Data.Vect

public export 
data Note : Type where 
    C       : Note 
    CSharp  : Note 
    DFlat   : Note
    D       : Note 
    DSharp  : Note 
    EFlat   : Note
    E       : Note 
    F       : Note 
    FSharp  : Note
    GFlat   : Note 
    G       : Note 
    GSharp  : Note
    AFlat   : Note 
    A       : Note 
    ASharp  : Note
    BFlat   : Note 
    B       : Note

public export
notes : Vect 12 Note 
notes = [C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B]

public export
data Step : Type where 
    Tone     : Step 
    Semitone : Step

public export 
majorToneShape : Vect 7 Step 
majorToneShape = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

||| Steps one pitch class up the chromatic circle, ignoring register.
||| Wraps silently at B -> C; callers that care about register crossing
||| that boundary must track it themselves (see `Pitch`'s `upSemitone`).
classUpSemitone : Note -> Note
classUpSemitone C      = CSharp
classUpSemitone CSharp = D
classUpSemitone DFlat  = D
classUpSemitone D      = DSharp
classUpSemitone DSharp = E
classUpSemitone EFlat  = E
classUpSemitone E      = F
classUpSemitone F      = FSharp
classUpSemitone FSharp = G
classUpSemitone GFlat  = G
classUpSemitone G      = GSharp
classUpSemitone GSharp = A
classUpSemitone AFlat  = A
classUpSemitone A      = ASharp
classUpSemitone ASharp = B
classUpSemitone BFlat  = B
classUpSemitone B      = C

||| Steps one pitch class down the chromatic circle, ignoring register.
||| Wraps silently at C -> B; see `classUpSemitone`.
classDownSemitone : Note -> Note
classDownSemitone C        = B
classDownSemitone B        = BFlat
classDownSemitone BFlat    = A
classDownSemitone ASharp   = A
classDownSemitone A        = AFlat
classDownSemitone AFlat    = G
classDownSemitone GSharp   = G
classDownSemitone G        = GFlat
classDownSemitone GFlat    = F
classDownSemitone FSharp   = F
classDownSemitone F        = E
classDownSemitone E        = EFlat
classDownSemitone EFlat    = D
classDownSemitone DSharp   = D
classDownSemitone D        = DFlat
classDownSemitone DFlat    = C
classDownSemitone CSharp   = C

||| A pitch class at a specific register, using the MIDI convention
||| that middle C (C4) has `register = 4`.
public export
data Pitch : Type where
  MkPitch : (pitchClass : Note) -> (register : Int) -> Pitch

||| Steps one semitone up. Bumps the register on the one transition
||| where the pitch class wraps (B -> C); every other step leaves the
||| register untouched.
public export
upSemitone : Pitch -> Pitch
upSemitone (MkPitch B reg) = MkPitch C (reg + 1)
upSemitone (MkPitch n reg) = MkPitch (classUpSemitone n) reg

public export
upTone : Pitch -> Pitch
upTone p = upSemitone (upSemitone p)

||| Steps one semitone down. Bumps the register on the one transition
||| where the pitch class wraps (C -> B); every other step leaves the
||| register untouched.
public export
downSemitone : Pitch -> Pitch
downSemitone (MkPitch C reg) = MkPitch B (reg - 1)
downSemitone (MkPitch n reg) = MkPitch (classDownSemitone n) reg

public export
downTone : Pitch -> Pitch
downTone p = downSemitone (downSemitone p)


public export
data Degree : Type where 
    Tonic       : Degree 
    Supertonic  : Degree 
    Mediant     : Degree 
    Subdominant : Degree
    Dominant    : Degree 
    Submediant  : Degree
    Leading     : Degree 

public export
Scale : Type
Scale = Vect 8 Pitch

public export
createMajor : (tonic : Pitch) -> (shape : Vect n Step) -> Vect (n+1) Pitch
createMajor tonic [] = [tonic]
createMajor tonic (Tone :: steps) = tonic :: createMajor (upTone tonic) steps
createMajor tonic (Semitone :: steps) = tonic :: createMajor (upSemitone tonic) steps

public export
majorScale : (tonic : Pitch) -> Scale
majorScale tonic = createMajor tonic majorToneShape

public export
tonic : Scale -> Pitch
tonic scale = index 0 scale

public export
supertonic : Scale -> Pitch
supertonic scale = index 1 scale

public export
mediant : Scale -> Pitch
mediant scale = index 2 scale

public export
subdominant : Scale -> Pitch
subdominant scale = index 3 scale

public export
dominant : Scale -> Pitch
dominant scale = index 4 scale

public export
submediant : Scale -> Pitch
submediant scale = index 5 scale

public export
leadingtone : Scale -> Pitch
leadingtone scale = index 6 scale

public export
octave : Scale -> Pitch
octave scale = index 7 scale