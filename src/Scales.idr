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

public export 
upSemitone : Note -> Note
upSemitone C      = CSharp 
upSemitone CSharp = D 
upSemitone DFlat  = D
upSemitone D      = DSharp
upSemitone DSharp = E 
upSemitone EFlat  = E 
upSemitone E      = F 
upSemitone F      = FSharp 
upSemitone FSharp = G
upSemitone GFlat  = G 
upSemitone G      = GSharp 
upSemitone GSharp = A 
upSemitone AFlat  = A
upSemitone A      = ASharp
upSemitone ASharp = B 
upSemitone BFlat  = B
upSemitone B      = C 

public export
upTone : Note -> Note 
upTone n = upSemitone (upSemitone n)

public export
downSemitone : Note -> Note 
downSemitone C        = B
downSemitone B        = BFlat 
downSemitone BFlat    = A 
downSemitone ASharp   = A
downSemitone A        = AFlat 
downSemitone AFlat    = G 
downSemitone GSharp   = G
downSemitone G        = GFlat 
downSemitone GFlat    = F 
downSemitone FSharp   = F 
downSemitone F        = E
downSemitone E        = EFlat 
downSemitone EFlat    = D 
downSemitone DSharp   = D 
downSemitone D        = DFlat 
downSemitone DFlat    = C
downSemitone CSharp   = C

public export 
downTone : Note -> Note 
downTone n = downSemitone (downSemitone n) 


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
Scale = Vect 8 Note 

public export 
createMajor : (tonic : Note) -> (shape : Vect n Step) -> Vect (n+1) Note 
createMajor tonic [] = [tonic] 
createMajor tonic (Tone :: steps) = tonic :: createMajor (upTone tonic) steps  
createMajor tonic (Semitone :: steps) = tonic :: createMajor (upSemitone tonic) steps

public export
majorScale : (tonic : Note) -> Scale
majorScale tonic = createMajor tonic majorToneShape

public export
tonic : Scale -> Note 
tonic scale = index 0 scale 

public export
supertonic : Scale -> Note 
supertonic scale = index 1 scale 

public export
mediant : Scale -> Note 
mediant scale = index 2 scale 

public export
subdominant : Scale -> Note 
subdominant scale = index 3 scale 

public export
dominant : Scale -> Note 
dominant scale = index 4 scale

public export
submediant : Scale -> Note 
submediant scale = index 5 scale 

public export
leadingtone : Scale -> Note 
leadingtone scale = index 6 scale 

public export
octave : Scale -> Note 
octave scale = index 7 scale 