module Midi

import Data.Buffer
import Data.Vect
import System.File.Buffer

import Scales
import Duration

||| Chromatic position of a Note within an octave (C = 0 .. B = 11).
||| Enharmonic spellings (DFlat, EFlat, ...) map to the same index as
||| their sharp equivalent.
export
chromaticIndex : Note -> Int
chromaticIndex C      = 0
chromaticIndex CSharp = 1
chromaticIndex DFlat  = 1
chromaticIndex D      = 2
chromaticIndex DSharp = 3
chromaticIndex EFlat  = 3
chromaticIndex E      = 4
chromaticIndex F      = 5
chromaticIndex FSharp = 6
chromaticIndex GFlat  = 6
chromaticIndex G      = 7
chromaticIndex GSharp = 8
chromaticIndex AFlat  = 8
chromaticIndex A      = 9
chromaticIndex ASharp = 10
chromaticIndex BFlat  = 10
chromaticIndex B      = 11

||| MIDI note number, using the common convention that middle C (C4) = 60.
export
pitch : Pitch -> Int
pitch (MkPitch note reg) = (reg + 1) * 12 + chromaticIndex note

||| How many ticks a Duration lasts, given `division` ticks per crotchet.
export
ticks : (division : Int) -> Duration -> Int
ticks division Semibreve      = division * 4
ticks division Minim          = division * 2
ticks division Crotchet       = division
ticks division Quaver         = division `div` 2
ticks division Semiquaver     = division `div` 4
ticks division Demisemiquaver = division `div` 8

public export
data MidiEvent : Type where
  NoteOn  : (channel : Int) -> (pitch : Int) -> (velocity : Int) -> MidiEvent
  NoteOff : (channel : Int) -> (pitch : Int) -> (velocity : Int) -> MidiEvent

||| A MIDI event together with the number of ticks since the previous event.
public export
data TimedEvent : Type where
  MkTimedEvent : (deltaTime : Int) -> (event : MidiEvent) -> TimedEvent

------------------------------------------------------------------------
-- Raw byte encoding

stringBytes : String -> List Bits8
stringBytes s = map (cast . ord) (unpack s)

u16be : Int -> List Bits8
u16be n = [ cast (n `div` 256), cast (n `mod` 256) ]

u32be : Int -> List Bits8
u32be n =
  [ cast (n `div` 16777216)
  , cast ((n `div` 65536)   `mod` 256)
  , cast ((n `div` 256)     `mod` 256)
  , cast (n `mod` 256)
  ]

||| Variable-length quantity encoding used for MIDI delta-times.
vlq : Int -> List Bits8
vlq n = setContinuation (reverse (toGroups n))
  where
    toGroups : Int -> List Int
    toGroups m = if m < 128 then [m] else (m `mod` 128) :: toGroups (m `div` 128)

    setContinuation : List Int -> List Bits8
    setContinuation []        = [0]
    setContinuation [x]       = [cast x]
    setContinuation (x :: xs) = cast (x + 128) :: setContinuation xs

encodeEvent : MidiEvent -> List Bits8
encodeEvent (NoteOn  ch p v) = [cast (0x90 + (ch `mod` 16)), cast p, cast v]
encodeEvent (NoteOff ch p v) = [cast (0x80 + (ch `mod` 16)), cast p, cast v]

encodeTimedEvent : TimedEvent -> List Bits8
encodeTimedEvent (MkTimedEvent dt ev) = vlq dt ++ encodeEvent ev

endOfTrack : List Bits8
endOfTrack = [0x00, 0xFF, 0x2F, 0x00]

trackChunk : List TimedEvent -> List Bits8
trackChunk events =
  let body = concatMap encodeTimedEvent events ++ endOfTrack
  in stringBytes "MTrk" ++ u32be (cast (length body)) ++ body

||| Single-track (format 0) MIDI file, `division` ticks per quarter note.
midiFile : (division : Int) -> List TimedEvent -> List Bits8
midiFile division events =
  stringBytes "MThd" ++ u32be 6 ++ u16be 0 ++ u16be 1 ++ u16be division
    ++ trackChunk events

------------------------------------------------------------------------
-- Building event lists

||| Play a sequence of pitches one after another, each held for `duration`
||| ticks at the given `velocity`, on channel 0.
export
noteEvents : (duration : Int) -> (velocity : Int) -> List Pitch -> List TimedEvent
noteEvents duration velocity []        = []
noteEvents duration velocity (p :: ps) =
  MkTimedEvent 0        (NoteOn  0 (pitch p) velocity) ::
  MkTimedEvent duration (NoteOff 0 (pitch p) velocity) ::
  noteEvents duration velocity ps

||| Play a sequence of pitches, each held for its own Duration (at the
||| given `division` ticks per crotchet), at the given `velocity`, on
||| channel 0.
export
melodyEvents : (division : Int) -> (velocity : Int) -> List (Pitch, Duration) -> List TimedEvent
melodyEvents division velocity []                = []
melodyEvents division velocity ((p, dur) :: rest) =
  MkTimedEvent 0                    (NoteOn  0 (pitch p) velocity) ::
  MkTimedEvent (ticks division dur) (NoteOff 0 (pitch p) velocity) ::
  melodyEvents division velocity rest

------------------------------------------------------------------------
-- Writing to disk

export
writeBytes : (path : String) -> List Bits8 -> IO (Either String ())
writeBytes path bytes = do
  let len = cast (length bytes)
  Just buf <- newBuffer len
    | Nothing => pure (Left "could not allocate buffer")
  fillBuffer buf 0 bytes
  Right () <- writeBufferToFile path buf len
    | Left (err, _) => pure (Left (show err))
  pure (Right ())
  where
    fillBuffer : Buffer -> Int -> List Bits8 -> IO ()
    fillBuffer buf offset []        = pure ()
    fillBuffer buf offset (b :: bs) =
      setBits8 buf offset b >>= \_ => fillBuffer buf (offset + 1) bs

||| Write a sequence of pitches to a Standard MIDI File.
||| `division` is ticks per quarter note (480 is a common default),
||| `duration` is how many ticks each note is held for.
export
writeNotes : (path : String) -> (division : Int) ->
             (duration : Int) -> (velocity : Int) -> List Pitch -> IO (Either String ())
writeNotes path division duration velocity pitches =
  writeBytes path (midiFile division (noteEvents duration velocity pitches))

||| Directory that generated MIDI files are written into by default.
export
outputDir : String
outputDir = "output"

||| Write a sequence of pitches, each with its own Duration, to a
||| Standard MIDI File in `outputDir` -- e.g. the output of `Melody.resolve`.
export
writeMelody : (filename : String) -> (division : Int) -> (velocity : Int) ->
              List (Pitch, Duration) -> IO (Either String ())
writeMelody filename division velocity events =
  writeBytes (outputDir ++ "/" ++ filename) (midiFile division (melodyEvents division velocity events))

||| Write a major scale (ascending, one octave) to a Standard MIDI File in
||| `outputDir`. `octave` is the register of the tonic (e.g. 4 for
||| middle-C's octave).
export
writeScale : (filename : String) -> (tonic : Note) -> (octave : Int) -> IO (Either String ())
writeScale filename tonicNote octave =
  writeNotes (outputDir ++ "/" ++ filename) 480 480 96 (toList (majorScale (MkPitch tonicNote octave)))
