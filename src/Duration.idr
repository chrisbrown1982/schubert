module Duration

||| A conventional (British) note-duration name, independent of tempo or
||| of any particular tick resolution.
public export
data Duration : Type where
  Semibreve      : Duration
  Minim          : Duration
  Crotchet       : Duration
  Quaver         : Duration
  Semiquaver     : Duration
  Demisemiquaver : Duration
