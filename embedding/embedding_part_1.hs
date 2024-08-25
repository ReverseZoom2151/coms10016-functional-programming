module EmbeddingPartI where

-- Our example:
-- BBate's instruction language.
--   BBate can perform the following moves:
--     * move forwards n meters
--     * pivot 90 degrees right
--     * pivot 90 degree left

-- Deep Embedding
-- -----------------------------------------------------------------------------

-- "The language constructs are embedded as data types in the host language."

-- What does this mean? Well, it mean we do the syntax first, and it is created
-- as a data type, then we assign semantics by writing a function that evaluates that data type.

-- Syntax first

-- | This data type represents the different moves BBate can make,
--   with each constructor representing a different move.
--   The type is also recursive, so once a move has been completed,
--   BBate can make another move of the same type.
--   More generally, this data type can be thought of as an "Abstract Syntax Tree",
--   and the moves as language constructs.
--   (makes sense that constructors encapsulate constructs!)
data BBate
  = BBFwd Int BBate -- ^ BBate moves forwards n:Int meters, then executes more BBate instrs.
  | BBRight BBate   -- ^ BBate pivots right 90 degrees, then executes more BBate instrs.
  | BBLeft BBate    -- ^ BBate pivots left 90 degrees, then executes more BBate instrs.
  | BBStop

-- | An example BBate program:
--   (see slides for animation of what this does)
--
--  @  . ==> .
--    /\     ||
--    ||     \/
--    8  <== .
--    ||     /\
--    \/     ||
--     . ==> .
-- @
bbAteFigureOfEight
  = BBFwd 8
  $ BBRight
  $ BBFwd 8
  $ BBRight
  $ BBFwd 8
  $ BBRight
  $ BBFwd 8
  $ BBLeft
  $ BBFwd 8
  $ BBLeft
  $ BBFwd 8
  $ BBLeft
  $ BBFwd 8
  $ BBStop

-- | A potential meaning that we could apply to a journey is how far BBate went:
--
-- >>> bbSemantics (BBFwd 10)
-- 10
--
-- >>> bbSemantics bbAteFigureOfEight
-- 56
bbSemantics :: BBate -> Int
bbSemantics BBStop = 0
bbSemantics (BBRight instrs) = bbSemantics instrs
bbSemantics (BBLeft  instrs) = bbSemantics instrs
bbSemantics (BBFwd n instrs) = n + bbSemantics instrs

-- Shallow Embedding
-- -----------------------------------------------------------------------------

-- "The language constructs are embedded as “first class” constructs in the host language."
-- semantics are directly linked to haskell functions

-- Semantics first

type BBate' = Int -- How far they went.

-- then the syntax is a haskell function per construct

bbFwd
  :: Int -- ^ How far forward to go.
  -> BBate' -- ^ The rest of the instrs, whose semantics mean how far the rest of the instrs take us
  -> BBate' -- ^ How far overall
bbFwd n instrs = n + instrs

bbRight :: BBate' -> BBate'
bbRight instrs = instrs

bbLeft :: BBate' -> BBate'
bbLeft instrs = instrs

bbStop :: BBate'
bbStop = 0

-- We can have a similar figure of eight example, just with the code using these functions
-- (so just the same, but with small letters)

bbAteFigureOfEight'
  = bbFwd 8
  $ bbRight
  $ bbFwd 8
  $ bbRight
  $ bbFwd 8
  $ bbRight
  $ bbFwd 8
  $ bbLeft
  $ bbFwd 8
  $ bbLeft
  $ bbFwd 8
  $ bbLeft
  $ bbFwd 8
  $ bbStop


-- Trade Offs
-- -----------------------------------------------------------------------------

-- It is easy to add new interps to a deep embedding
-- All you need is a new semantics function
-- e.g. say now we wanted to interpret bbate's instructions as how many right turns they make:

bbRightTurnSemantics :: BBate -> Int
bbRightTurnSemantics BBStop = 0
bbRightTurnSemantics (BBRight instrs) = 1 + bbSemantics instrs -- 1 only added when we turn right
bbRightTurnSemantics (BBLeft  instrs) = bbSemantics instrs
bbRightTurnSemantics (BBFwd _ instrs) = bbSemantics instrs     -- how far travelled is now irrelevant

-- It is hard to add new constructors to deep embeddings
-- A new constructor requires a new line per semantic function

data BBateDance
  = BBDanceFwd Int BBateDance
  | BBDanceRight BBateDance
  | BBDanceLeft BBateDance
  | BBDanceDance BBateDance -- new constructor!
  | BBDanceStop

-- our old semantic functions will now need cases covering that

-- When counting how far bbate is moving, we just recurse on the dance constructor
bbDanceSemantics (BBDanceDance instrs) = bbDanceSemantics instrs

-- Same with the right turn semantics
bbDanceRightTurnSemantics (BBDanceDance instrs) = bbDanceRightTurnSemantics instrs

-- It is hard to add new interps to a shallow embedding
-- Everything needs changed

type BBateDistRight = (Int, Int) -- (How far they went, how many right turns)

bbDistRightFwd n (d,rs) = (d+n,rs) -- now the semantics is a tuple, and we must keep track of both
bbDistRightRight (d,rs) = (d,rs+1)
bbDistRightLeft (d,rs) = (d,rs)
bbDistRightStop = (0,0)

-- It is east to add new construct to a shallow embedding
-- Just add a new function

bbDance instrs = instrs -- just recurse for original distance moved semantics
-- (no previous code needs changed)