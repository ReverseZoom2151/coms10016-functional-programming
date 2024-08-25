module EmbeddingPartII where

-- Clash
-- -----------------------------------------------------------------------------

import Clash.Prelude

-- In hardware, we dont just go from one input, to one output, we have a continuous
-- stream of inputs --- like a wire, with an input arriving every clock cycle.

-- This comes as a predefined type in Clash.

-- They are called "Signals".

-- For example, I could create an infinite sequence of 8s as follows:
sig8 :: Signal System Int
sig8 = pure 8

-- >>> sig8
-- 8 8 8 8 8 8 8 ...

-- Running the above is a bit overwhelming, so instead, we can use a function
-- called sampleN

-- >>> :t sampleN
-- sampleN :: (KnownDomain dom, NFDataX a) => Int -> (HiddenClockResetEnable dom => Signal dom a) -> [a]

-- That type is a bit much, and that is because Clash needs to know about things
-- like clocks cos its all to do with hardware, but if we simplify the type,
-- omitting all the technical deets, it is a lot clearer what this does

-- sampleN :: (...) => Int -> (... => Signal dom a) -> [a]

-- It takes a number and a signal, and returns a list.
-- Running it tells us even more about this

-- >>> sampleN 6 sig8
-- [8,8,8,8,8,8]

-- Ah it extracts n values from the signal, collecting them into a list

-- Let's write a sequential circuit in Clash!

-- The circuit we will make is called multiply and accumulate, or mac.
-- Its input will be a stream of pairs of numbers and it will multply the numbers
-- together then, add that to all previous results.

-- e.g.
-- >>> mac [(1,1), (2,2), ...
-- 0 1 5 ...
-- This breaks down to
--   0 = initially we have accumulated no values. (1,1) arrives on the first tick
--       of the clock, and we need a tick to process it, so we just output 0 for now
--   1 = then we multiply the two numbers in our tuple together 1*1 = 1 and accumulate it
--   5 = (2*2)+1 = 5

-- The tool we use to do this is called a mealy machine:

-- mealy
--   :: (...)             -- ^ it has some clash / hardware constraints that we will ignore
--   => (s -> i -> (s,o)) -- ^ the it asks for a function to tell it how to behave*
--   -> s                 -- ^ we also need an initial state so we have something to output on the first tick
--   -> (Signal dom i -> Signal dom o) -- ^ given all these things we can creates circuit that operates on signals, turning inputs to outputs
-- mealy f initS = ...

-- * This function takes the current state, or our example, this will be the accumulated value
--   and the next input, and outputs the new state and an output.
--   Basically this function encapsulates what happens at each tick, and each tick
--   needs to output and update the state

mac inp = mealy transition initial inp
  where
    
    -- transition :: state -> input -> (newState, output)
    transition :: Int -> (Int,Int) -> (Int, Int)
    transition s (x,y) = let out = (x*y)+s in (out,out)

    -- initial :: state
    initial :: Int
    initial = 0

-- To test this, I will use a function called simulateN

-- Its simplified type is:
-- simulateN :: (...) => Int -> (... => Signal dom a -> Signal dom b) -> [a] -> [b]

-- Basically, it runs the circuit provided on inputs given in a list

-- >>> simulateN @System 2 mac [(1,1), (2,2) ]
-- [1,5]

-- Don't worry too much about the @System, that is just me telling Haskell some
-- hardware details.

-- For more see the clash tutorial: https://hackage.haskell.org/package/clash-prelude-1.4.6/docs/Clash-Tutorial.html