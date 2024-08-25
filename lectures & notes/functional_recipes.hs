secretSauce = undefined

f ... = let
          x = undefined
        in undefined

f ... = let
          -- things i need:
          ingredient1 = undefined
          ingredient2 = undefined
          tool = undefined
          -- idk yet:
        in undefined

iceCakes :: [Cake] -> [IcedCake]
iceCakes = undefined -- When in doubt, just put undefined!

iceCakes :: [Cake] -> [IcedCake]
iceCakes [] = -- idkkk
iceCakes (cupcake:cupcakes) = -- idkkk

iceCakes :: [Cake] -> [IcedCake]
iceCakes [] = undefined -- The classic idkkk placeholder
iceCakes (cupcake:cupcakes) = undefined -- The classic idkkk placeholder

iceCakes :: [Cake] -> [IcedCake]
iceCakes [] = [] -- BOOM! Half way there!
iceCakes (cupcake:cupcakes) = undefined

iceCakes :: [Cake] -> [IcedCake]
iceCakes [] = []
iceCakes (cupcake:cupcakes)
  = let
      -- I need:
      icing = undefined
      ice :: Cake -> IcedCake
      ice = undefined
  in undefined

iceCakes :: [Cake] -> [IcedCake]
iceCakes [] = []
iceCakes (cupcake:cupcakes)
  = let
      icing = ButterIcing -- The tasy kind.
      ice :: Cake -> IcedCake
      ice = undefined
  in undefined

iceCakes :: [Cake] -> [IcedCake]
iceCakes [] = []
iceCakes (cupcake:cupcakes)
  = let
      icing = ButterIcing
      ice :: Cake -> IcedCake
      ice cake = pipe icing cake
    in undefined -- Ingredients gathered - time for the showstopper!

iceCakes :: [Cake] -> [IcedCake]
iceCakes [] = []
iceCakes (cupcake:cupcakes)
  = let
      icing = ButterIcing
      ice :: Cake -> IcedCake
      ice cake = pipe icing cake
    in ice cupcake : iceCakes cupcakes


