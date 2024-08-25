patternMatching True = this
patternMatching False = that
guards b
  | b         = this
  | otherwise = that