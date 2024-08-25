data StarBakerCount = StarBakerCount
{ dave    :: Int
, hermine :: Int
, laura   :: Int
, linda   :: Int
, loriea  :: Int
, lottie  :: Int
, makbul  :: Int
, marc    :: Int
, mark    :: Int
, peter   :: Int
, rowan   :: Int
, sura    :: Int
}

----------------------------------------------------

starPeter :: StarBakerCount -> StarBakerCount
starPeter count = count {peter = peter count + 1}
