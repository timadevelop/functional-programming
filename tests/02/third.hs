-- type ShowTime = (Int, Int)
--
-- data Show' = Show' {
--                     name :: String,
--                     starts :: (Int, Int),
--                     time :: Int
--                   } deriving (Show)

shows' = [("A", (11,0), 120), ("B", (12, 0), 15), ("C", (10,30),90)]

type Show' = (String, (Integer, Integer), Integer)
type ShowsList = [(String, (Integer, Integer), Integer)]

getName (name, (_, _), _) = name
getStartTime :: Show' -> (Integer, Integer)
getStartTime (_, (h, m), _) = (h, m)
getTime :: Show' -> Integer
getTime (_, (_, _), t) = t

getEndTime :: Show' -> (Integer, Integer)
getEndTime sh =
  let st = getStartTime sh
      t = (div (getTime sh) 60, rem (getTime sh) 60)
  in (fst st + fst t, snd st + snd t)

lastShow :: ShowsList -> String
lastShow shs = getName $ lastByTime
  where lastByTime = foldr (\c acc ->  if (getEndTime c) < (getEndTime acc) then acc else c) ("", (0, 0), 0) shs

longestProgram :: ShowsList -> ShowsList
longestProgram shs = [foldr (\c acc ->  if (getTime c) < (getTime acc) then acc else c) ("", (0, 0), 0) shs]
