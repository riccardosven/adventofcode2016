import           Data.Char (isDigit)
import           Data.Char
import           Data.List

type Name = String

type SectorID = Int

type Checksum = String

data Room =
  Room
    { name     :: Name
    , sectorId :: SectorID
    , checksum :: Checksum
    }
  deriving (Eq, Show)

-- Parse a room from a string specification
parseRoom :: String -> Room
parseRoom x = Room n ((read :: String -> Int) id) (init . tail $ c)
  where
    (first, last) = span (not . isDigit) x
    n = init first
    (id, c) = span (isDigit) last

-- Ordering used to compute the checksum
lengthOrder :: String -> String -> Ordering
lengthOrder x y
  | length x < length y = GT
  | length x == length y && head x > head y = GT
  | length x > length y = LT
  | length x == length y && head x < head y = LT
  | otherwise = EQ

-- Compute the correct checksum of the room name
computeChecksum :: Room -> String
computeChecksum (Room n _ c) =
  take (length c) .
  map head . sortBy lengthOrder . groupBy (==) . sort . filter (/= '-') $
  n

-- Check if a room specification is valid (the checksum is correct)
isValid :: Room -> Bool
isValid r@(Room n id c) = c == computeChecksum r

-- Solution to the first star
star1 :: String -> Int
star1 = sum . map sectorId . filter isValid . map parseRoom . lines

-- Caesar cipher
shiftCipher :: Int -> Name -> Name
shiftCipher n = map (f n)
  where
    f _ '-' = ' '
    f n x   = cycle ['a' .. 'z'] !! ((ord x) - (ord 'a') + (n `mod` 26))

-- Decrypt the name of the room using the cipher
decryptName :: Room -> Room
decryptName (Room n id c) = Room (shiftCipher id n) id c

--Solution to the second star
star2 :: String -> SectorID
star2 =
  sectorId .
  head .
  filter (isInfixOf "northpole" . name) .
  map decryptName . filter isValid . map parseRoom . lines

main = do
  let room1 = parseRoom "aaaaa-bbb-z-y-x-123[abxyz]"
  let room2 = parseRoom "a-b-c-d-e-f-g-h-987[abcde]"
  let room3 = parseRoom "not-a-real-room-404[oarel]"
  let room4 = parseRoom "totally-real-room-200[decoy]"
  print $ map isValid [room1, room2, room3, room4]
  s <- readFile "input"
  print $ name . decryptName . parseRoom $ "qzmt-zixmtkozy-ivhz-343[xxxx]"
  print $ star1 s
  print $ star2 s
