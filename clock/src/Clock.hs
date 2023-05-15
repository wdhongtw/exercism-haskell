module Clock (addDelta, fromHourMin, toString) where
import Text.Printf (printf)

data Clock = Clock Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = unify $ Clock hour minute 

toString :: Clock -> String
toString (Clock hour minute) = printf "%02d:%02d" hour minute

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock toHour toMinute) = 
  fromHourMin (toHour + hour) (toMinute + minute)
  -- unify added
  -- where
  --   added = Clock (toHour + hour) (toMinute + minute)

unify :: Clock -> Clock
unify (Clock oldHour oldMinute) = 
  Clock newHour newMinute
  where
    newMinute = oldMinute `mod` 60
    newHour = (oldHour + (oldMinute `div` 60)) `mod` 24
