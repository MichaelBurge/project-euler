import Data.List

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
             deriving Eq

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
           deriving (Show, Eq)

daysIn m y = case m of
  January -> 31
  February -> if 0 == y `mod` 4 && (0 /= y `mod` 100 || 0 == y `mod` 400)
                 then 29
                 else 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

days = cycle [Monday,Tuesday, Wednesday, Thursday,Friday, Saturday, Sunday]
months = cycle [January, February, March, April, May, June, July, August, September, October, November, December]
firsts (m:ms) ds y = (head ds) : (firsts ms (drop (daysIn m y) ds) (if m == December then y+1 else y))