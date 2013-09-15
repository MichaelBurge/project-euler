letters 0 = 4
letters 1 = 3
letters 2 = 3
letters 3 = 5
letters 4 = 4
letters 5 = 4
letters 6 = 3
letters 7 = 5
letters 8 = 5
letters 9 = 4
letters 10 = 3
letters 11 = 6
letters 12 = 6
letters 13 = 8
letters 14 = 8
letters 15 = 7
letters 16 = 7
letters 17 = 9
letters 18 = 8
letters 19 = 8
letters 20 = 6
letters 30 = 6
letters 40 = 5
letters 50 = 5
letters 60 = 5
letters 70 = 7
letters 80 = 6
letters 90 = 6
letters n | n < 100 = (letters (n - (n `rem` 10))) + (letters $ n `rem` 10)
letters n | n < 1000 = (letters (n `div` 100)) + (length "hundred") + 
                       if (n `rem` 100) == 0
                          then 0
                          else (length "and") + (letters (n `rem` 100))
letters 1000 = (letters 1) + length "thousand"