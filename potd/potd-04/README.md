POTD 4 --- The Luhn algorithm, part 2
==============================

Your work is to implement the second part of the Luhn algorithm. You implemented the first part of this algorithm in POTD 3, and we have now included the solution to those functions in `src/Lib.hs`. You can also replace those solutions with your own if you would like.

Here is a recap of the Luhn algorithm:

1) Double the value of every second digit beginning from the right. 
That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example, `[5,5,9,4]` becomes `[10,5,18,4]`.

2) Sum the digits of all the values, no matter if the original value was doubled or not. For example, `[10,5,18,4]` becomes `(1 + 0) + 5 + (1 + 8) + 4 = 19`.

3) Mod the sum by `10`. If the result is `0`, then the number is valid. For example, `19 % 10 = 9`. Thus, `[5,5,9,4]` would not be a valid identification number.

You will now implement the second half of the algorithm:

The `sumDigits` function calculates the sum of all the digits in the given list.

```
sumDigits [] == 0
sumDigits [16,7,12,5] == 22
```

The `validate` function takes an identification number and determines whether this number is valid according to the Luhn algorithm. This will use all functions defined in the previous exercises.

```
validate 4012888888881881 == True
validate 4012888888881882 == False
```

Put the code for these functions in `src/Lib.hs`.
