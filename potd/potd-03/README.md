POTD 3 --- The Luhn algorithm, part 1
==============================

Your work is to implement the first part of the Luhn algorithm, which is a simple checksum formula used to validate a variety of identification numbers, such as credit card numbers and social security numbers.

For our purposes, the inputs to the Luhn algorithm will be identification numbers represented as whole numbers. For example, the social security number `409-52-2002` would be represented as `409522002`. Thus, you can assume that all of the inputs will be non-negative.

Here is the basic outline of the Luhn algorithm:
1) Double the value of every second digit beginning from the right. 
That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. 
For example, `[5,5,9,4]` becomes `[10,5,18,4]`.

2) Sum the digits of all the values, no matter if the original value was doubled or not. 
For example, `[10,5,18,4]` becomes `(1 + 0) + 5 + (1 + 8) + 4 = 19`.

3) Mod the sum by `10`. If the result is `0`, then the number is valid.
For example, `19 % 10 = 9`. Thus, `[5,5,9,4]` would not be a valid identification number.

You will implement this algorithm in two parts. For the first part, implement the functions `toDigits` and `doubleEveryOther`.

The `toDigits` function converts an integer into a list of its single digits.
```
toDigits 1234 == [1,2,3,4]
toDigits 0 == []
```

The `doubleEveryOther` function doubles the values of every second digit beginning from the right.
```
doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleEveryOther [1,2,3] == [1,4,3]
```

Put the code for these functions in `src/Lib.hs`.
