-- Question 1
-- Write a multiline comment below.
{-
This is a multiline comment.
-}
-- Question 2
-- Define a function that takes a value and multiplies it by 3.
q2 x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.
q3 r = pi * r ^ 2

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder.
q4 h r = q3 r * h

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
q5 h r = q4 h r >= 42
