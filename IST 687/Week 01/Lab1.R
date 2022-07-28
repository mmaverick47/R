#Boolean expressions are statements that are TRUE or FALSE. 
#R returns Boolean expressions as TRUE or FALSE.
#To return Boolean results in R, you must use the AND, the OR, and the NOT operators.
#AND operator is &
#OR operator is | (shift+backslash)
#NOT operator is !

#Information found at https://towardsdatascience.com/the-complete-guide-to-logical-operators-in-r-9eacb5fd9abd

#AND

#AND takes two values and returns TRUE only if both are TRUE
#Example, TRUE & TRUE will return a TRUE result
#Example, TRUE & FALSE will return a FALSE result
#Example, FALSE & TRUE will return a FALSE result
#Example, FALSE & FALSE will return a FALSE result

#These lines show the Boolean AND function at work.
1 & 1
1 & 0
0 & 0
0 & 1

#OR

#OR takes two values and returns TRUE only if at least one is TRUE
#Example, TRUE | TRUE will return a TRUE result
#Example, TRUE | FALSE will return a TRUE result
#Example, FALSE | TRUE will return a TRUE result
#Example, FALSE | FALSE will return a FALSE result

#These lines show the Boolean OR function at work.
1 | 1
1 | 0
0 | 0
0 | 1

#NOT

#NOT is a negative, and will return FALSE if something is not true, and TRUE if something is not false
#Example, !TRUE == TRUE will return a FALSE result
#Example, !FALSE == TRUE will return a TRUE results

#These lines show the Boolean NOT function at work.
!1 == 1
!1 == 0
!0 == 0
!0 == 1
