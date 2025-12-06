# Day 6: Trash Compactor ([link](https://adventofcode.com/2025/day/6))
## Part 1
We take all the lines containing numbers, convert them from a matrix[^1] of `Char`s (since `Strings`s are just lists of `Char`) to a matrix of `Integer`s. Then we [transpose](https://en.wikipedia.org/wiki/Transpose) the resulting matrix so that we can process each column as a normal list.  
After having finished with the numbers, we process the last line, which contains a list of `"+"` and `"*"` and we convert it to a list of pairs, having as first element the function (either `(+)` or `(*)`) and as second element the neutral value for the specific function/operation (so either `0` or `1`).  
Lastly, we take each line of the new matrix and apply to it (along with the neutral element stored in the pair) the corresponding function using [`foldl`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:foldl).  
Since each row of the matrix corresponds to a column in the input file, the `i`th operation (`(function,netutral element)` pair) has to be applied to the `i`th row of the matrix.
## Part 2
We take all the lines containing numbers, transpose them so we can read the actual numbers as they are encoded and convert them to `Integer`s.  
But now the problems is that we have each set of numbers only divided by an empty list (which got converted to that from the columns of spaces we have).  
To resolve that, we first split the matrix (which we just consider as a normal list here) using `[]` as delimiter (using [`splitOn`](https://hackage.haskell.org/package/split/docs/Data-List-Split.html#v:splitOn)), then we `concat` each split group so we have a new matrix that is just like the one in Part 1, but with different numbers because of the different encoding.  
Now we do the rest in the same way as Part 1, process the line with the operations and apply them to the correct column.

[^1]: Since the only matrices (or "matrixes"?) libraries I found either wouldn't compile ([`matrix`](https://hackage-content.haskell.org/package/matrix)) or had matrices with only `Double`s ([`hmatrix`](http://hackage.haskell.org/package/hmatrix)), here I mean "matrix" as "a normal list of lists", like in C.
