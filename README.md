# Bulls and cows (.hs)
This repository contains a Haskell implementation of a Mastermind (also known as "bulls and cows") player.
More information on the game [here](http://en.wikipedia.org/wiki/Bulls_and_cows).

The choice of Haskell as programming language is for educational purposes, as I started writing this code
when I was desperately looking for something useful that made it worth to learn such an elegant, efficient but
overall speaking quite difficult functional programming language. [Haskell](https://www.haskell.org/) rocks.

On functional programming: [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/)

On Haskell: [Learn You a Haskell for a Great Good!](http://learnyouahaskell.com/)


## How to use it
Make sure you have the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) installed on your computer
(check [here](https://www.haskell.org/ghc/distribution_packages) for OS-specific packages).
Then simply launch a terminal, navigate to the repository and hit
```
user@host$ make
```
This will compile the solver and produce an executable file named `bullscows`. To test it interactively,
simply launch it and it will start printing guesses to the screen, to which the user should reply with
(consistent) feedbacks of the form
```
b c
```
where `b` is the number of right digits in the right position (bulls), and `c` is the number of right digits
in wrong positions (cows). The game ends either when the `4 0` feedback is given by the user, or when inconsistent
feedbacks are detected, in which case the solver prints `error` and terminates. An example of user-solver
interaction is the following (the sequence to guess is `1 2 3 4`):
```
user@host$ ./bullscows 
9 4 8 0
0 1
0 1 2 3
0 3
1 0 5 2
1 1
3 0 1 6
0 2
1 2 3 4
4 0
```

## To be continued
The code is very simple and was more of an exercise than a serious project. Many things need to be done, mainly
on the coding style and performance sides. Stay tuned for updates.
