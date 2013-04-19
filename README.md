scalagp
=======

scalagp is a library for automatically generating programs by genetic programming. Genetic programming is a technique that searches for good solutions to a problem in a way analagous to how living organisms evolve over generations in response to the pressures of their environment.

Genetic programming begins by making a large pool of random programs out of a small number of functions. Every program is then scored for "fitness" against some standard, where a lower fitness indicates a better program. We then breed a new generation of programs from the current generation, in such a way that programs with a better fitness score are probably going to get to reproduce and hand down some or all of their code to the next generation. They correspond, in the natural world, to organisms who have done well, and probably lived a long time, and had many children. Over time, in both the natural and digital world, the average fitness of the population will improve.

scalagp implements a general infrastructure for this kind of problem, which allows you to concentrate on the more interesting question of modelling the domain-specific parts of the system.

scalagp is currently in alpha. I've used it to solve some toy problems (see the scalagp-examples subproject), but there is also lots more I would like to add or polish (see the TODO file). Patches and constructive commentary are plenty welcome.

Quickstart
==========

Suppose you wanted to find a way to construct the XOR function of inputs A and B, building it out of the OR, AND and NOT functions. We could think of a potential XOR function built this way as a tree with A or B in each leaf; and OR, AND, or NOT in each branch. So one tree made of these elements might look like:

    AND ---- B
    \
     \
      \
       OR -- B
        \
         \
          \
           A

which we could also write as an S-expression
    (AND B (OR B A))


