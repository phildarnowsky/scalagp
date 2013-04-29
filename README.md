scalagp
=======

scalagp is a library for automatically generating programs by genetic programming. Genetic programming is a technique that searches for good solutions to a problem in a way analagous to how living organisms evolve over generations in response to the pressures of their environment.

Genetic programming begins by making a large pool of random programs out of a small number of functions. Every program is then scored for "fitness" against some standard, where a lower fitness indicates a better program. We then breed a new generation of programs from the current generation, in such a way that programs with a better fitness score are probably going to get to reproduce and hand down some or all of their code to the next generation. They correspond, in the natural world, to organisms who have done well, and probably lived a long time, and had many children. Over time, in both the natural and digital world, the average fitness of the population will improve.

scalagp implements a general infrastructure for this kind of problem, which allows you to concentrate on the more interesting question of modelling the domain-specific parts of the system.

scalagp is currently in alpha. I've used it to solve some toy problems (see the scalagp-examples subproject), but there is also lots more I would like to add or polish (see the TODO file). Patches and constructive commentary are plenty welcome. It is also a safe bet that the API and internals will both change frequently for a while.

Quickstart
==========

(Code in this section can be seen, in runnable shape, in scalagp-examples/src/main/scala/Xor.scala. A sample transcript also follows).

Our basic strategy is to generate a large number of programs, rate them all on some scale, and give the better-rated programs more of a chance to pass their "DNA" to the next generation of programs. We keep the cycle going until we have a good enough program, or we've used up as much time and CPU as we care to on this problem.

So let's look first at the raw materials.

Suppose you wanted to find a way to construct the XOR function of two inputs A and B. You have AND, OR and NOT gates at your disposal. We could think of a function made out of these primitives as a tree with A or B in each leaf; and OR, AND, or NOT in each branch. So one tree made of these elements might look like:

    AND ---- B
    \
     \
      \
       OR -- B
        \
         \
          \
           NOT ---- A

which we could also write as an S-expression

    (AND B (OR B (NOT A)))

So the inputs A and B would be our __terminals__; and OR, AND and NOT would be our __nonterminals__.

We also need a __fitness function__ that takes a program like the one above and scores it: specifically, it should return a non-negative real number, with a lower score representing a more fit program. So a program with perfect fitness, if such a thing is possible with the problem we're looking at, should have a fitness score of 0.

In this XOR example, one suitable function would be the number of mistakes the potential XOR function makes. In other words, if we pass this candidate each the 4 possible Boolean-valued combinations of A and B, on how many does it get the wrong answer? If you draw out the truth tables, you'll see that this example program gives the wrong answer in 2 out of 4 cases, so its fitness would be 2.0.

Representing our terminals, the A and B inputs, is easy. What we want in the end is a program that takes a pair of booleans and performs the XOR operation on them, returning another boolean. We'll also want all of our terminals and nonterminals to have the same structure. So all our A input terminal has to be is a function that takes a pair of booleans and returns the first:

```scala

class BooleanPair(override val _1: Boolean, override val _2: Boolean) extends (Boolean, Boolean)(_1, _2)

abstract class BooleanPairFunction extends (BooleanPair => Boolean)

object AFunction extends BooleanPairFunction {
  def apply(pair: BooleanPair) = pair._1
}
```

and we can define our B input terminal similarly, returning the second value in the pair. Note that we use "object AFunction" to declare a singleton AFunction.

Writing non-terminals is a little trickier, since, say, an OR is a function of a pair of inputs that can themselves be functions. So to instantiate a non-terminal node we need to know what the branches below it are doing, and the code looks like

```scala
class OrFunction(children: IndexedSeq[BooleanPairFunction]) extends BooleanPairFunction {
  def apply(pair: BooleanPair) = children(0)(pair) || children(1)(pair)
}
```

That is, we evaluate each of our inputs with the pair we were given, then OR them together.

The last important ingredient is our fitness function. We want to get passed in a function of the type that we're generating out of our terminals and non-terminals, and return a Double that indicates how many mistakes the function made while iterating over possible input pairs. So:

```scala
object XorFitnessFunction extends ProgramFitnessFunction[BooleanPairFunction] {
  val testCases = List(
    (new BooleanPair(false, false), false),
    (new BooleanPair(false, true),  true),
    (new BooleanPair(true, false),  true),
    (new BooleanPair(true, true),   false)
  )
  def apply(candidate: BooleanPairFunction) = testCases.count {(testCase) => 
    val pair = testCase._1
    val expectedResult = testCase._2

    candidate(pair) != expectedResult
  }
}

```

With our other terminals and non-terminals implemented, and some glue code added (not shown here but all together in Xor.scala), we can run something like we do in the sample transcript below, and come up with an S-expression for XOR:

    (AND (NOT (AND A B)) (OR (AND (AND B B) A) (AND (OR B A) (OR A (OR (NOT A) (NOT B))))))

which you can reduce by hand:

    (AND (NOT (AND A B)) (OR (AND (AND B B) A) (AND (OR B A) (OR A (OR (NOT A) (NOT B))))))
    (AND (NOT (AND A B)) (OR (AND (AND B B) A) (AND (OR B A) true)))
    (AND (NOT (AND A B)) (OR (AND (AND B B) A) (OR B A)))
    (AND (NOT (AND A B)) (OR (AND B A) (OR B A)))
    (AND (NOT (AND A B)) (OR B A))

and verify that that simplified expression is indeed XOR. So, we got an inelegant but correct solution. This is characteristic of an evolutionary method like GP. 

Generating an expression for XOR that's twice as complicated than it really needs to be isn't so impressive. But this is the toy-est of toy problems. With the proper setup, evolutionary methods can find novel results to hard problems. A list of several dozen examples is at http://www.genetic-programming.org/combined.html.

Sample transcript
=================

```scala
scala> import com.darnowsky.scalagp_examples.Xor._
import com.darnowsky.scalagp_examples.Xor._

scala> import com.darnowsky.scalagp.Population._
import com.darnowsky.scalagp.Population._

scala> val pop = XorPopulation.create(20, 5)
pop: com.darnowsky.scalagp.Population.Population[com.darnowsky.scalagp_examples.Xor.BooleanPairFunction] = Population 910728460 (generation 1)

scala> val result = Population.run(pop, Some(Population.printGenerationStatistics))
*******************
GENERATION 1
BEST FITNESS OF CURRENT GENERATION: 1.0
BEST FITNESS OF RUN: 1.0
CURRENT TIME: Wed Apr 24 23:07:46 EDT 2013
*******************
GENERATION 2
BEST FITNESS OF CURRENT GENERATION: 1.0
BEST FITNESS OF RUN: 1.0
CURRENT TIME: Wed Apr 24 23:07:46 EDT 2013
*******************
GENERATION 3
BEST FITNESS OF CURRENT GENERATION: 1.0
BEST FITNESS OF RUN: 1.0
CURRENT TIME: Wed Apr 24 23:07:46 EDT 2013
*******************
GENERATION 4
BEST FITNESS OF CURRENT GENERATION: 1.0
BEST FITNESS OF RUN: 1.0
CURRENT TIME: Wed Apr 24 23:07:46 EDT 2013
*******************
GENERATION 5
BEST FITNESS OF CURRENT GENERATION: 1.0
BEST FITNESS OF RUN: 1.0
CURRENT TIME: Wed Apr 24 23:07:46 EDT 2013
result: com.darnowsky.scalagp.Population.Population[com.darnowsky.scalagp_examples.Xor.BooleanPairFunction] = Population -2051724636 (generation 6)

scala> result.bestOfRun._1.toSExpression
res0: String = (AND (NOT (AND A B)) (OR (AND (AND B B) A) (AND (OR B A) (OR A (OR (NOT A) (NOT B))))))
// Not real elegant, but it's correct.

scala> result.bestOfRun._2 
res1: Double = 0.0
// fitness, where 0.0 is ideal and higher values are worse

scala> val xorFunc = result.bestOfRun._1.evaluate
xorFunc: com.darnowsky.scalagp_examples.Xor.BooleanPairFunction = <function1>

scala> xorFunc(new BooleanPair(false, false))
res2: Boolean = false

scala> xorFunc(new BooleanPair(false, true))
res3: Boolean = true

scala> xorFunc(new BooleanPair(true, false))
res4: Boolean = true

scala> xorFunc(new BooleanPair(true, true))
res5: Boolean = false
```
