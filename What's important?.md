I dislike a lot of programming languages. Lately it's gotten even worse, I can not find a programming language I really like.

Being a programmer I quickly came to the conclusion that I could of course write my own programming language. But what should it be like? That journey became more and more complex as I dove deeper. 

A few of my feelings about programming:
Why the hell is it all so difficult?
How can it be that so many people accept dysfunctional software?
Why are we still creating software as if we are in the 70's?
If open source is so great, why do we have vendor lock-in on a language level?
Why is the gap between 'writing software' and 'knowing how software works' so big?
There probably is a lot more points to this.

Have you ever tried to read a paper produced by a university that might have been related to something you were working on?
Have you ever looked at the source code of the compiler or interpreter of your favorite programming language?
Did you ever get the feeling that we have made it all so difficult for ourselves?

Running around on the internet trying to find my way I learned quite a few things. One of the more interesting ones (for me) was related to mathematics. It became clear that mathematics uses programming languages themselves and that there exists a branch of mathematics that is concerned with building up theories. If you were ever in a discussion about something, it's this branch of mathematics that you were leaning on. It's main concern is to limit the amount of assumptions. Their word for it is 'axiom', which is: "a premise or starting point of reasoning".

One of the languages used in the aforementioned branch of mathematics is category theory. Their assumptions are pretty basic and are closely related to some schools of philosophy. They assume 'something exists' and that it's alright to say 'if something exists' and that those two are interchangeable. They also allow themselves to say that if something exists, it's reasonable to say that other things exist as well. It's also assumed that there are relations between things. I could continue following this line of reasoning, but it should be clear that their axioms stem from the assumption that something exists (which is kinda practical if you want to talk, or reason about something). A more important insight is that category theory provides a way of describing things in a formal or structured language. This allows them to describe most (if not all) other constructions.

Is category theory useful? Well, it might be a lot more useful if it was transformed into something that felt less alien. On the other hand, that's what it is, it's about the abstract world, not the concrete one and so it naturally feels alien. For me personally it helped in providing different glasses. It gave me another way of looking at structure: what can I discover. What's currently missing is some type of search engine that could tell me what I have found. An example:

```
container.map(x => x + 1) 
```

Abstracting over that bit of code I could write it as `C[X] -> (X -> Y) -> C[Y]`.  I have a container with `x`'s, I give it a function from `x` to `y` and it will give me back a container with `y`'s. Category theory knows about these types of transformations, it has names for them, but more importantly, I could tell me what other things are possible with a container that has this ability. Theoretically it could help me by saying that if I added a function with the following signature `C[C[X]] -> C[X]` and a function like this `X -> C[X]`, I would be able to other interesting stuff. Why don't we have a programming language that gives me that kind of help?

Another example:

```
Option(A) = Some(A) | None
```

Why don't I have a programming language that would give me the following functions for free? It could easily derive the correct implementation of those functions:

```
Option(A) -> A -> A                        // option.getOrElse(a)
Option(A) -> (A -> B) -> Option(B)         // option.map(x => x + 1)
Option(Option(A)) -> Option(A)             // option.flatten
Option(A) -> (A -> Option(B)) -> Option(B) // option.flatMap(x => optionY)
Option(A) -> Option(A) -> Option(A)        // option.orElse(otherOption)
Option(A) -> Boolean                       // option.hasValue
...
```

Imagine the amount of code reduction in libraries,

As you can see, even from my pseudo code, I have more experience in certain kinds of languages than others, but what is the difference?

```
class Test(a) {
  def x = a + a
}

b = new Test(1)
b.x // 2
```

```
(define Test (lambda (a) 
  (cond (selector) 
        ((= selector x) (+ a a))
  )
))

(define b (Test 1))
(b x) ; 2
```

During my quest of finding the differences one interesting aspect was the when and where of reporting problems. Most programming languages seem to take an all or nothing approach. A lot has been said about these differences using words like typed, untyped, singly typed languages. The main differences in my opinion is when I (as a developer) get notified of the problem: before or while the code is run to perform a task for my end-user. There is no developer in the world that disagrees with me when I say that 'before' is better than 'while'. The question is: what is the developer willing to do (or give up) to get more of the 'before'? An example:

```
(function() {
  addGreenMonsters(3)

  function addGreenMonsters(arr) {
    return arr.map(x => x + "green")
  }
})()
```

In 99.9999% of the cases this is a mistake. But because 'theoretically' the `prototype` of `3` could have been augmented with a `map` method, we wait until run time to report the problem. Why don't we (as users of programming languages) have the ability to state our conventions and more help from our tools?

Some languages allow us to add more information to help the tools (usually called compilers) to provide more help:

```
trait Test {
  def test: Int
}

def doIt(a: Test): Unit {
  ...
}
```

```
(ann doIt [{:test [Int]} -> Unit])
(defn doIt [a]
  (...))
```

Some languages insist in providing a name for the group of things (or type) while others only describe their abilities (functions). Which approach to use seems a philosophical matter. We can call something an orange, but is that necessary when all we do is eat it? If all we do is to eat it, would it matter if we were given an apple?

Do we really need a number for `add(a) = a + a`, or just something that has the function `+`?
If we wanted to describe a number, would we list all possible operations?

When reading about programming language you inevitably come across discussions that involve 'purists' and 'pragmatists'. 
"If you keep your data structures immutable, you gain a lot"
"The real world has finite amounts of memory, that needs to be mutable"
This, again, is a very philosophical argument. We live in a finite world with infinite concepts. A circle has an infinite amount of points, but screen it is drawn on has not. Vector graphics are an interesting solution to the problem. They allow the user to zoom in as far as he wants. The user could not tell you if he was looking at a real circle or a clever trick to sample the formula. The funny thing is that mathematics tell us that things can be considered the same if you can not tell them apart, even though they are different.

During these adventures I discovered that code is often optimized for performance at the expense of other useful features (most notably readability, safety and provability). The weirdest thing I discovered is that this is most often done in languages that have a compiler. Even weirder is that (self hosting) compilers have the same type of 'optimized' code. It seems such a waste that the internals of a compiler are polluted while they are able to detect those patterns output the optimized version. The real world might need some ugliness, but there is no need to write all things ugly using hideous handwriting.

Another thing I keep bumping into is the feeling of being overwhelmed by complexity. I know from experience that some problems are inherently complex. What I see however is that do not make enough use of in software of the 'proven' solutions. We could learn a lot from the scientific field in this regard. When the writer of a paper proposes something he conveniently refers to the work (and proofs) of other writers instead of building his argument up from the minimal amount of axioms. This concept is one of the fundamental concepts in programming: abstraction.

In programming we however have a problem: incompatible languages. Problems that are trivial in one language are complex in the other. And even if it's not the language itself, it's that domain experts express themselves in different languages. For example: programmers that are very good at solving problems where the facts are stated and questions are asked are likely to be found in the logic corner of languages. It would be awesome if I could use those libraries in any other language without first 'porting' them.

The commercial programmer has perfected the skill of creating highly maintainable and performant software, but lacks the time to investigate his chosen solution. The academic programmer has perfected the skill of figuring out the different solutions to a particular problem, but lacks the motivation or skill to create highly maintainable and performant software. To me it's clear these world should be unified. We (as the human race) know the theory and have the means to implement it in the real world.

```
if (a) b else c
=
switch(a) {
  true  -> b
  false -> c
}
```

```
switch(a) {
  x -> b
  y -> c
  ...
}
=
(cond (a)
  (x b)
  (y c)
  ...
)
```

Our human brain can easily see the patterns. Creating a program for this, especially if you have multiple rules like that is pretty complex if you lack the deep knowledge of that domain. It would however grant us the ability to define our own syntax for arbitrary programming languages. It could then be used to translate subsets of language specific libraries to other libraries.

To come back to the original question of what the programming language should be like: I don't know yet. But a few things are clear:
I want syntax to be free and interchangeable
The mathematical formula should be married with the implementation
The language should help whenever it can, the user of the language should be able to choose his trade-offs
Functions should not be trapped in libraries, the work of others should be indexed and findable
...

The world of mathematics is realizing different fields are talking about the same problems in different languages (google homotopy type theory), let's add the worlds of programming and logic circuits.
