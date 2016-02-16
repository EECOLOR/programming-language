This document is an attempt to sketch my vision. Students (or people in the JavaScript and PHP community) are not afraid to try and encode their ideas. I want to try and follow in their footsteps.

In theory I like the Scala programming language, in practice I don't. There are way too many bugs and inconsistencies for me comfortably express myself.

I am convinced that my (imaginary) perfect programming language is not rooted in text, but richer. Before I can work on that, I first need to find some more traditional programming language that makes me feel comfortable. That is the basis for this project: to create a language that I feel comfortable writing in.

### Functions

One of the most important factors is consistency. I want my language to have clear semantics. While exploring the vast amount of documentation on related math theories I found that arrows (or functions) are often used to describe semantics. From a programmer point of view, Conal Elliott made a very strong case for this with [Denotational design with type class morphisms](http://conal.net/papers/type-class-morphisms/).

This observation made me think: what if I can translate language concepts to functions? If I can do that, I will have very clear semantics. So I started fiddling and came up with a few examples. I'll show a few, so you can get the idea:

**Product**

A product is a combination of things. For example `A` *and* `B`, or written in Scala style `(A, B)`. The meaning of this is that you have two values and you can use either, both or none to perform some computation. Translated to a function it would be:

```
(A => B => C) => C
```

So if I have a product `val x: (A, B)` I can translate it to `val x: (A => B => C) => C`. Notice how it would be used:

```
val x: (A => B => C) => C = f(a)(b)

val a: A = x(a => b => a)
val b: B = x(a => b => b)
val c: C = x(`create c from a and b`)
```

**Coproduct**

In my mind I also call this a choice. Where you have one of the given things. For example `A` *or* `B`, or written in a different style `A | B`. The meaning of this is that you can do something with any of the choices as long as you are prepared to deal with all of the choices. Translated to a function it would be:

```
(A => C) => (B => C) => C
```

So if I have a coproduct `val x: A | B` I can translate it to `val x: (A => C) => (B => C) => C`. Notice how it would be used:

```
val x: (A => C) => (B => C) => C = ifA => ifB => ifA(a)
val y: (A => C) => (B => C) => C = ifA => ifB => ifB(a)

val c: C = x(a => `create c from a`)(b => `create c from b`)
val d: D = y(a => `create d from a`)(b => `create d from b`)
```

This concept of representing things as functions is not new. Most of it has been explained as stuff that's related to Lambda Calculus. But for me it was refreshing to think about common Scala classes in this light:

```
type Option[A] = A | Unit
def Some[A](a: A): Option[A] = a
val None[A]: Option[A] = ()

// translate `def` and `type` to `val` and functions

val Option = (A: Type) => A | Unit
val Some = (A: Type) => (a: A) => a
val None = (A: Type) => ()

// translate `|` to function with the appropriate implementations

val Option = (A: Type) => (A => X) => (Unit => X) => X
val Some = (A: Type) => (a: A) => (ifSome: A => X) => (ifNone: Unit => X) => ifSome(a)
val None = (A: Type) => (ifSome: A => X) => (ifNone: Unit => X) => ifNone(())
```

Note that this construction lets me create all of the functions present on the `Option` class in the Scala library. More information about this concept (with `Option` as an example) in: [Debut with a Catamorphism](http://blog.tmorris.net/posts/debut-with-a-catamorphism/).

### Compiler

As for the compiler design: I honestly have no clue what it will turn out to be. There are however a few guidelines I try to follow:

- Readable code. It should be easy to understand. Take small steps going from one phase to the other.
- Less code is better. It's so easy to write a lot of code, if you can write less code, try to do so. It's ok to go slow if that means you can reach less code.
- Don't make it too trivial. It's ok to skip the `extends` keyword for now, but traits with member access should be in there.
- Try to use the type system to limit the possibilities.

I am currently using Scala as a language to implement it and that means I have to make a few concessions to the code. A few examples.

**Extends vs |**

I have defined a coproduct type `|` like this `type |[+A, +B] = Either[A, B]` and I would like to write `type Expression = Function | Application | ...`. This however becomes very tedious very fast. The combination of bugs in implicit resolution and type inference makes using that concept for more than 2 choices extremely hard. This forces me in many cases to use `extends` in combination with a pattern match later on, essentially hiding the types and then bringing them to the surface again.

This hinders composability as I have to handle groups instead of their individual parts and compose them together.

**Case classes that look the same**

In Scala it's impossible to do the following:

```
def (x: Value): Value with AstInformation = x with AstInformation { val ast = ... }
```

That means that if I want to use the same `Value` with different information attached to it, I have to create a similar case class. For now I have similar classes at different stages that are very similar apart from the extra information they carry.

**Product and function arguments**

A product `(A, B)` should be usable as an argument to `(A, B) => C`, it isn't. It can only be applied to a function `((A, B)) => C`. Massive amounts of boilerplate are the result of this.

### Parser

In the parser I am trying a non standard approach. I am parsing the structure of the code, but not things like primitive values. In most cases the parser allows a lot more than a standard Scala parser. My vision is that I can later on, when resolving references, resolve the reference `1` to the number `1` with type `Int` and `"test"` to the string `"test"` of type `String`. This would allow me to use `1` and `"test"` as method names if I wanted to.

The parser converts the bunch of characters into a set of case classes, all patterns the parsers knows should be translated 1 on 1, no interpretation of that structure should happen here.

### Translation

After the structure has been parsed it should move through separate phases. At the moment I am taking it step by step so a lot of the phases that lay in front of me are very fuzzy. An overview of what I am thinking:

1. *Tighten the result of the parser.* The case classes and types that come from the parser allow for combinations that don't make any sense. An example of this is unimplemented members in objects. In this step we report those problems and also inform the user of any (as of yet) unimplemented features.
2. *Translate to functions.* Here I try to translate all of the constructs to their representation as functions. The amount of building blocks is greatly reduced, at time of writing they are `Val`, `Block`, `Function`, `Application`, `Reference` and `FunctionType`.
3. *Resolving references*. In this part I should figure out what is referred to with a reference. Part of this phase is probably the ordering of `val` statements in a block (I don't like the limitation of having to define my `val` statements in order of usage). Another part is resolving to primitives.
4. *Type inference*. Have the compiler provide types fill in the details for things like type application or return types of methods. In my mind (and I might be completely wrong on this one) it should be possible to have no types declared anywhere. The type of function arguments will then be structural based on their usage.
5. *Type checking*. I have always thought of type checking as that simple game with square pegs and round holes: does it fit? At the moment (and after reading a lot of theory) I am more and more convinced that types are explained in a very complicated way. Especially if you combine the terms 'kind', 'type' and 'value'. A more naive approach seems to be: code that runs at compile time and code the runs at runtime. Another thing I'm fuzzy about is where to allow 'morpisms' to enter the typing world: should I allow `(A, B)` as an argument to something that requires an `A`? And, should I allow an `A` to be supplied when an `A | B` is required?
6. *Type removal*. Once the program has been checked to have all it's pegs in the holes of the correct shape the information can be removed. This should simplify the program immensely.
7. *Execution*. This is actually already needed for the type related bits. So there needs to be an interpreter. Having a simple representation of the program should also allow for easy translation to your favorite VM (java, javascript) or other execution environment.

I am not sure yet how and where in these translation steps I want to allow for customization, a few examples:
- allow a user to specify that the identifier `1/4` is actually a fraction with certain properties
- allow a user to convince the compiler that it's OK to use `X` as an argument to function that requires a `Y`

### Out of scope (for now)

This is an experiment that helps me to figure out where the limitations of my idea's are. If this thing is ever going to be successful it needs a lot more than the above, at the least:

- A different, more persistent representation than memory. Most likely a graph-like database that allows for visualization and incremental changes.
- A way to share code. In most programming languages it's way too hard to find methods with a certain signature and behavior that others have written.
- A standard library.
- API for editors, everything that the compiler knows about your program should be available to the editor.
- Some forms of pattern recognition, wouldn't it be awesome if the compiler would ask you the names of `Some` and `None` when you wrote `type Option[A] = A | Unit` and after that asked you if the appropriate category theory concepts (like the monad laws) should hold?
- Side effects... A very important concept that causes most of the complexity.

