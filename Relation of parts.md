Parts
- AST (Abstract Syntax Tree)
- IR (Intermediate Representation)
- Bindings (free and bound references)
- Type Inference
- Type Checking

The AST represents the intention of the user of the language. It is the public interface of the programming language. It determines which constructs the user can provide to the compiler.

The IR represents the internal model of the language. At this stage it makes sense to use a model that has been researched. A few examples of such models:
- λ [Lamda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
  - Any system (extension) from the [Lambda Cube](https://en.wikipedia.org/wiki/Lambda_cube) (or using a more generalized specification from [Pure Type Systems](http://www.rbjones.com/rbjpub/logic/cl/tlc004.htm), also see [PTS Lambda Cube](http://www.rbjones.com/rbjpub/logic/cl/tlc001.htm))
    - λ→  [Simply Typed Lambda Calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)
    - λ2  [Second Order Lambda Calculus](https://en.wikipedia.org/wiki/System_F) (Polymorphic Lambda Calculus or System F)
    - λΠ  [Lambda Pi Calculus](https://en.wikipedia.org/wiki/Dependent_type#First_order_dependent_type_theory) (Dependently Typed Lambda Calculus)
    - λω  [Weak Lambda Omega Calculus](https://en.wikipedia.org/wiki/Type_constructor) (ω written with underline, Simply Typed Lambda Calculus with Type Operators)
    - λω  [Lambda Omega Calculus](https://en.wikipedia.org/wiki/System_F#System_F.CF.89) (System Fω)
    - λΠ2 [Second Order Lambda Pi Calculus](https://en.wikipedia.org/wiki/Dependent_type#Second_order_dependent_type_theory)
    - λΠω Weak Lamda Pi Omega Calculus (ω written with underline)
    - λΠω [Calculus of Constructions](https://en.wikipedia.org/wiki/Calculus_of_constructions) (Higher Order Dependently Typed Polymorphic Lambda Calculus)
- π [Pi Calculus](https://en.wikipedia.org/wiki/%CE%A0-calculus)
- κ [Kappa Calculus](https://en.wikipedia.org/wiki/Kappa_calculus)
- [System F<:](https://en.wikipedia.org/wiki/System_F-sub) (System F sub, [Bounded quantification](https://en.wikipedia.org/wiki/Bounded_quantification))
- [DOT Calculus](http://infoscience.epfl.ch/record/215280/files/paper_1.pdf)
- [SK Calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
- [SF Calculus](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf)
- [Pure Pattern Calculus](http://link.springer.com/content/pdf/10.1007%2F11693024_8.pdf)
- λN [Lambda Calculus with Names](http://scg.unibe.ch/archive/oosc/PDF/Dami95aLambdaN.pdf)
- λμ [Lambda Mu Calculus](https://en.wikipedia.org/wiki/Lambda-mu_calculus)
- μ [Mu Calculus](https://en.wikipedia.org/wiki/Modal_%CE%BC-calculus)

This list is not complete. Some of the models are extensions of other models while others are not compatible. There is a ton of information out there, but it seems there is no structured overview of features.

Note that the choice of model greatly influences the following steps (binding, type inference and type checking). For example: in some models names of bindings are insignificant (and can be replaced) while in other models they are. Another example is the constructions that can be encoded. The lambda cube clearly shows the relation between features and models.

The choice of a model might also be influenced the other way around. Most models come equiped with rules for binding which you might want to use, some models are even equiped with type inference. An example is the [Hindley-Milner Type System](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system). For the lambda calculus the types are similar to a dynamic language, the only type (which thus can easily be inferred) is a single recursive type.

The business of bindings usually deals with referencing other parts of the program. The usual distinction are bound and free symbols; Abstract binding trees are a way to encode these properties. This stage however can also be used to determine the primitives or encode symbols in the model. When working with the lambda calculus as a model you could convert the symbol `3` to the appropriate lambda. If you are working with a typed model you could bind `3` to a type (usually called a primitive type). In most programming languages this is done in the parser that produces the AST. So instead of during binding, the transformations are done when moving from the AST to the IR.

Type inference is used to 'fill in the blanks'. For dynamic languages this step is rather trivial, all types are inferred to a single type and thus type checking always succeeds. Other languages have various degrees of type inference. Some enforce strict rules on the available features so that types can be completely inferred (no extra information from the programmer). Others require types to be given at certain locations (for example function arguments) and some require the programmer to explicitly mention the types for all forms of abstraction.

Type checking. This checks if the model adheres to all the acompanying rules. Big influences here are [nominal](https://en.wikipedia.org/wiki/Nominal_type_system) vs [structural](https://en.wikipedia.org/wiki/Structural_type_system).
