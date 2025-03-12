# TypeLoopS1: Exploring the Fundamental Group of S¹ via Type-Level Haskell

TypeLoopS1 is an educational Haskell program that demonstrates the fundamental group of a circle (π₁(S¹)) using both type-level and runtime representations. This program showcases how Haskell's advanced type system can be used to encode sophisticated mathematical concepts and prove properties at compile time.

## Mathematical Background

In algebraic topology, the **fundamental group** of a topological space consists of equivalence classes of loops, where loops are considered equivalent if one can be continuously deformed into the other.

For a circle (S¹), each loop is characterized by its **winding number**:
- How many times the loop wraps around the circle
- Positive numbers represent counterclockwise wrapping
- Negative numbers represent clockwise wrapping
- Zero represents a loop that can be contracted to a point (the identity element)

The fundamental insight is that π₁(S¹) ≅ ℤ (the fundamental group of a circle is isomorphic to the integers), with:
- Loop composition corresponding to integer addition
- Loop inversion corresponding to integer negation
- The identity loop corresponding to zero

## Implementation Highlights

This program demonstrates these concepts in two complementary ways:

### 1. Static (Type-Level) Implementation

The static implementation encodes the fundamental group structure at the type level:

- **Type-Level Integers**: A custom type-level representation of integers using GADTs and type families
  ```haskell
  data TInt = Pos Nat | Neg Nat | Zero
  data Nat = Z | S Nat
  ```

- **Type-Level Operations**: Addition, negation, and comparison implemented as type families
  ```haskell
  type family TAdd (a :: TInt) (b :: TInt) :: TInt
  type family TNegate (a :: TInt) :: TInt
  ```

- **Type-Safe Loop Representation**: A GADT parametrized by the winding number
  ```haskell
  data StaticLoop (n :: TInt) where
    MkStaticLoop :: StaticLoop n
  ```

- **Compile-Time Proofs**: Uses type equality to verify group properties
  ```haskell
  proofStatic :: StaticLoop T1 -> (StaticLoop (TAdd T1 TNeg1) :~: StaticLoop T0)
  proofStatic _ = Refl
  ```

### 2. Dynamic (Runtime) Implementation

The dynamic implementation provides an interactive way to explore these concepts:

- **Runtime Loop Representation**: Winding numbers as regular integers
  ```haskell
  newtype DynamicLoop = DynamicLoop { getDynWinding :: Int }
  ```

- **Interactive Commands**: Compose loops, invert loops, and create loops with specific winding numbers
  ```
  compose 2 3    -- Compose loops with winding numbers 2 and 3
  inverse -1     -- Invert a loop with winding number -1
  identity       -- Get the identity loop
  power 5        -- Create a loop with winding number 5
  ```

## Type-Level Programming Techniques

This program demonstrates several advanced type-level programming techniques:

1. **Generalized Algebraic Data Types (GADTs)** for type-indexed values
2. **Type Families** for type-level functions
3. **DataKinds** for promoting data constructors to the type level
4. **Type-Level Arithmetic** for computing with types
5. **Type Equality** for proving relationships between types

## Building and Running

### Prerequisites

- GHC (Glasgow Haskell Compiler) 8.6 or newer
- The following language extensions:
  - DataKinds
  - GADTs
  - KindSignatures
  - TypeOperators
  - TypeFamilies
  - UndecidableInstances
  - ScopedTypeVariables
  - PolyKinds

### Compilation and Execution

```bash
# Compile the program
ghc --make Main.hs -o TypeLoopS1

# Run the program
./TypeLoopS1
```

Alternatively, you can rename the module and file to match the repository name:

```bash
# If you rename Main.hs to TypeLoopS1.hs and change the module declaration
ghc --make TypeLoopS1.hs -o TypeLoopS1
```

## Usage Guide

### Main Menu Commands

- `dynamic` - Enter the dynamic (runtime) simulation
- `static` - View the static (type-level) proof demonstration
- `help` - Display available commands
- `quit` - Exit the program

### Dynamic Simulation Commands

- `compose n m` - Compose loops with winding numbers n and m
- `inverse n` - Get the inverse of a loop with winding number n
- `identity` - Display the identity loop
- `power n` - Create a loop with winding number n
- `back` - Return to the main menu

## Educational Value

TypeLoopS1 serves as an educational tool for exploring:

1. **Homotopy Type Theory** - The connection between topology and type theory
2. **Algebraic Topology** - The fundamental group concept and its properties
3. **Type-Level Programming** - Advanced techniques in Haskell's type system
4. **Group Theory** - The algebraic structure of groups in a concrete example

## Extending the Program

Possible extensions to this program include:

1. **Higher Homotopy Groups** - Extending to π₂(S²) and beyond
2. **More Complex Spaces** - Implementing fundamental groups of other spaces
3. **Visualization** - Adding graphical representations of the loops
4. **Dependent Types** - Exploring connections with dependent type theory
5. **Additional Proofs** - Demonstrating more complex group-theoretic properties

## Mathematical Details

### Group Structure

The fundamental group π₁(S¹) satisfies the axioms of a group:

1. **Closure**: For any loops a and b, their composition a·b is also a loop
2. **Associativity**: For loops a, b, c: (a·b)·c = a·(b·c)
3. **Identity**: There exists an identity loop e such that e·a = a·e = a
4. **Inverse**: For any loop a, there exists an inverse a⁻¹ such that a·a⁻¹ = a⁻¹·a = e

### The Loop Inverse Axiom

A key property demonstrated in the code is that composing a loop with its inverse yields the identity element:

```
loop · (inverse loop) = identity
```

The program proves this property at the type level for the fundamental loop (winding number 1):

```haskell
proofStatic :: StaticLoop T1 -> (StaticLoop (TAdd T1 TNeg1) :~: StaticLoop T0)
```

This corresponds to the mathematical statement: 1 + (-1) = 0, which is verified by the Haskell type checker.

## License

This program is provided as open educational material. You are free to use, modify, and distribute it for educational purposes.

## Acknowledgments

This program draws inspiration from:
- Homotopy Type Theory research
- Advanced type-level programming techniques in Haskell
- The mathematical theory of fundamental groups in algebraic topology
