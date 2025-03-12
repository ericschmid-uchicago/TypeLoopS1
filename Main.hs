{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, PolyKinds #-}

module Main where

import Data.Type.Equality ((:~:)(Refl))
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Kind (Type)

--------------------------------------------------------------------------------
-- Type-Level Integer Representation
--------------------------------------------------------------------------------

-- | Type-level natural numbers using Peano representation
data Nat = Z | S Nat

-- | Type-level integers represented as either positive, negative, or zero
data TInt = Pos Nat | Neg Nat | Zero

-- | Compare two natural numbers, returning an ordering
type family NatCmp (a :: Nat) (b :: Nat) :: Ordering where
  NatCmp 'Z 'Z = 'EQ
  NatCmp 'Z ('S b) = 'LT
  NatCmp ('S a) 'Z = 'GT
  NatCmp ('S a) ('S b) = NatCmp a b

-- | Add two natural numbers
type family NatAdd (a :: Nat) (b :: Nat) :: Nat where
  NatAdd 'Z b = b
  NatAdd ('S a) b = 'S (NatAdd a b)

-- | Subtract natural number b from a
type family NatSub (a :: Nat) (b :: Nat) :: Nat where
  NatSub a 'Z = a
  NatSub ('S a) ('S b) = NatSub a b
  NatSub 'Z b = 'Z  -- Underflow case returns zero

-- | Ensure integers are in canonical form (no Pos Z or Neg Z)
type family Normalize (n :: TInt) :: TInt where
  Normalize ('Pos 'Z) = 'Zero
  Normalize ('Neg 'Z) = 'Zero
  Normalize n = n

-- | Negate an integer
type family TNegate (a :: TInt) :: TInt where
  TNegate 'Zero = 'Zero
  TNegate ('Pos n) = 'Neg n
  TNegate ('Neg n) = 'Pos n

-- | Get the absolute value of an integer
type family TAbs (a :: TInt) :: Nat where
  TAbs 'Zero = 'Z
  TAbs ('Pos n) = n
  TAbs ('Neg n) = n

-- | Helper for type-level branching based on an ordering
type family Case (o :: Ordering) (gt :: a) (eq :: a) (lt :: a) :: a where
  Case 'GT gt eq lt = gt
  Case 'EQ gt eq lt = eq
  Case 'LT gt eq lt = lt

-- | Add two integers
type family TAdd (a :: TInt) (b :: TInt) :: TInt where
  -- Special cases with zero
  TAdd 'Zero b = b
  TAdd a 'Zero = a
  
  -- Same sign cases
  TAdd ('Pos a) ('Pos b) = 'Pos (NatAdd a b)
  TAdd ('Neg a) ('Neg b) = 'Neg (NatAdd a b)
  
  -- Mixed sign cases
  TAdd ('Pos a) ('Neg b) = 
    Normalize (
      Case (NatCmp a b) 
        ('Pos (NatSub a b))  -- a > b
        'Zero                -- a = b
        ('Neg (NatSub b a))  -- a < b
    )
    
  TAdd ('Neg a) ('Pos b) = 
    Normalize (
      Case (NatCmp a b) 
        ('Neg (NatSub a b))  -- a > b
        'Zero                -- a = b
        ('Pos (NatSub b a))  -- a < b
    )

-- | Subtract integer b from a
type family TSub (a :: TInt) (b :: TInt) :: TInt where
  TSub a b = TAdd a (TNegate b)

-- | Common integer constants for convenience
type T0 = 'Zero
type T1 = 'Pos ('S 'Z)
type T2 = 'Pos ('S ('S 'Z))
type T3 = 'Pos ('S ('S ('S 'Z)))
type TNeg1 = 'Neg ('S 'Z)
type TNeg2 = 'Neg ('S ('S 'Z))

--------------------------------------------------------------------------------
-- Static (Type-Level) Fundamental Group
--------------------------------------------------------------------------------

-- | 'StaticLoop' represents a loop in S¹ with a type-level winding number.
-- This models the fundamental group π₁(S¹) at the type level.
data StaticLoop (n :: TInt) where
  MkStaticLoop :: StaticLoop n

-- | The identity loop (winding number 0)
identityStatic :: StaticLoop T0
identityStatic = MkStaticLoop

-- | Compose two loops by adding their winding numbers
composeStatic :: StaticLoop n -> StaticLoop m -> StaticLoop (TAdd n m)
composeStatic MkStaticLoop MkStaticLoop = MkStaticLoop

-- | Invert a loop by negating its winding number
inverseStatic :: StaticLoop n -> StaticLoop (TNegate n)
inverseStatic MkStaticLoop = MkStaticLoop

-- | The fundamental loop (one counterclockwise traversal)
fundamentalStatic :: StaticLoop T1
fundamentalStatic = MkStaticLoop

-- | Proof that a loop composed with its inverse equals the identity
proofStatic :: StaticLoop T1 -> (StaticLoop (TAdd T1 TNeg1) :~: StaticLoop T0)
proofStatic _ = Refl

-- | Advanced proof: composing loop -2 with loop 3 gives loop 1
advancedProof :: StaticLoop TNeg2 -> StaticLoop T3 -> (StaticLoop (TAdd TNeg2 T3) :~: StaticLoop T1)
advancedProof _ _ = Refl

-- | Term-level representation of type-level natural numbers
data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | Term-level representation of type-level integers
data STInt (n :: TInt) where
  SPos :: SNat n -> STInt ('Pos n)
  SNeg :: SNat n -> STInt ('Neg n)
  SZero :: STInt 'Zero

-- | Convert type-level integer to runtime Int
tIntToInt :: STInt n -> Int
tIntToInt SZero = 0
tIntToInt (SPos n) = natToInt n
tIntToInt (SNeg n) = - natToInt n

-- | Convert type-level natural number to runtime Int
natToInt :: SNat n -> Int
natToInt SZ = 0
natToInt (SS n) = 1 + natToInt n

--------------------------------------------------------------------------------
-- Dynamic (Runtime) Fundamental Group
--------------------------------------------------------------------------------

-- | 'DynamicLoop' represents a loop by its winding number at runtime
newtype DynamicLoop = DynamicLoop { getDynWinding :: Int }
  deriving (Eq, Show)

-- | Compose two loops by adding their winding numbers
composeDynamic :: DynamicLoop -> DynamicLoop -> DynamicLoop
composeDynamic (DynamicLoop n) (DynamicLoop m) = DynamicLoop (n + m)

-- | Invert a loop by negating its winding number
inverseDynamic :: DynamicLoop -> DynamicLoop
inverseDynamic (DynamicLoop n) = DynamicLoop (-n)

-- | The identity loop (winding number 0)
identityDynamic :: DynamicLoop
identityDynamic = DynamicLoop 0

-- | Create a loop with the specified winding number
powerDynamic :: Int -> DynamicLoop
powerDynamic n = DynamicLoop n

--------------------------------------------------------------------------------
-- Interactive Game
--------------------------------------------------------------------------------

-- | Display available commands
helpMessage :: IO ()
helpMessage = do
  putStrLn "Top-level commands:"
  putStrLn "  dynamic   -- Run the dynamic simulation (using runtime integers)"
  putStrLn "  static    -- Show the static type-level proof"
  putStrLn "  help      -- Display this help message"
  putStrLn "  quit      -- Exit the game"

-- | Start the dynamic simulation
dynamicGame :: IO ()
dynamicGame = do
  putStrLn "Dynamic Version: Loops are represented as integers (winding numbers)."
  putStrLn "Available commands: compose n m, inverse n, identity, power n, back"
  dynamicLoop

-- | Main interactive loop for dynamic simulation
dynamicLoop :: IO ()
dynamicLoop = forever $ do
  putStr "> "
  input <- getLine
  case words input of
    ["compose", a, b] ->
      case (reads a, reads b) of
        ([(n, "")], [(m, "")]) ->
          putStrLn $ "Result: " ++ show (composeDynamic (DynamicLoop n) (DynamicLoop m))
        _ -> putStrLn "Error: please enter valid integers for compose."
    ["inverse", a] ->
      case reads a of
        [(n, "")] -> putStrLn $ "Result: " ++ show (inverseDynamic (DynamicLoop n))
        _ -> putStrLn "Error: please enter a valid integer for inverse."
    ["identity"] ->
      putStrLn $ "Identity loop: " ++ show identityDynamic
    ["power", a] ->
      case reads a of
        [(n, "")] -> putStrLn $ "Result: " ++ show (powerDynamic n)
        _ -> putStrLn "Error: please enter a valid integer for power."
    ["back"] -> do
      putStrLn "Returning to top-level menu..."
      return ()
    _ -> putStrLn "Unknown command. Available: compose, inverse, identity, power, back."

-- | Show the static type-level demonstration
staticDemo :: IO ()
staticDemo = do
  putStrLn "Static Version: Type-level representation of loops."
  putStrLn "We have fundamentalStatic :: StaticLoop T1 and its inverse."
  putStrLn "Compile-time proof that fundamentalStatic composed with its inverse equals identity:"
  case proofStatic fundamentalStatic of
    Refl -> do
      putStrLn "Proof complete: T1 + TNeg1 equals T0 at the type level."
      putStrLn ""
      putStrLn "This implementation uses a general-purpose type-level integer system"
      putStrLn "that can handle arbitrary integer arithmetic at the type level."
      putStrLn ""
      putStrLn "For example, we can also prove that -2 + 3 = 1:"
      case advancedProof MkStaticLoop MkStaticLoop of
        Refl -> putStrLn "Proof verified: TNeg2 + T3 equals T1 at the type level."

--------------------------------------------------------------------------------
-- Main Program Entry Point
--------------------------------------------------------------------------------

-- | Main program
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to TypeLoopS1:"
  putStrLn "Exploring the Fundamental Group of the Circle (π₁(S¹)) via Type-Level Haskell"
  putStrLn ""
  putStrLn "This demonstrates how π₁(S¹) is isomorphic to ℤ (the integers),"
  putStrLn "using both a static (type-level) and dynamic (runtime) representation."
  putStrLn ""
  putStrLn "Choose 'static' to view the type-level demonstration (using DataKinds and GADTs),"
  putStrLn "or 'dynamic' for a runtime interactive simulation."
  helpMessage
  mainLoop

-- | Main interactive loop
mainLoop :: IO ()
mainLoop = forever $ do
  putStr "> "
  cmd <- getLine
  case cmd of
    "dynamic" -> dynamicGame
    "static"  -> staticDemo
    "help"    -> helpMessage
    "quit"    -> do
                  putStrLn "Goodbye!"
                  exitSuccess
    _         -> putStrLn "Unknown command. Type 'help' for commands."
