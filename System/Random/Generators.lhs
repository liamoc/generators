\documentclass{article}
%include lhs2TeX.fmt
\usepackage[left=3cm,top=2.5cm,right=3cm,bottom=3cm,nohead]{geometry}
\begin{document}
\title{System.Random.Generators: A Monad for Convenient Randomness in Haskell}
\author{Liam O'Connor-Davis}
\maketitle

\section{Introduction}

Randomness in Haskell has always been an \emph{ugly} affair, requiring acquisition of a @RandomGen@ value and then a threading of each successive generator throughout the code. The @System.Random@ library, while flexible, is rather unwieldly for common applications.

In particular, making a series of random decisions (such as navigating a random path through a space, or generating a sequence of text), or negotiating situations where the set of options for a random generator are dependent on the previously determined random value, are rather unimpressive using @System.Random@, and fail to demonstrate Haskell's excellent expressive power.

Random generators in @System.Random@ are tied to data types, instances of the @Random@ typeclass, and more complicated generators must be painstakingly specified.

In this Literate Haskell Module I present a @Generator@ data structure, a Monad, that when provided with a seed value or a @RandomGen@ value, will provide an infinite list of random choices based on some composition of other @Generator@s. 

> module System.Random.Generators 
>     ( Generator
>     , runGenerator
>     , runGeneratorWith
>     , constantG
>     , randomG
>     , rangeG
>     , listG
>     ) where

> import Control.Monad.Reader
> import System.Random
> import Data.List(transpose)
> import Control.Applicative((<$>))

\section {Implementation}

First we define a @Generator@ type, which is a simple transformer around a @Reader@ Monad containing a @StdGen@ and returning a list of random values.

The list of values produced by the encapsulated @Reader@ is an \emph{infinite} list of random choices. 

> newtype Generator v  = Generator { extract :: Reader StdGen [v] } 

To instantiate @Functor@, we map the function over each value in the infinite list.

> instance Functor Generator where
>    fmap f (Generator z) = Generator $ fmap (map f) z
 
As we have provided a @Functor@ instance, we define @>>=@ in terms of @joinGen@. @return@ produces a constant @Generator@ (that is, a @Generator@ that always produces the same value).

> instance Monad Generator where   
>    xs >>= f = joinGen (fmap f xs) 
>    return v = Generator $ return $ repeat v

The @joinGen@ function produces a random list of seed values from the generator within the @Reader@, runs each @Generator@ produced by the outer @Generator@ with its own unique seed value, and then merges the results. Unlike the (similar) List monad, the @Generator@ monad @transpose@s the results before concatenating, as each result set is infinite - simply concatenating would mean that any result set after the first would become inaccessible (as the first result set is infinitely long).

> joinGen :: Generator (Generator a) -> Generator a
> joinGen (Generator g) = Generator $ do 
>      v <- ask 
>      z <- g
>      return $ concat $ transpose $ 
>           map (\(v,s) -> flip runReader (mkStdGen s) $ extract $ v) 
>             (zip z (randoms v :: [Int]))

\section {Interface}

\subsection {Inbuilt Generators}

@constantGen@ produces a @Generator@ that always generates the value specified.

> constantG :: v -> Generator v
> constantG = return

@randomG@ produces a @Generator@ that produces random values for some instance of the @Random@ typeclass. For example, @randomG :: Generator Int@ is a random @Int@ generator.

> randomG :: Random r => Generator r
> randomG = Generator $ randoms <$> ask 

@rangeG@ is the same as @randomG@ except that it also accepts an allowable range of values, such that @rangeG ('a','z')@ is the set of lowercase letters.

> rangeG :: Random r => (r,r) -> Generator r
> rangeG rs = Generator $ randomRs rs <$> ask 

@listG@ is a generator that selects a random value from the list. For example, @listG "aeiou"@ is a generator for vowels.

> listG :: [a] -> Generator a
> listG ps = (ps !!) <$> rangeG (0,length ps - 1)

@weightedG@ is like @listG@ except that (positive) weightings (of arbitrary size) can be used to make specific elements more or less likely.

> weightedG :: [(Float,a)] -> Generator a
> weightedG els = Generator $ weightedG' <$> ask
>   where
>     weightedG' gen =let total = (sum (map fst els))
>                         (v,g) = randomR (0, total) gen
>                     in getElement v els:weightedG' g   
>     getElement :: Float -> [(Float,a)] -> a
>     getElement n ((w,e):es) | n - w <= 0 = e
>                             | otherwise  = getElement (n - w) es
>     getElement _ es = error "Should have found an element by now"
 
\subsection {Using Generators}

@runGenerator@ takes an integer seed and a @Generator@, and produces an infinite list of random values, depending on the @Generator@.

> runGenerator :: Int -> Generator v -> [v]
> runGenerator seed = runGeneratorWith (mkStdGen seed)
 
@runGeneratorWith@ takes a @StdGen@ value, which allows you to use the standard @System.Random@ generators to seed these @Generator@s.

> runGeneratorWith :: StdGen -> Generator v -> [v]
> runGeneratorWith std gen = runReader (extract gen) std

\section{Example: Pronouncable Words}

A @Generator@ for a random list of words of a specified length, that consist of a consonant, followed by a vowel, followed by a consonant, and so on.

> vowels :: Generator Char
> vowels = listG "aeiou"

> consonants :: Generator Char
> consonants = listG "qwrtypsdfghjklzxcvbnm"

> pronouncable :: Int -> Generator String
> pronouncable l = pronouncable' consonants vowels l
>   where pronouncable' _ _ 0 = return []
>         pronouncable' current other l = do
>           c <- current
>           rest <- pronouncable' other current (l-1)
>           return (c:rest)


\end{document}
