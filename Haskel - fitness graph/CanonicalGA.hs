{-# LANGUAGE BangPatterns #-}
-- | This module provides an implementation of a canonical genetic algorithm as described in \"An Introduction to Genetic Algorithms\" by Melanie Mitchell
module CanonicalGA ( 
                   -- * Typeclass of Individuals
                     Individual(..)
                   , maybeCombine
                   , generateN
                   -- * Running an Instance
                   , evolve
                   , step
                   , stepN
                   -- * Helpers
                   , RandomST
                   , runProb
                   , makeRandR
                   ) where

import System.Random
import Control.Monad.State
import Data.List
import Debug.Trace

-- | State monad applied for random generation
--
--   Plus logging of fitnesses
type RandomST a = State (StdGen, [[Double]]) a

-- | Take a probability and probabilistically return True or False
runProb :: (Real p, Random p) => p -> RandomST Bool
runProb p = do
    p' <- makeRandR (0,1)
    return (p'<p)

-- | Generates a random thing inside a range within the RandomST monad
makeRandR :: (Random p) => (p,p) -> RandomST p
makeRandR a = do
    (gen, fitnesses) <- get
    let (p, newgen) = randomR a gen
    put (newgen, fitnesses)
    return p

-- | The 'Individual' class defines operations over individuals for the purposes of
--   genetic algorithms, classical examples of an individual would be a bit string
--   or chromosome (chromosome is another candidate name for this class)
class Individual individual where
    -- | Generates a random individual
    generate :: RandomST individual

    -- | Takes two individuals as parents and produces two offspring.
    --
    --   Expected implementation will use crossover of some kind
    combine :: individual -> individual -> RandomST (individual, individual) -- Should be constant time

    -- | Takes an individual and returns a mutated individual with parts of the chromosome changed at random
    mutate  :: (Real p, Random p) => p -> individual -> RandomST individual -- Should be constant time

    -- | Takes an individual and returns a double that can be used to compare individuals.
    fitness :: individual -> [individual] -> Double -- Hopefully linear time

    -- | Takes a population and produces a list of parents from that population to breed.
    --
    --   Default implementation uses Stochastic Universal Sampling
    select :: (Show individual) => [individual] -> RandomST [individual] -- O(n^2)
    select !pop = do
        let !fitnesses = map (`fitness` pop) pop -- O(n^2)
        (gen, fitnesseses) <- get
        put (gen, fitnesses:fitnesseses)
        let (!elite', !rest') = splitAt nElite $ sortBy (\(a,_) (b,_) -> compare b a) $ zip fitnesses pop -- O(n lg(n))
        let (!elite, !rest, restFitnesses) = (map snd elite', map snd rest', map fst rest') -- O(n)
        let !fitnessDist = cumulativeSum restFitnesses -- O(n)
        let !fitPopulation = zip fitnessDist rest
        !selectedRest <- replicateM (length rest) (select1 fitPopulation) -- O(n^2) -- Possible place to optimise
        return $ elite++selectedRest
            where
                nElite = length pop `div` 15
                select1 !fitPopulation = do
                    let top = maximum $ map fst fitPopulation -- O(n)
                    n <- makeRandR (0, top)
                    let (_,p) = head $ filter (\(partition,_) -> n<=partition) fitPopulation -- O(n)
                    return p

-- | Much like a cumulative frequency distribution, cumulative sum of list
cumulativeSum :: [Double] -> [Double]
cumulativeSum (d:ds) = d:(f d ds)
    where
        f _ [] = []
        f n (e:es) = e':(f e' es)
            where e' = n+e

-- | Either combine or return parents with probability p
maybeCombine :: (Individual i, Real p, Random p) => p -> i -> i -> RandomST (i,i)
maybeCombine p i1 i2 = do
    y <- runProb p
    if y
        then combine i1 i2
        else return (i1, i2)

-- | Make a list of n individuals at random
generateN :: (Individual individual) => Int -> RandomST [individual] -- O(n)
generateN n = replicateM n generate

-- | Iterates a population once, returning the new population.
--
--   Probably shouldn't be exported.
step :: (Random probability, Real probability, Individual individual, Show individual, Eq individual) -- O(n^2)
     => probability            -- ^ Chance of mutation
     -> probability            -- ^ Chance of crossover
     -> [individual]           -- ^ Old population
     -> RandomST [individual]  -- ^ New population (+StdGen in state)
step pM pC !population = do
    !someParents <- select population -- O(n^2)
    !newGen <- makeNewGen someParents pC -- O(n)
    mapM (mutate pM) newGen -- O(n)

makeNewGen :: (Random probability, Real probability, Individual parent, Eq parent) -- O(n)
           => [parent] -- ^ parent set
           -> probability -- ^ Chance of crossover
           -> RandomST [parent] -- ^ New generation
makeNewGen (p1:p2:parents) pC = do
    (p1', p2') <- maybeCombine pC p1 p2
    newGen' <- makeNewGen parents pC
    return (p1':p2':newGen')
makeNewGen _ _ = return [] -- I ignore the last one if given an odd number.

-- | Iterate step n times
stepN :: (Random probability, Real probability, Individual individual, Show individual, Eq individual)
      => probability            -- ^ Chance of mutation
      -> probability            -- ^ Chance of crossover
      -> [individual]           -- ^ Old population
      -> Int                    -- ^ Number of generations
      -> RandomST [individual]  -- ^ New population (+StdGen in state)
stepN _ _ !pop 0 = return pop
stepN pM pC !pop n = do
    oldPop <- stepN pM pC pop (n-1) -- O(n^2)
    step pM pC oldPop -- O(n^2)
    -- Note different n, so total is O(m*n^2)

-- | The expected entry point for execution in this module, carries out an implementation of a GA
evolve :: (Random probability, Real probability, Individual individual, Show individual, Eq individual)
       => probability       -- ^ Chance of mutation, keep low
       -> probability       -- ^ Chance of crossover, relatively high
       -> Int               -- ^ Number of generations to consider
       -> Int               -- ^ Number of individuals in a generation
       -> RandomST [individual] -- ^ Final population
evolve pM pC generations populationSize = do
    !initialPopulation <- generateN populationSize -- O(n)
    stepN pM pC initialPopulation generations -- O(m*n^2)
