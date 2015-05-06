{-# LANGUAGE CPP, BangPatterns #-}
-- | This module provides a data type for individuals in the Prisoner's Dilemma and an instance of 'Individual' from "CanonicalGA" for this
module IteratedPD (
                  -- * Types
                    Allele(..)
                  , Chromosome
                  , History
                  , PDIndividual
                  -- * Helper functions
                  , alleleToBinary
                  , historyToBinary
                  , score
                  , makeMove
                  , playAgainst
                  , playAgainstNTimes
                  , getHistory
                  , averageScoreOverN
                  , flipAllele
                  , mkAllele
                  , maybeFlip
                  , 
                  )where

import CanonicalGA
--import qualified Data.Vector as V
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Arrow ((***))
import Data.Tuple (swap)
import System.IO.Unsafe
import Test.QuickCheck
import Debug.Trace


--
--   Would ideally hold precisely 70 Alleles
type Chromosome = [Allele]

-- | History is a list of moves made by two players, \"me\" first, \"challenger\" second
--
--   Expected to be of length 3, could generalise I suppose
--   
--   Used to be a list of pairs but this just complicated things.
type History = [Allele]

-- | Allele is either defect or cooperate
--
--   Encoded as 
--
--   > CCCCCC -> 0
--
--   > CCCCCD -> 1
--
--   > CCCCDC -> 2
--
--    i.e. C = 0, D = 1, binary encoding,
--
--    Also with leftmost as most recent
data Allele = D -- ^ Defect
            | C -- ^ Cooperate
        deriving (Eq, Ord, Show, Read)

-- | Represents an individual in the Iterated Prisoner's Dilemma with a list of alleles as the chromosome
data PDIndividual = PDIndividual {getChromosome :: !Chromosome, getHistoryLength :: !Int}
        deriving (Eq, Show, Read)

-- | Turns an allele into its corresponding binary encoding
alleleToBinary :: Allele -> Int
alleleToBinary !C = 0
alleleToBinary !D = 1

-- | Takes a list of pairs (homogeneous type pairs) and returns a list
pairsToList :: [(a,a)] -> [a]
pairsToList ((a,a'):as) = a:a':(pairsToList as)
pairsToList [] = []

-- | Generates a number from the binary encoding of a history
historyToBinary :: History -> Int
historyToBinary history = f 1 $ reverse ns
    where ns = map alleleToBinary history
          f _ [] = 0
          f i (n':n's) = i*n'+f (2*i) n's

-- | Takes two moves and generates a score for the /first/ player from them
score :: Allele -> Allele -> Int
score !C !C = 3
score !C !D = 0
score !D !C = 5
score !D !D = 1

-- | Takes an individual and a history and returns the move that individual would make given that history
makeMove :: PDIndividual -> History -> Allele
makeMove (PDIndividual !chromosome !hlength) !history = chromosome !! historyToBinary history

-- | Cooperates on first move then does whatever the opponent did in the previous game next
titForTat :: PDIndividual
titForTat = PDIndividual (read "CDCDCC") 1

-- | Sets one Individual against another once, with a history for each, returning the moves that would be made
playAgainst :: PDIndividual -> PDIndividual -> History -> History -> (Allele, Allele)
playAgainst !me !challenger h h' = (makeMove me h, makeMove challenger h')

-- | Sets one individual against another n times and returns the sum of the scores of the first individual
playAgainstNTimes :: Int -> PDIndividual -> PDIndividual -> History -> History -> Int
playAgainstNTimes 0 _ _ _ _ = 0
playAgainstNTimes n !me@(PDIndividual _ l) !challenger@(PDIndividual _ l') h h' = thisScore+restOfScores
            where
                (!a,!b) = playAgainst me challenger h h'
                thisScore = a `score` b
                newH  = a:b:take (2*(l-1)) h
                newH' = b:a:take (2*(l'-1)) h'
                !restOfScores = playAgainstNTimes (n-1) me challenger newH newH'

-- | Extracts the imagined pre-history from a given individual
getHistory :: PDIndividual -> History
getHistory (PDIndividual !chromosome !n) = drop (4^n) chromosome

-- | Plays m against c n times and returns m's average score
averageScoreOverN :: Int -> PDIndividual -> PDIndividual -> Double
averageScoreOverN n !m !c = fromIntegral (playAgainstNTimes n m c hM hC)/fromIntegral n
    where
        !hM = getHistory m
        !hC = getHistory c

-- | Flips an allele
flipAllele :: Allele -> Allele
flipAllele !C = D
flipAllele !D = C

-- | Generates a random allele within the RandomST monad
mkAllele :: RandomST Allele
mkAllele = do
    y <- runProb (0.5 :: Double)
    return $ if y then C else D

-- | Given a history length returns the chromosome length
getChromosomeLength :: Int -> Int
getChromosomeLength n = 4^n+2*n

-- | Flips an allele with probability p
maybeFlip :: (Real p, Random p) => p -> Allele -> RandomST Allele
maybeFlip !p !a = do
    !y <- runProb p
    if y
        then return $ flipAllele a
        else return a

instance Individual PDIndividual where
    combine (PDIndividual p1 n) (PDIndividual p2 n') = do
        !l <- makeRandR (0,getChromosomeLength $ min n n') -- Can only really split at some point in the smaller one
        let (!p11, !p12) = splitAt l p1
        let (!p21, !p22) = splitAt l p2
        let c1 = p11 ++ p22
        let c2 = p21 ++ p12
        return (PDIndividual c1 n', PDIndividual c2 n)
    
    mutate p (PDIndividual i n)  = do
        i' <- mapM (maybeFlip p) i
        return $ PDIndividual i' n
    fitness individual population = {-trace (show f)-} f -- Debugging, lol
        where f = sum (map (averageScoreOverN 50 individual) population)/fromIntegral (length population)
    generate = do
        historyLength <- makeRandR (3,4) :: RandomST Int
        let n = getChromosomeLength historyLength
        chromosome <- replicateM n mkAllele
        return $ PDIndividual chromosome historyLength
