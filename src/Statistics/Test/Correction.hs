-- |
-- Copyright  : (c) 2012 Daniël de Kok
-- License    : BSD3
--
-- Maintainer : Daniël de Kok <me@danieldk.eu>
-- Stability  : experimental
--
-- The more tests that are applied on a dataset, the more likely it becomes
-- that a Type I error occurs (rejecting the null hypothesis when it is
-- true). This module provides significance level corrections for repeated
-- tests.
--
-- For more information, see:
--
--  * /Bonferroni and Šidák corrections for multiple comparisons, H. Abdi, 2007, in: N.J. Salkind (ed.), Encyclopedia of Measurement and Statistics, Thousand Oaks, CA: Sage/

module Statistics.Test.Corrections (
  sidak,
  bonferroni
) where

-- |
-- Bonferroni correction: α* = α / n, where α is the intended
-- significance level over all tests, n the number of tests, and α* the
-- corrected significance level.
bonferroni :: (Fractional f, Integral i) =>
     f -- ^ Significance level
  -> i -- ^ Number of comparisons
  -> f -- ^ The corrected significance level
bonferroni level n =
  level / fromIntegral n

-- |
-- Sidak correction: α* = 1 - (1 - α)^(1 / n), where α is the intended
-- significance level over all tests, n the number of tests, and α* the
-- corrected significance level.
sidak :: (Floating f, Integral i) =>
     f -- ^ Significance level
  -> i -- ^ Number of comparisons
  -> f -- ^ The corrected significance level
sidak level n =
  1 - (1 - level) ** (1 / (fromIntegral n))
