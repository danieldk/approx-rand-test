# approx-rand-test

## Introduction

This Haskell package provides a module and utilities to perform paired
and unpaired approximate randomization tests.

Approximate randomization tests rely on a simple premise: given a test
statistic, if the null-hypothesis (the samples do not differ) is true,
we can randomly swap values between samples without an (extreme) impact
on the test statistic. Otherwise, the null-hypothesis must be rejected.

The test works by generating a given number of sample shuffles and computing
the test statistic for each shuffle. If *r* is the number of shuffled
samples where the test statistic is at least as high as the test statistic
applied on the original samples; and *N* the number of shuffles, then
the null-hypothesis is rejected iff *(r + 1):(N + 1) < p-value* (for
one-sided tests).

The included command-line utilities can perform randomization tests and
draw histograms of the test statistic for the randomized samples.

## Installation

Install the necessary dependencies, and run:

    cabal install

The command-line utilities also have support for making histograms. By
default, this uses the *diagrams* backend, which can produce EPS and SVG
files. The Cairo backend can produce more files (such as PNG and PDF), but
may be more difficult to install on some systems. To compile the package
with Cairo support, use:

    cabal install -fwithCairo

## Usage

Documentation for the Haskell module can be read through Haddock. The
package also provides two utilities: <tt>approx_rand_test</tt> and
<tt>approx_rand_test_paired</tt>. Both utilities provide nearly the same
options. The first is for unpaired tests, the latter for paired tests.

The format for samples is simple: use one value per line. Three samples
are provided in the <tt>examples</tt> directory. The three samples
contain evaluation scores of fluency ranking components:

  * <tt>ngram.scores</tt>: Scores of an n-gram language model
  * <tt>fluency.scores</tt>: Scores of a feature-based fluency ranking model
  * <tt>reversible.scores</tt>: Scores of a reversible model (a model
    that can be used in parsing and generation)

We can now use pair-wise test utility to see that the evaluation scores
of the n-gram language model and the feature-based fluency model differ
significantly:

    % approx_rand_test_paired -i 10000 -p 0.05 examples/ngram.scores examples/fluency.scores
    Iterations: 10000
    Sample size: 1621
    Test statistic: -0.030646088066079557
    Test type: TwoTailed
    Test significance: 0.05
    Tail significance: 0.025
    Significant: 0.00009999000099990002

Here we generate 10,000 shuffled samples, with a significance level of
*p = 0.05*. Likewise, we can compare the scores of feature-based fluency
model and the reversible model:

    % approx_rand_test_paired -i 10000 -p 0.05 examples/fluency.scores examples/reversible.scores 
    Iterations: 10000
    Sample size: 1621
    Test statistic: 0.0032431465344367667
    Test type: TwoTailed
    Test significance: 0.05
    Tail significance: 0.025
    Not significant: 0.0273972602739726

In this case, the samples do not differ significantly.
