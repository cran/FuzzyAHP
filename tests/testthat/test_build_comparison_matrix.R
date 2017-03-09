require("testthat")

pmatrix1 = matrix(c(1,3,5,1/3,1,2,1/5,1/2,1), nrow = 3, byrow = TRUE)
pmatrix1 = pairwiseComparisonMatrix(pmatrix1)
pmatrix2 = matrix(c(1,2,7,1/2,1,4,1/7,1/4,1), nrow = 3, byrow = TRUE)
pmatrix2 = pairwiseComparisonMatrix(pmatrix2)
pmatrix3 = matrix(c(1,1,4,1/1,1,2,1/4,1/2,1), nrow = 3, byrow = TRUE)
pmatrix3 = pairwiseComparisonMatrix(pmatrix3)

test_that("Tests of buildPairwiseComparisonMatrix function", {

  unified_matrix = buildPairwiseComparisonMatrix(list(pmatrix1, pmatrix2, pmatrix3), agg = "geometric")

  expect_is(unified_matrix, "PairwiseComparisonMatrix")
})

test_that("Tests of buildPairwiseComparisonMatrixbuildFuzzyPairwiseComparisonMatrix function from PairwiseComparisonMatrices", {

  unified_fuzzy_matrix = buildFuzzyPairwiseComparisonMatrix(list(pmatrix1, pmatrix2, pmatrix3))

  expect_is(unified_fuzzy_matrix, "FuzzyPairwiseComparisonMatrix")
})

fpmatrix1 = fuzzyPairwiseComparisonMatrix(pmatrix1, getFuzzyScale("full"))
fpmatrix2 = fuzzyPairwiseComparisonMatrix(pmatrix2, getFuzzyScale("full"))
fpmatrix3 = fuzzyPairwiseComparisonMatrix(pmatrix3, getFuzzyScale("full"))

test_that("Tests of buildPairwiseComparisonMatrixbuildFuzzyPairwiseComparisonMatrix function from FuzzyPairwiseComparisonMatrices", {

  unified_fuzzy_matrix = buildFuzzyPairwiseComparisonMatrix(list(fpmatrix1, fpmatrix2, fpmatrix3))

  expect_is(unified_fuzzy_matrix, "FuzzyPairwiseComparisonMatrix")
})
