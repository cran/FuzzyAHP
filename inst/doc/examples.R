## ----setup, include=FALSE---------------------------------------------------------------------------------------------
options(width=120)
library(FuzzyAHP)

## ---- eval = FALSE----------------------------------------------------------------------------------------------------
#  matrixFile =  "comparison_matrix.csv"
#  comparisonMatrix = read.csv(matrixFile, sep = ";",
#                     stringsAsFactors = FALSE, header = TRUE, row.names = 1, strip.white = TRUE)
#  comparisonMatrix = as.matrix(comparisonMatrix)

## ---------------------------------------------------------------------------------------------------------------------
comparisonMatrixValues = c(1,9,5,
                           NA,1,1/3,
                           NA,NA,1)
comparisonMatrix = matrix(comparisonMatrixValues, nrow = 3, ncol = 3, byrow = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
comparisonMatrixValues = c("1","9","5",
                           "","1","1/3",
                           "","","1")
comparisonMatrix = matrix(comparisonMatrixValues, nrow = 3, ncol = 3, byrow = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
comparisonMatrix = pairwiseComparisonMatrix(comparisonMatrix)
show(comparisonMatrix)

## ---------------------------------------------------------------------------------------------------------------------
textMatrix = textRepresentation(comparisonMatrix, whole = FALSE)
print(textMatrix)

## ---------------------------------------------------------------------------------------------------------------------
print(comparisonMatrix)

## ---------------------------------------------------------------------------------------------------------------------
consistencyRatio(comparisonMatrix)
CR = consistencyRatio(comparisonMatrix, print.report = FALSE)
print(CR)

## ---------------------------------------------------------------------------------------------------------------------
weakConsistency = weakConsistency(comparisonMatrix)

## ---------------------------------------------------------------------------------------------------------------------
strictConsistency = strictConsistency(comparisonMatrix)

## ---------------------------------------------------------------------------------------------------------------------
weights = calculateWeights(comparisonMatrix)
print(weights)

## ---------------------------------------------------------------------------------------------------------------------
values = c(4,5,3,
1,3,9,
8,6,4,
3,2,7,
6,7,5,
4,5,3,
NA,9,9,
NA,NA,NA)
values = matrix(values, nrow = length(values)/length(weights@weights), ncol = length(weights@weights), byrow = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
result = calculateAHP(weights, values)
print(result)

## ---------------------------------------------------------------------------------------------------------------------
rank = compareResults(result)
print(rank)

## ---------------------------------------------------------------------------------------------------------------------
result = cbind(values, result, rank)
colnames(result) = c("crit1", "crit2", "crit3", "result_value", "ranking")
print(result)

## ---------------------------------------------------------------------------------------------------------------------
comparisonMatrixValues = c("1","9","5",
                       "1/9","1","1/3",
                       "1/5","3","1")
comparisonMatrix = matrix(comparisonMatrixValues, nrow = 3, ncol = 3, byrow = TRUE)
comparisonMatrix = pairwiseComparisonMatrix(comparisonMatrix)

## ---------------------------------------------------------------------------------------------------------------------
fuzzyComparisonMatrix = fuzzyPairwiseComparisonMatrix(comparisonMatrix)
print(fuzzyComparisonMatrix)

## ---------------------------------------------------------------------------------------------------------------------
result = calculateAHP(fuzzyComparisonMatrix, values)

## ---------------------------------------------------------------------------------------------------------------------
fuzzyNumer = getFuzzyNumber(result, as.integer(2))
print(fuzzyNumer)

## ---------------------------------------------------------------------------------------------------------------------
defuzzified = defuzziffy(result, "Yager")
print(defuzzified)
rank = (nrow(values)+1) - sum(is.na(defuzzified)) - rank(defuzzified, na.last = "keep", ties.method= "max")
print(rank)

## ---------------------------------------------------------------------------------------------------------------------
ranked = compareFuzzyNumbers(result, "Chen")
print(ranked)

## ---- results = "hide"------------------------------------------------------------------------------------------------
ranked = compareFuzzyNumbers(result, "possibilityTheory")
# ranked = compareFuzzyNumbers(result, "possibilityTheory", progressBar = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
print(ranked)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  calculateWeights_old_methods(fuzzyComparisonMatrix, type)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  calculate_weighting_vector(fuzzyWeights).

## ---------------------------------------------------------------------------------------------------------------------
pmatrix1 = matrix(c(1,3,5,1/3,1,2,1/5,1/2,1), nrow = 3, byrow = TRUE)
pmatrix1 = pairwiseComparisonMatrix(pmatrix1)
pmatrix2 = matrix(c(1,2,7,1/2,1,4,1/7,1/4,1), nrow = 3, byrow = TRUE)
pmatrix2 = pairwiseComparisonMatrix(pmatrix2)
pmatrix3 = matrix(c(1,1,4,1/1,1,2,1/4,1/2,1), nrow = 3, byrow = TRUE)
pmatrix3 = pairwiseComparisonMatrix(pmatrix3)

## ---------------------------------------------------------------------------------------------------------------------
unified_matrix = buildPairwiseComparisonMatrix(list(pmatrix1, pmatrix2, pmatrix3), agg = "geometric")
print(unified_matrix)
print(unified_matrix@values)

## ---------------------------------------------------------------------------------------------------------------------
unified_fuzzy_matrix = buildFuzzyPairwiseComparisonMatrix(list(pmatrix1, pmatrix2, pmatrix3))
print(unified_fuzzy_matrix)

## ---------------------------------------------------------------------------------------------------------------------
fpmatrix1 = fuzzyPairwiseComparisonMatrix(pmatrix1, getFuzzyScale("full"))
fpmatrix2 = fuzzyPairwiseComparisonMatrix(pmatrix2, getFuzzyScale("full"))
fpmatrix3 = fuzzyPairwiseComparisonMatrix(pmatrix3, getFuzzyScale("full"))

unified_fuzzy_matrix = buildFuzzyPairwiseComparisonMatrix(list(fpmatrix1, fpmatrix2, fpmatrix3))
print(unified_fuzzy_matrix)

