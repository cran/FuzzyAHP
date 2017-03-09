#' Function that builds Fuzzy Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices or
#' Fuzzy Pairwise Comparison Matrices
#'
#' @description
#' This functions builds Fuzzy Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices
#' or Fuzzy Pairwise Comparison Matrices. the resulting Fuzzy Pairwise Comparison Matrix is calculated
#' as minimum, geometric mean and maximum of each cell of all Pairwise Comparison Matrices in
#' \code{listOfMatrices}. In case of Fuzzy Pairwise Comparison Matrices the resulting Fuzzy Pairwise
#' Comparison Matrix is calculated as geometric mean of minimum, modal and maximum values.
#'
#' @param listOfMatrices An object of \code{\linkS4class{list}}.
#'
#' @return An object of class \code{\linkS4class{FuzzyPairwiseComparisonMatrix}}
#'
#' @export
#' @rdname buildFuzzyPairwiseComparisonMatrix-methods
#' @name buildFuzzyPairwiseComparisonMatrix
setGeneric("buildFuzzyPairwiseComparisonMatrix",
           function(listOfMatrices) standardGeneric("buildFuzzyPairwiseComparisonMatrix"))

#' @rdname buildFuzzyPairwiseComparisonMatrix-methods
#' @aliases buildFuzzyPairwiseComparisonMatrix,list-method
setMethod(
  f="buildFuzzyPairwiseComparisonMatrix",
  signature(listOfMatrices = "list"),
  definition=function(listOfMatrices)
  {
    number = length(listOfMatrices)

    types = .typeOfObjects(listOfMatrices)

    if (length(unique(types)) != 1){
      stop(paste0("All the elements in the list must be of the same class. However types ",
                  paste(types, collapse=", ", " were found.")))
    }

    if(!(unique(types)[1] == "PairwiseComparisonMatrix" || unique(types)[1] == "FuzzyPairwiseComparisonMatrix")){
      stop(paste0("Allowed classes in the list are PairwiseComparisonMatrix or FuzzyPairwiseComparisonMatrix, but ",
                  unique(types)[1], " was found."))
    }

    .checkDimensions(listOfMatrices, unique(types)[1])

    if(unique(types)[1] == "PairwiseComparisonMatrix"){
      size = nrow(listOfMatrices[[1]]@values)
    }
    else if(unique(types)[1] == "FuzzyPairwiseComparisonMatrix"){
      size = nrow(listOfMatrices[[1]]@fnMin)
    }

    modalValues = matrix(data = 0, nrow = size, ncol = size)
    lowerValues = matrix(data = 0, nrow = size, ncol = size)
    upperValues = matrix(data = 0, nrow = size, ncol = size)

    if(unique(types)[1] == "PairwiseComparisonMatrix"){

      for (i in 1:size){
        for (j in 1:size){
          values = c()

          for (k in 1:number){
            values = c(values, listOfMatrices[[k]]@values[i, j])
          }

          modalValues[i, j] = prod(values)^(1/number)
          lowerValues[i, j] = min(values)
          upperValues[i, j] = max(values)
        }
      }
    }
    else if(unique(types)[1] == "FuzzyPairwiseComparisonMatrix"){

      for (i in 1:size){
        for (j in 1:size){
          valuesMod = c()
          valuesMin = c()
          valuesMax = c()

          for (k in 1:number){
            valuesMin = c(valuesMin, listOfMatrices[[k]]@fnMin[i, j])
            valuesMod = c(valuesMod, listOfMatrices[[k]]@fnModal[i, j])
            valuesMax = c(valuesMax, listOfMatrices[[k]]@fnMax[i, j])
          }

          modalValues[i, j] = prod(valuesMod)^(1/number)
          lowerValues[i, j] = prod(valuesMin)^(1/number)
          upperValues[i, j] = prod(valuesMax)^(1/number)
        }
      }
    }



    return(new("FuzzyPairwiseComparisonMatrix", fnMin = lowerValues, fnModal = modalValues,
               fnMax = upperValues, variableNames = listOfMatrices[[1]]@variableNames))
  }
)

setGeneric(".typeOfObjects",
           function(listMatrices) standardGeneric(".typeOfObjects"))

setMethod(
  f=".typeOfObjects",
  signature(listMatrices = "list"),
  definition=function(listMatrices)
  {
    typesList = c()
    for(i in 1:length(listMatrices)){
      typesList = c(typesList, class(listMatrices[[i]]))
    }

    return(typesList)
  }
)

setGeneric(".checkDimensions",
           function(listOfMatrices, type) standardGeneric(".checkDimensions"))

setMethod(
  f=".checkDimensions",
  signature(listOfMatrices = "list", type = "character"),
  definition=function(listOfMatrices, type)
  {

    for(i in 1:length(listOfMatrices)){

      if(type == "PairwiseComparisonMatrix"){
        if (dim(listOfMatrices[[1]]@values)[1] != dim(listOfMatrices[[i]]@values)[1] &&
            dim(listOfMatrices[[1]]@values)[2] != dim(listOfMatrices[[i]]@values)[2]){

          stop(paste0("PairwiseComparisonMatrices do not have the same sizes: [", dim(listOfMatrices[[1]]@values)[1], ",",
                      dim(listOfMatrices[[1]]@values)[2], "] != [", dim(listOfMatrices[[i]]@values)[1], ",",
                      dim(listOfMatrices[[1]]@values)[2], "]."))
        }
      }
      else if(type == "FuzzyPairwiseComparisonMatrix"){
        if (dim(listOfMatrices[[1]]@fnMin)[1] != dim(listOfMatrices[[i]]@fnMin)[1] &&
            dim(listOfMatrices[[1]]@fnMin)[2] != dim(listOfMatrices[[i]]@fnMin)[2]){

          stop(paste0("PairwiseComparisonMatrices do not have the same sizes: [", dim(listOfMatrices[[1]]@fnMin)[1], ",",
                      dim(listOfMatrices[[1]]@fnMin)[2], "] != [", dim(listOfMatrices[[i]]@fnMin)[1], ",",
                      dim(listOfMatrices[[1]]@fnMin)[2], "]."))
        }
      }

    }

  }
)
