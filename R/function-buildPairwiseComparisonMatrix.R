#' Function that builds Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices
#'
#' @description
#' This functions builds Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices
#' the resulting Pairwise Comparison Matrix is calculated as geometric mean of all
#' Pairwise Comparison Matrices in \code{listOfMatrices}.
#'
#' @param listOfMatrices An object of \code{\linkS4class{list}}.
#' @param agg A \code{\linkS4class{character}} specifying aggreation used to build
#' Pairwise comparison matrix. Values \code{"geometic"} and \code{"arithmetic"} means are implemented,
#' with \code{"geometic"} being default value.
#'
#' @return An object of class \code{\linkS4class{PairwiseComparisonMatrix}}
#'
#' @export
#' @rdname buildPairwiseComparisonMatrix-methods
#' @name buildPairwiseComparisonMatrix
setGeneric("buildPairwiseComparisonMatrix",
           function(listOfMatrices, agg = "geometric") standardGeneric("buildPairwiseComparisonMatrix"))

#' @rdname buildPairwiseComparisonMatrix-methods
#' @aliases buildPairwiseComparisonMatrix,list,character-method
setMethod(
  f="buildPairwiseComparisonMatrix",
  signature(listOfMatrices = "list", agg = "character"),
  definition=function(listOfMatrices, agg)
  {

    number = length(listOfMatrices)
    size = nrow(listOfMatrices[[1]]@values)

    for(i in 1:number){
      if (class(listOfMatrices[[i]]) != "PairwiseComparisonMatrix"){
        stop(paste0("Element on position ", i, " is not of class PairwiseComparisonMatrix. Its type is ", class(listOfMatrices[[i]]), "."))
      }

      if (dim(listOfMatrices[[1]]@values)[1] != dim(listOfMatrices[[i]]@values)[1] &&
          dim(listOfMatrices[[1]]@values)[2] != dim(listOfMatrices[[i]]@values)[2]){

        stop(paste0("PairwiseComparisonMatrices do not have the same sizes: [", dim(listOfMatrices[[1]]@values)[1], ",",
                    dim(listOfMatrices[[1]]@values)[2], "] != [", dim(listOfMatrices[[i]]@values)[1], ",",
                    dim(listOfMatrices[[1]]@values)[2], "]."))
      }
    }

    if(!agg %in% c("geometric", "arithmetic")){
      stop(paste0("Unknow aggreation type - ", agg, ". Implemented types are geometric and arithmetic."))
    }

    resultMatrix = listOfMatrices[[1]]@values

    for (i in 1:size){
      for (j in 1:size){
        vector = c()

        for (k in 1:number){
          vector = c(vector, listOfMatrices[[k]]@values[i, j])
        }

        if(agg == "geometric"){
          resultMatrix[i, j] = prod(vector)^(1/number)
        }
        else if(agg == "arithmetic"){
          resultMatrix[i, j] = mean(vector)
        }
        else{
          stop("Error in aggreation type. This should not happen.")
        }

      }
    }

    textMatrix = .textMatrixRepresentation(resultMatrix)

    return(new("PairwiseComparisonMatrix", valuesChar = textMatrix, values = resultMatrix,
               variableNames = listOfMatrices[[1]]@variableNames))
  }
)
