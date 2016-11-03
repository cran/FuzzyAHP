setGeneric("calculateFuzzyAHP",
           function(criteraMatrix, alternativesMatrices) standardGeneric("calculateFuzzyAHP"))

setMethod(
  f="calculateFuzzyAHP",
  signature(criteraMatrix="FuzzyPairwiseComparisonMatrix", alternativesMatrices = "list"),
  definition=function(criteraMatrix, alternativesMatrices)
  {
    correct = lapply(alternativesMatrices, function(x) {
      if(is(x, "FuzzyPairwiseComparisonMatrix")){
        return(TRUE)
      }else{
        return(FALSE)
      }

    })

    if(length(which(correct==FALSE)) > 0){
      stop(paste0("All elements of list alternativesMatrices must be of class FuzzyPairwiseComparisonMatrix.",
                  " But another class was found in the list."))
    }

    if(nrow(criteraMatrix@fnModal) != length(alternativesMatrices)){
      stop("Number of criteria weights must be equal to number of matrices of alternatives.")
    }

    alternativesWeights = lapply(alternativesMatrices,
                                 function(x) {
                                   return(calculateWeights(x))
                                 }
    )

    # print(alternativesWeights)



  }
)

