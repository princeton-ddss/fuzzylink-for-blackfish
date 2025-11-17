#' @param dfA,dfB A pair of data frames or data frame extensions
#' @param blocking.variables A character vector of variables that must match exactly in order to match two records
#' @param debug TRUE to print various statments throughout the code to track progess
#'
#' @return updated dfB and blocks
#' @export
#'

block_data <- function(dfA, dfB,
                       blocking.variables,
                      debug){

  ## Step 0: Blocking -----------------
  if(debug){
    print("DEBUG: BEGINNING STEP 0: BLOCKING----------------------------------------------")
  }

  if(!is.null(blocking.variables)){

    # get every unique combination of blocking variables in dfA
    blocks <- unique(dfA[,blocking.variables,drop = FALSE])

    # keep only the rows in dfB with exact matches on the blocking variables
    dfB <- dplyr::inner_join(dfB, blocks,
                             by = blocking.variables)

    if(nrow(dfB) == 0){
      stop("There are no exact matches in dfB on the blocking.variables specified.")
    }

  } else{
    blocks <- data.frame(block = 1)
  }

  results <- list("dfB" = dfB, "blocks" = blocks)
  return(results)
}