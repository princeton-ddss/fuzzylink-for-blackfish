#' @description
#' Get similarity matrix within each block
#' 
#' @param dfA,dfB A pair of data frames or data frame extensions (e.g. tibbles)
#' @param by A character denoting the name of the variable to use for fuzzy matching
#' @param verbose TRUE to print progress updates, FALSE for no output
#' @param blocking.variables A character vector of variables that must match exactly in order to match two records
#' @param blocks every unique combination of blocking variables in dfA
#' @param embeddings The embeddings for all unique strings from dfA and dfB
#' @param debug TRUE to print various statments throughout the code to track progess. Defaults to FALSE.
#' 
#'
#' @return A matrix of embedding vectors (one per row).
#' @export
get_similarity_matrix_per_block <- function(dfA, dfB, 
                                            verbose, by,
                                            blocking.variables,
                                            blocks,
                                            embeddings,
                                            debug) {
    
    ## Step 2: Get similarity matrix within each block ------------
  if(debug){
    print("DEBUG: BEGINNING STEP 2: GETTING SIMILARITY MATRICES ---------------------------")
  }

  if(verbose){
    message('Computing similarity matrix (',
        format(Sys.time(), '%X'),
        ')\n\n', sep = '')
  }
  sim <- list()
  for(i in 1:nrow(blocks)){

    if(!is.null(blocking.variables)){

      # subset the data for each block from dfA and dfB
      subset_A <- mapply(`==`,
                         dfA[, blocking.variables,drop=FALSE],
                         blocks[i,]) |>
        apply(1, all)
      block_A <- dfA[subset_A, ]

      subset_B <- mapply(`==`,
                         dfB[, blocking.variables,drop=FALSE],
                         blocks[i,]) |>
        apply(1, all)
      block_B <- dfB[subset_B, ]

      # if you can't find any matches in dfA or dfB, go to the next block
      if(nrow(block_A) == 0 | nrow(block_B) == 0){
        sim[[i]] <- NA
        next
      }

    } else{
      # if not blocking, compute similarity matrix for all dfA and dfB
      block_A <- dfA
      block_B <- dfB
    }

    # get a unique list of strings in each dataset
    strings_A <- unique(block_A[[by]])
    strings_B <- unique(block_B[[by]])

    # compute cosine similarity matrix
    sim[[i]] <- get_similarity_matrix(embeddings, strings_A, strings_B)
  }
  
   return(sim)
}