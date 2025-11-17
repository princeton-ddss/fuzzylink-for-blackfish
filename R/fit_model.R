#' @param verbose TRUE to print progress updates, FALSE for no output
#' @param learner Which supervised learner should be used to predict match probabilities. Defaults to logistic regression ('glm'), but will also accept random forest ('ranger').
#' @param fmla By default, logistic regression model predicts whether two records match as a linear combination of embedding similarity and Jaro-Winkler similarity (`match ~ sim + jw`). Change this input for alternate specifications.
#' @param train The training dataset
#' @param sim the similarity matrix
#' @param debug TRUE to print various statments throughout the code to track progess. Defaults to FALSE.
#'
#' @return The fit
#' @export
fit_model <- function(verbose,
                      learner,
                      fmla,
                      train,
                      sim,
                      debug) {

# Step 4: Fit model -------------------
  if(debug){
    print("DEBUG: BEGINNING STEP 4: FITTING MODEL -----------------------------------------")
  }

  
  if(verbose){
    if(debug){
      print("DEBUG: verbose")
    }
    message('Fitting model (',
        format(Sys.time(), '%X'),
        ')\n\n', sep = '')
  }
  
  if(learner == 'ranger'){
    if(debug){
      print("DEBUG: learner == ranger")
    }
    fit <- ranger::ranger(x = train |>
                            dplyr::filter(match %in% c('Yes', 'No')) |>
                            dplyr::select(sim, jw:soundex),
                          y = factor(train$match[train$match %in% c('Yes', 'No')]),
                          probability = TRUE)
  } else{
    if(debug){
      print("DEBUG: learner != ranger")
    }
    if (debug) {
      print( "The unique values in the table: ")
      print(unique(train$match))
      print("count occurances:")
      print(train$match, useNA = "ifany")
      print("structure:")
      print(str(train$match))
    }

    fit <- stats::glm(fmla,
                      data = train |>
                        dplyr::filter(match %in% c('Yes', 'No')) |>
                        dplyr::mutate(match = as.numeric(match == 'Yes')),
                      family = 'binomial')
  }

  return(fit)

}