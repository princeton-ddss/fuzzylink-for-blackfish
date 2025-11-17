#' @param verbose TRUE to print progress updates, FALSE for no output
#' @param learner Which supervised learner should be used to predict match probabilities. Defaults to logistic regression ('glm'), but will also accept random forest ('ranger').
#' @param fmla By default, logistic regression model predicts whether two records match as a linear combination of embedding similarity and Jaro-Winkler similarity (`match ~ sim + jw`). Change this input for alternate specifications.
#' @param train The training dataset
#' @param sim the similarity matrix
#' @param debug TRUE to print various statments throughout the code to track progess. Defaults to FALSE.
#'
#' @return The fit
#' @export
active_learning_loop <- function(learner,
                                verbose,
                                train,
                                fmla,
                                record_type,
                                instructions,
                                model ,
                                openai_api_key ,
                                parallel ,
                                text_gen_port_num,
                                sim,
                                fit,
                                debug) {

# Step 5: Active Learning Loop ---------------
  if(debug){
    print("DEBUG: BEGINNING STEP 5: ACTIVE LEARNING LOOP --------------------------------------")
  }

  i <- 1
  window_size <- 5
  gradient_estimate <- 0
  stop_threshold <- 0.01
  kernel_sd <- 0.2
  batch_size <- 100
  stop_condition_met <- FALSE
  if(learner == 'ranger'){
    stop_threshold <- 0.1
    train$match_probability <- stats::predict(fit, train)$predictions[,'Yes']
  } else{
    train$match_probability <- stats::predict.glm(fit, train, type = 'response')
  }


  while(!stop_condition_met){

    if(verbose){
      message('Refining Model ',
          i, ' (',
          format(Sys.time(), '%X'),
          ')\n\n', sep = '')
    }

    # Gaussian kernel
    log_odds <- stats::qlogis(train$match_probability)
    p_draw <- ifelse(is.na(train$match),
                     stats::dnorm(log_odds, mean = 0, sd = kernel_sd),
                     0)
    if(sum(p_draw > 0) == 0){
      break
    }

    pairs_to_label <- sample(
      1:nrow(train),
      size = ifelse(sum(p_draw > 0) < batch_size, sum(p_draw > 0), batch_size),
      replace = FALSE,
      prob = p_draw
    )

    # add the labels to the dataset
    train$match[pairs_to_label] <- check_match(
      train$A[pairs_to_label],
      train$B[pairs_to_label],
      record_type = record_type,
      instructions = instructions,
      model = model,
      openai_api_key = openai_api_key,
      parallel = parallel,
      port_number = text_gen_port_num,
      debug = debug
    )

    # refit the model and re-estimate match probabilities
    old_probs <- train$match_probability
    if(learner == 'ranger'){
      fit <- ranger::ranger(x = train |>
                              dplyr::filter(match %in% c('Yes', 'No')) |>
                              dplyr::select(sim, jw:soundex),
                            y = factor(train$match[train$match %in% c('Yes', 'No')]),
                            probability = TRUE)
      train$match_probability <- stats::predict(fit, train)$predictions[,'Yes']
      # for RF, only estimate gradient on out-of-sample observations
      gradient_estimate[i] <- max(abs(old_probs - train$match_probability)[is.na(train$match)])
    } else{
      fit <- stats::glm(fmla,
                        data = train |>
                          dplyr::filter(match %in% c('Yes', 'No')) |>
                          dplyr::mutate(match = as.numeric(match == 'Yes')),
                        family = 'binomial')

      train$match_probability <- stats::predict.glm(fit, train, type = 'response')
      gradient_estimate[i] <- max(abs(old_probs - train$match_probability))
    }



    if(i >= window_size){
      if(mean(gradient_estimate[(i-window_size+1):i]) < stop_threshold){
        stop_condition_met <- TRUE
      }
    }

    i <- i + 1
  }

 results <- list("train" = train, "fit" = fit)
 return(results)
}