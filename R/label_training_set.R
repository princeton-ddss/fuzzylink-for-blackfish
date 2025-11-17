#' @description
#' Labelling the training set
#' 
#' @param dfA,dfB A pair of data frames or data frame extensions (e.g. tibbles)
#' @param by A character denoting the name of the variable to use for fuzzy matching
#' @param verbose TRUE to print progress updates, FALSE for no output
#' @param blocking.variables A character vector of variables that must match exactly in order to match two records
#' @param record_type A character describing what type of entity the `by` variable represents. Should be a singular noun (e.g. "person", "organization", "interest group", "city").
#' @param instructions A string containing additional instructions to include in the LLM prompt during validation.
#' @param model Which LLM to prompt when validating matches; defaults to 'gpt-4o-2024-11-20	'. Set to "EMPTY" to run locally.
#' @param openai_api_key Your OpenAI API key. By default, looks for a system environment variable called "OPENAI_API_KEY" (recommended option). Otherwise, it will prompt you to enter the API key as an argument.
#' @param parallel TRUE to submit API requests in parallel. Setting to FALSE can reduce rate limit errors at the expense of longer runtime.
#' @param learner Which supervised learner should be used to predict match probabilities. Defaults to logistic regression ('glm'), but will also accept random forest ('ranger').
#' @param text_gen_port_num The port number that the local text generation model is running on. Defaults to 8081. 
#' @param sim A matrix of embedding vectors
#' @param debug TRUE to print various statments throughout the code to track progess. Defaults to FALSE.
#' 
#'
#' @return the training set, the number of exact matches, updated df
#' @export
label_training_set <- function(dfA, dfB, 
                                verbose, by,
                                blocking.variables,
                                record_type,
                                instructions,
                                model,
                                openai_api_key,
                                parallel,
                                text_gen_port_num,
                                learner,
                                sim,
                                debug) {

 if(debug){
    print("DEBUG: BEGINNING STEP 3: LABELLING TRAINING SET --------------------------------")
  }

  if(verbose){
    message('Labeling Initial Training Set (',
        format(Sys.time(), '%X'),
        ')\n\n', sep = '')
  }

  # df is the dataset of all within-block name pairs
  df <- reshape2::melt(sim)
  # rename columns
  namekey <- c(Var1 = 'A', Var2 = 'B', value = 'sim', L1 = 'block')
  names(df) <- namekey[names(df)]
  df <- dplyr::filter(df, !is.na(sim))
  df$A <- as.character(df$A)
  df$B <- as.character(df$B)

  # add lexical string distance measures
  df$jw <- stringdist::stringsim(tolower(df$A), tolower(df$B),
                                 method = 'jw', p = 0.1)

  # if using random forest as supervised learner, append full suite of
  # lexical string distance measures
  if(learner == 'ranger'){
    df$osa = stringdist::stringdist(tolower(df$A), tolower(df$B), method = "osa")
    df$cosine = stringdist::stringdist(tolower(df$A), tolower(df$B), method = "cosine")
    df$jaccard = stringdist::stringdist(tolower(df$A), tolower(df$B), method = "jaccard")
    df$lcs = stringdist::stringdist(tolower(df$A), tolower(df$B), method = "lcs")
    df$qgram = stringdist::stringdist(tolower(df$A), tolower(df$B), method = "qgram")
    df$soundex = stringdist::stringdist(tolower(df$A), tolower(df$B), method = "soundex")
  }

  # the 'train' dataset removes duplicate A/B pairs
  train <- df |>
    dplyr::distinct(A, B, .keep_all = TRUE)

  # omit exact matches from the train set during active learning loop
  num_exact <- sum(train$A == train$B)
  if(num_exact > 0){
    train_exact <- train[train$A == train$B,]
    train_exact$match <- 'Yes'
    train_exact$match_probability <- 1
    train <- train[train$A != train$B,]
  }

  # label initial training set (n_t=500)
  train$match <- NA
  n_t <- 500
  k <- max(floor(n_t / length(unique(train$A))), 1)
  pairs_to_label <- train |>
    # create index number
    dplyr::mutate(index = dplyr::row_number()) |>
    # get the k largest sim values in each group
    dplyr::group_by(A) |>
    dplyr::slice_max(sim, n = k) |>
    dplyr::ungroup() |>
    # only keep n_t record pairs
    dplyr::slice_sample(n = n_t) |>
    dplyr::pull(index)

    # if (debug) {
    #   print("train$match originally:")
    #   print( "The unique values in the table: ")
    #   print(unique(train$match))
    #   print("count occurances:")
    #   print(train$match, useNA = "ifany")
    #   print("structure:")
    #   print(str(train$match))
    # }

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

#   if (debug) {
#       print("train$match sfter check_match:")
#       print( "The unique values in the table: ")
#       print(unique(train$match))
#       print("count occurances:")
#       print(train$match, useNA = "ifany")
#       print("structure:")
#       print(str(train$match))
#     }

results <- list("train" = train, "num_exact" = num_exact, "df" = df)
return(results)
# return(train, num_exact)
 
}