#' Probabilistic Record Linkage Using Pretrained Text Embeddings
#'
#' @param dfA,dfB A pair of data frames or data frame extensions (e.g. tibbles)
#' @param by A character denoting the name of the variable to use for fuzzy matching
#' @param blocking.variables A character vector of variables that must match exactly in order to match two records
#' @param verbose TRUE to print progress updates, FALSE for no output
#' @param record_type A character describing what type of entity the `by` variable represents. Should be a singular noun (e.g. "person", "organization", "interest group", "city").
#' @param instructions A string containing additional instructions to include in the LLM prompt during validation.
#' @param model Which LLM to prompt when validating matches; defaults to 'gpt-4o-2024-11-20	'. Set to "EMPTY" to run locally.
#' @param openai_api_key Your OpenAI API key. By default, looks for a system environment variable called "OPENAI_API_KEY" (recommended option). Otherwise, it will prompt you to enter the API key as an argument.
#' @param embedding_dimensions The dimension of the embedding vectors to retrieve. Defaults to 256
#' @param embedding_model Which pretrained embedding model to use; defaults to 'text-embedding-3-large' (OpenAI), but will also accept 'mistral-embed' (Mistral). Set to "EMPTY" to run locally.
#' @param learner Which supervised learner should be used to predict match probabilities. Defaults to logistic regression ('glm'), but will also accept random forest ('ranger').
#' @param fmla By default, logistic regression model predicts whether two records match as a linear combination of embedding similarity and Jaro-Winkler similarity (`match ~ sim + jw`). Change this input for alternate specifications.
#' @param max_labels The maximum number of LLM prompts to submit when labeling record pairs. Defaults to 10,000
#' @param parallel TRUE to submit API requests in parallel. Setting to FALSE can reduce rate limit errors at the expense of longer runtime.
#' @param return_all_pairs If TRUE, returns *every* within-block record pair from dfA and dfB, not just validated pairs. Defaults to FALSE.
#' @param embedding_port_num The port number that the local embedding model is running on. Defaults to 8080. 
#' @param text_gen_port_num The port number that the local text generation model is running on. Defaults to 8081. 
#' @param save_embeddings TRUE to save the intermediate embeddings. Defaults to FALSE.
#' @param debug TRUE to print various statments throughout the code to track progess. Defaults to FALSE.
#'
#' @return A dataframe with all rows of `dfA` joined with any matches from `dfB`
#' @export
#'
#' @examples
#' \dontrun{
#' dfA <- data.frame(state.x77)
#' dfA$name <- rownames(dfA)
#' dfB <- data.frame(name = state.abb, state.division)
#' df <- fuzzylink(dfA, dfB,
#'                 by = 'name',
#'                 record_type = 'US state government',
#'                 instructions = 'The second dataset contains US postal codes.')
#' }
fuzzylink <- function(dfA, dfB,
                      by, blocking.variables = NULL,
                      verbose = TRUE,
                      record_type = 'entity',
                      instructions = NULL,
                      model = 'gpt-4o-2024-11-20',
                      openai_api_key = Sys.getenv('OPENAI_API_KEY'),
                      embedding_dimensions = 256,
                      embedding_model = 'text-embedding-3-large',
                      learner = 'glm',
                      fmla = match ~ sim + jw,
                      max_labels = 1e4,
                      parallel = TRUE,
                      return_all_pairs = FALSE,
                      embedding_port_num = 8080,
                      text_gen_port_num = 8081,
                      save_embeddings = FALSE,
                      # name = NULL, #The 'name' of this run for saving intermediate files. Defaults to the name of the model + embedding_model
                      debug = FALSE){

  # Check for errors in inputs
  if(debug){
    print("DEBUG: Beginning to check for errors in inputs")
  }
  # if(is.null(name)) {
  #   name <- paste(model, embedding_model, sep='_')
  # }

  if(is.null(dfA[[by]])){
    stop("There is no variable called \'", by, "\' in dfA.")
  }
  if(is.null(dfB[[by]])){
    stop("There is no variable called \'", by, "\' in dfB.")
  }
  if(openai_api_key == ''){
    if(model != "EMPTY") {
      stop("No API key for model detected in system environment. You can enter it manually using the 'openai_api_key' argument.")
    }
    if(embedding_model != "EMPTY") {
      stop("No API key for embedding detected in system environment. You can enter it manually using the 'openai_api_key' argument.")
    }
  }
  missing_dfA <- sum(!stats::complete.cases(dfA[,c(by, blocking.variables), drop = FALSE]))
  if(missing_dfA > 0){
    warning('Dropping ', missing_dfA, ' observation(s) with missing values from dfA.')
    dfA <- dfA[stats::complete.cases(dfA[, c(by,blocking.variables), drop = FALSE]), ]
  }
  missing_dfB <- sum(!stats::complete.cases(dfB[,c(by, blocking.variables), drop = FALSE]))
  if(missing_dfB > 0){
    warning('Dropping ', missing_dfB, ' observation(s) with missing values from dfB.')
    dfB <- dfB[stats::complete.cases(dfB[, c(by,blocking.variables), drop = FALSE]), ]
  }
 
 if(debug){
    print("DEBUG: No errors found in inputs")
  }

  ## Step 0: Blocking -----------------
  blocked <- block_data(dfA = dfA,
                       dfB = dfB,
                       blocking.variables = blocking.variables,
                       debug = debug)
  dfB <- blocked$dfB
  blocks <- blocked$blocks

  ## Step 1: Get embeddings ----------------

  embeddings <- get_embeddings(dfA = dfA,
                               dfB = dfB,
                               by = by,
                               verbose = verbose,
                               model = embedding_model,
                               dimensions = embedding_dimensions,
                               openai_api_key = openai_api_key,
                               parallel = parallel,
                               port_number = embedding_port_num,
                               debug = debug)

  

  ## Step 2: Get similarity matrix within each block ------------
  
  sim <- get_similarity_matrix_per_block(dfA = dfA,
                                        dfB = dfB,
                                        by = by,
                                        verbose = verbose,
                                        blocking.variables = blocking.variables,
                                        blocks = blocks,
                                        embeddings = embeddings,
                                        debug = debug)

  ## Step 3: Label Training Set -------------

  training <- label_training_set(dfA = dfA, dfB = dfB, 
                              verbose = verbose, by = by,
                              blocking.variables = blocking.variables,
                              record_type = record_type,
                              instructions = instructions,
                              model = model,
                              openai_api_key = openai_api_key,
                              parallel = parallel,
                              text_gen_port_num = text_gen_port_num,
                              learner = learner,
                              sim = sim,
                              debug = debug)

  train <- training$train
  num_exact <- training$num_exact
  df <- training$df
 
  ## Step 4: Fit model -------------------

  fit <- fit_model(verbose = verbose,
                    learner = learner,
                    fmla = fmla,
                    train = train,
                    sim = sim,
                    debug = debug)

  # Step 5: Active Learning Loop ---------------

  trained <- active_learning_loop(learner = learner,
                                  verbose = verbose,
                                  train = train,
                                  fmla = fmla,
                                  fit = fit,
                                  record_type = record_type,
                                  instructions = instructions,
                                  model = model,
                                  openai_api_key = openai_api_key,
                                  parallel = parallel ,
                                  text_gen_port_num = text_gen_port_num,
                                  sim = sim,
                                  debug = debug)
  train <- trained$train
  fit <- trained$fit

  ## Step 6: Recall Search -----------------

  df <- recall_search(df = df,
                      learner = learner,
                      verbose = verbose,
                      record_type = record_type,
                      instructions = instructions,
                      model = model,
                      openai_api_key = openai_api_key,
                      parallel = parallel,
                      text_gen_port_num = text_gen_port_num,
                      fit = fit,
                      num_exact = num_exact,
                      train = train,
                      debug = debug)

  ## Step 7: Return Linked Datasets -----------------
  if(debug){
    print("DEBUG: BEGINNING STEP 7: RETURNING LINKED DATASETS -----------------------------------------")
  }

  # if blocking, merge with the blocking variables prior to linking
  if(!is.null(blocking.variables)){
    blocks$block <- 1:nrow(blocks)
    df <- dplyr::left_join(df, blocks, by = 'block')
  }

  if(!return_all_pairs){

    df <- df |>
      # only keep pairs that have been labeled Yes or have a match probability > p_cutoff
      dplyr::filter((match_probability > get_cutoff(df, fit) &
                       is.na(match)) | match == 'Yes') |>
      dplyr::right_join(dfA,
                        by = c('A' = by, blocking.variables),
                        relationship = 'many-to-many') |>
      dplyr::left_join(dfB,
                       by = c('B' = by, blocking.variables),
                       relationship = 'many-to-many')
  }

  if(is.null(blocking.variables)) df <- dplyr::select(df, -block)

  if(verbose){
    message('Done! (',
        format(Sys.time(), '%X'),
        ')\n', sep = '')
  }

  if(debug){
    print("DEBUG: FUZZYLINK METHOD COMPLETE. RETURNING -------------------------------------------------------")
  }

  return(df)

}

## quiets concerns of R CMD check re: dplyr pipelines
utils::globalVariables(c('A', 'B', 'index', 'jw',
                         'soundex', 'block', 'match_probability',
                         'match.1', 'match.2'))
