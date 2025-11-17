recall_search <- function(df,
                          learner,
                          verbose,
                          record_type,
                          instructions,
                          model,
                          openai_api_key,
                          parallel,
                          text_gen_port_num,
                          fit,
                          num_exact,
                          train = train,
                          debug) {

    ## Step 6: Recall Search -----------------
  if(debug){
    print("DEBUG: BEGINNING STEP 6: RECALL SEARCH -----------------------------------------")
  }

  # 1. Identify records in A without in-block matches from B
  # 2. Sample from kernel in batches of 100; label but do not update model.
  # Loop 1-2 until either there are no remaining record pairs to label or you've hit
  # user-specified label maximum


  # add the exact matches back to train before remerging with df
  if(num_exact > 0){
    train <- dplyr::bind_rows(train_exact, train)
  }

  df <- df |>
    # merge with labels from train set
    dplyr::left_join(train |>
                       dplyr::select(A, B, match),
                     by = c('A', 'B'))

  if(learner == 'ranger'){
    df$match_probability <- stats::predict(fit, df)$predictions[,'Yes']
  } else{
    df$match_probability <- stats::predict.glm(fit, df, type = 'response')
  }

  # for exact matches, match_probability = 1
  df$match_probability <- ifelse(df$A == df$B, 1, df$match_probability)

  stop_condition_met <- FALSE
  while(!stop_condition_met){

    # find all records in A with no within-block matches
    # and return any unlabeled record pairs
    cutoff <- get_cutoff(df, fit)
    to_search <- df |>
      dplyr::group_by(A, block) |>
      dplyr::filter(sum(match == 'Yes' | match_probability > cutoff,
                        na.rm = TRUE) == 0) |>
      dplyr::filter(is.na(match)) |>
      dplyr::distinct(A, B, .keep_all = TRUE) |>
      dplyr::ungroup()

    if(nrow(to_search) == 0){
      break
    }

    # Gaussian kernel
    p_draw <- stats::dnorm(stats::qlogis(to_search$match_probability),
                      mean = 0,
                      sd = kernel_sd)
    if(sum(p_draw > 0) == 0){
      break
    }

    remaining_budget <- max_labels - sum(!is.na(df$match))
    if(verbose){
      message(paste0('Record Pairs Remaining To Label: ',
                 prettyNum(min(remaining_budget, sum(p_draw>0)),
                           big.mark = ','),
                 '\n\n'))
    }


    pairs_to_label <- sample(
      1:nrow(to_search),
      size = ifelse(sum(p_draw > 0) < batch_size, sum(p_draw > 0), batch_size),
      replace = FALSE,
      prob = p_draw
    )

    # add the labels to the dataset
    to_search$match[pairs_to_label] <- check_match(
      to_search$A[pairs_to_label],
      to_search$B[pairs_to_label],
      record_type = record_type,
      instructions = instructions,
      model = model,
      openai_api_key = openai_api_key,
      parallel = parallel,
      port_number = text_gen_port_num,
      debug = debug
    )

    # merge into df, updating match values where they differ
    to_search <- dplyr::select(to_search, A, B, match)
    df <- df |>
      dplyr::left_join(to_search,
                       by = c("A", "B"),
                       suffix = c(".1", ".2")) |>
      dplyr::mutate(match = dplyr::coalesce(match.1, match.2)) |>
      dplyr::select(-match.1, -match.2)

    # check if stopping condition has been met
    if(sum(!is.na(df$match)) >= max_labels){
      stop_condition_met <- TRUE
    }
  }
  return(df)
}