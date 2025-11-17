

 # return the cutoff that maximizes expected F-score
  get_cutoff <- function(df, fit){
    df <- df[order(df$match_probability),]
    df$expected_false_negatives <- cumsum(df$match_probability)
    df$identified_false_negatives <- cumsum(ifelse(is.na(df$match), 0, as.numeric(df$match == 'Yes')))
    df <- df[order(-df$match_probability),]
    df$expected_false_positives <- cumsum(1-df$match_probability)
    df$identified_false_positives <- cumsum(1 - ifelse(is.na(df$match), 1, as.numeric(df$match == 'Yes')))
    df$expected_true_positives <- cumsum(df$match_probability)
    df$identified_true_positives <- cumsum(ifelse(is.na(df$match), 0, as.numeric(df$match == 'Yes')))

    total_labeled_true <- sum(df$match == 'Yes', na.rm = TRUE)

    df$tp <- total_labeled_true + (df$expected_true_positives - df$identified_true_positives)
    df$fp <- df$expected_false_positives - df$identified_false_positives
    df$fn <- df$expected_false_negatives - df$identified_false_negatives

    df$expected_recall <- df$tp / (df$tp + df$fn)
    df$expected_precision <- df$tp / (df$tp + df$fp)
    df$expected_f1 = 2 * (df$expected_recall * df$expected_precision) /
      (df$expected_recall + df$expected_precision)

    return(df$match_probability[which.max(df$expected_f1)])
  }