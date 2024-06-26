#' Execute a D-Study
#'
#' @param data A data frame containing a column for "Person", a column for "Trial", and the remaining columns for metrics.
#' @param col.scores A column index or variable name denoting the column in the original data frame that serves as the dependent variable.
#' @param from The first number of trials to test.
#' @param to The final number of trials to test.
#' @param by The interval at which trial numbers should be tested.
#' @param rounded The number of decimal places the reliability coefficients should be rounded to.
#'
#' @return A data frame of G-coefficients (absolute agreement) for the specified numbers of trials.
#' @export
#'
#' @examples
#' x <- data.frame(c(1,1,1,2,2,2,3,3,3), c(1,2,3,1,2,3,1,2,3), c(12, 14, 12, 22, 22, 19, 17, 22, 20))
#' colnames(x) <- c("Person", "Trial", "Metric")
#' dstudy(x, col.scores = "Metric", from = 1, to = 10, by = 1)
dstudy <- function(data, col.scores, from, to, by, rounded = 3) {
  suppressWarnings(if (!is.na(as.integer(col.scores)) & col.scores == as.integer(col.scores)) {
    data.small <- data %>%
      dplyr::select(c("Person", "Trial", as.integer(col.scores)))

    col.scores <- colnames(data)[col.scores]
  }
  else {
    data.small <- data %>%
      dplyr::select(c("Person", "Trial", col.scores))
  })

  colnames(data.small)[3] <- "Measure"
  formula2 <- Measure ~ (1|Person) + (1|Trial)
  comps <- data.frame(gtheory::gstudy(data = data.small, formula2)$components)
  y <- c("Person", "Trial", "Residual")

  comps <- comps %>%
    dplyr::slice(match(y, source))

  from <- as.integer(from)
  to <- as.integer(to)
  by <- as.integer(by)

  final_df <- data.frame(0)
  colnames(final_df)[1] <- paste0("n = ", from)
  rownames(final_df)[1] <- col.scores
  comps$dvar <- c(comps[1,2], comps[2,2]/from, comps[3,2]/from)
  G1 <- comps[1, ncol(comps)]/sum(comps$dvar)
  final_df[1,1] <- round(G1, as.integer(rounded))

  for (n in seq(from = from + by, to = to, by = by)) {
    temp <- data.frame(0)
    colnames(temp)[1] <- paste0("n = ", n)
    comps$dvar <- c(comps[1,2], comps[2,2]/n, comps[3,2]/n)
    G_i <- comps[1, ncol(comps)]/sum(comps$dvar)
    temp[1,1] <- round(G_i, as.integer(rounded))
    final_df <- cbind(final_df, temp)
  }

  return(final_df)
}
