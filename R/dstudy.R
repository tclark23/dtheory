#' Execute a D-Study
#'
#' @param data A data frame containing a column for "Person", a column for "Trial", and the remaining columns for metrics.
#' @param col.scores A column index or variable name denoting the column in the original data frame that serves as the dependent variable.
#' @param from The first number of trials to test.
#' @param to The final number of trials to test.
#' @param by The interval at which trial numbers should be tested.
#' @param rounded The number of decimal places the reliability coefficients should be rounded to. 3 by default.
#'
#' @return A data frame of G-coefficients (absolute agreement) for the specified numbers of trials.
#' @export
#'
#' @examples
#' x <- data.frame(c(1,1,1,2,2,2,3,3,3), c(1,2,3,1,2,3,1,2,3), c(12, 14, 12, 22, 22, 19, 17, 22, 20))
#' colnames(x) <- c("Person", "Trial", "Metric")
#' dstudy(x, col.scores = "Metric", from = 1, to = 10, by = 1)
dstudy <- function(data, col.scores, from, to, by, rounded = 3) {
  # Dealing with data in the incorrect format
  if (!("Person" %in% colnames(data))) {
    stop("Data frame must contain a column called 'Person'", call. = F)
  }
  else if(!("Trial" %in% colnames(data))) {
    stop("Data frame must contain a column called 'Trial'", call. = F)
  }

  # Dealing with the different ways the user might enter col.scores
  suppressWarnings(if (!is.na(as.integer(col.scores)) & col.scores == as.integer(col.scores)) {
    if (col.scores <= 0 | col.scores > ncol(data)) {
      stop(paste0("Invalid column index entered for col.scores!"), call. = F)
    }
    else {
      data.small <- data %>%
        dplyr::select(c("Person", "Trial", as.integer(col.scores)))

      col.scores <- colnames(data)[col.scores]
    }
  }
  else {
    if (!(col.scores %in% colnames(data))) {
      stop(paste0("'", col.scores, "'", " is not a column in the data frame. Please check spelling."), call. = F)
    }
    else {
      data.small <- data %>%
        dplyr::select(c("Person", "Trial", col.scores))
    }
  })

  colnames(data.small)[3] <- "Measure"

  # Condition checking
  if (!class(data.small$Measure) %in% c("integer", "numeric")) {
    stop("Scores data must be numeric!")
  }

  # Running a G-study from the gtheory package
  formula2 <- Measure ~ (1|Person) + (1|Trial)
  comps <- data.frame(gtheory::gstudy(data = data.small, formula2)$components)
  y <- c("Person", "Trial", "Residual")

  # Putting the output into a specific format
  comps <- comps %>%
    dplyr::slice(match(y, source))

  suppressWarnings(if (is.logical(from) | is.na(as.integer(from)) | from != as.integer(from) | as.integer(from) <= 0) {
    stop("'from' must be a positive integer.", call. = F)
  }
  else if (is.logical(to) | is.na(as.integer(to)) | to != as.integer(to) | as.integer(to) <= 0) {
    stop("'to' must be a positive integer.", call. = F)
  }
  else if (is.logical(by) | is.na(as.integer(by)) | by != as.integer(by)) {
    stop("'by' must be an integer.", call. = F)
  }
  else {
    from <- as.integer(from)
    to <- as.integer(to)
    by <- as.integer(by)
  })

  suppressWarnings(if (is.logical(rounded) | is.na(as.integer(rounded)) | rounded != as.integer(rounded) | as.integer(rounded) <= 0) {
    stop("'rounded' must be a positive integer.", call. = F)
  })

  # Calculating the first G-coef and adding it to the final data frame
  final_df <- data.frame(0)
  colnames(final_df)[1] <- paste0("n = ", from)
  rownames(final_df)[1] <- col.scores
  comps$dvar <- c(comps[1,2], comps[2,2]/from, comps[3,2]/from)
  G1 <- comps[1, ncol(comps)]/sum(comps$dvar)
  final_df[1,1] <- round(G1, as.integer(rounded))

  # Iterating through the rest of the trial numbers in the sequence and adding
  # the corresponding G-coefs to the final data frame as well
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
