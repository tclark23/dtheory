#' Find G-Coefficient Confidence Interval
#'
#' @param data A data frame containing a column for "Person", a column for "Trial", and the remaining columns for metrics.
#' @param col.scores A column index or variable name denoting the column in the data frame that serves as the dependent variable.
#' @param n The number of trials to test.
#' @param conf.level Confidence level (in decimal form). 0.9 by default.
#' @param rounded The number of decimal places the reliability coefficients should be rounded to. 3 by default.
#'
#' @return A data frame containing the G-coefficient (for absolute agreement), as well as the corresponding lower and upper bounds at the specified confidence level.
#' @export
#'
#' @note
#' The confidence interval calculation, originally developed by Arteaga et al. (1982) assumes that score effects are normally distributed. Hence, any attempt to use dconf() with non-normal data will not produce reasonable results!

#'
#' @examples
#' x <- data.frame(c(1,1,1,2,2,2,3,3,3), c(1,2,3,1,2,3,1,2,3), c(12, 14, 12, 22, 22, 19, 17, 22, 20))
#' colnames(x) <- c("Person", "Trial", "Metric")
#' dconf(x, col.scores = "Metric", n = 5, conf.level = .9)
dconf <- function(data, col.scores, n, conf.level = 0.9, rounded = 3) {
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
        dplyr::select(c("Person", "Trial", as.integer(col.scores))) %>%
        tidyr::drop_na()

      col.scores <- colnames(data)[col.scores]
    }
  }
  else {
    if (!(col.scores %in% colnames(data))) {
      stop(paste0("'", col.scores, "'", " is not a column in the data frame. Please check spelling."), call. = F)
    }
    else {
      data.small <- data %>%
        dplyr::select(c("Person", "Trial", col.scores)) %>%
        tidyr::drop_na()
    }
  })

  colnames(data.small)[3] <- "Measure"

  # Condition checking for correct column types
  if (!class(data.small$Measure) %in% c("integer", "numeric")) {
    stop("Scores data must be numeric!")
  }

  # Testing for univariate normality
  result1 <- stats::shapiro.test(data.small$Measure)
  if (result1$p.value < 0.05){
    warning(paste0("Data entered for ", col.scores, " does not follow normal distribution.
Function may not produce reasonable lower/upper bounds."), call. = F)
  }

  # Running a G-study using the gtheory package
  formula2 <- Measure ~ (1|Person) + (1|Trial)
  comps <- data.frame(gtheory::gstudy(data = data.small, formula2)$components)
  y <- c("Person", "Trial", "Residual")

  # Putting the output into a specific format
  comps <- comps %>%
    dplyr::slice(match(y, source))

  suppressWarnings(if (is.logical(n) | is.na(as.integer(n)) | n != as.integer(n) | as.integer(n) <= 0) {
    stop("'n' must be a positive integer.", call. = F)
  })

  suppressWarnings(if (is.logical(rounded) | is.na(as.integer(rounded)) | rounded != as.integer(rounded) | as.integer(rounded) <= 0) {
    stop("'rounded' must be a positive integer.", call. = F)
  })

  suppressWarnings(if (is.logical(conf.level) | is.na(as.numeric(conf.level)) | conf.level == as.integer(conf.level) | as.numeric(conf.level) <= 0 | as.numeric(conf.level) >= 1) {
    stop("'conf.level' must be a positive decimal between 0 and 1, exclusive.", call. = F)
  })

  # Calculating the G-coef, and adding it to the final data frame
  n <- as.integer(n)
  final_df <- data.frame(0,0,0)
  colnames(final_df)[1] <- paste0("G-coef")
  row.names(final_df)[1] <- paste0(col.scores, ", n = ", n)
  comps$dvar <- c(comps[1,2], comps[2,2]/n, comps[3,2]/n)
  G <- comps[1, ncol(comps)]/sum(comps$dvar)
  final_df[1,1] <- round(G, as.integer(rounded))

  # Using Huebner's method to calculate the MS (needed for CI calculation)
  n_p <- length(unique(data.small$Person))
  n_i <- length(unique(data.small$Trial))
  univ_mean <- mean(data.small$Measure, na.rm = T)
  conf.level <- as.numeric(conf.level)

  persons <- data.small %>% dplyr::group_by(Person) %>% dplyr::summarize(mean(Measure, na.rm = TRUE)) %>% dplyr::select(-Person)
  SS_p <- n_i*sum((persons - univ_mean)^2)

  jumps <- data.small %>% dplyr::group_by(Trial) %>% dplyr::summarize(mean(Measure, na.rm = TRUE)) %>% dplyr::select(-Trial)
  SS_i <- n_p*sum((jumps - univ_mean)^2)

  total <- data.small %>% dplyr::select(Measure)
  SS_total <- sum((total - univ_mean)^2)
  SS_pi <- SS_total - SS_p - SS_i

  df_p <- n_p - 1
  df_i <- n_i - 1
  df_pi <- df_p*df_i

  M_p <- SS_p/df_p
  M_i <- SS_i/df_i
  M_pi <- SS_pi/df_pi
  alpha <- (1-conf.level)/2

  # Calculating the confidence interval for the G-coef
  L_num <- M_p^2 - stats::qf(1-alpha,df_p,Inf)*M_p*M_pi + (stats::qf(1-alpha,df_p,Inf) - stats::qf(1-alpha,df_p,df_pi))*stats::qf(1-alpha,df_p,df_pi)*(M_pi^2)
  L_denom <- (n_p - 1)*stats::qf(1-alpha,df_p,Inf)*M_p*M_pi + stats::qf(1-alpha,df_p,df_i)*M_p*M_i
  L_p <- L_num/L_denom

  U_num <- M_p^2 - stats::qf(alpha,df_p,Inf)*M_p*M_pi + (stats::qf(alpha,df_p,Inf) - stats::qf(alpha,df_p,df_pi))*stats::qf(alpha,df_p,df_pi)*(M_pi^2)
  U_denom <- (n_p - 1)*stats::qf(alpha,df_p,Inf)*M_p*M_pi + stats::qf(alpha,df_p,df_i)*M_p*M_i
  U_p <- U_num/U_denom

  L_rat <- (n_p*L_p)/(n_p*L_p + n_i)
  U_rat <- (n_p*U_p)/(n_p*U_p + n_i)

  lower.bound <- (n*L_rat)/(1 + (n - 1)*L_rat)
  upper.bound <- (n*U_rat)/(1 + (n - 1)*U_rat)

  # Adding the lower and upper bounds to the final data frame
  final_df[1,2] <- round(lower.bound, as.integer(rounded))
  colnames(final_df)[2] <- "Lower Bound"
  final_df[1,3] <- round(upper.bound, as.integer(rounded))
  colnames(final_df)[3] <- "Upper Bound"

  return(final_df)
}
