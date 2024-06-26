dconf <- function(data, col.scores, n, conf.level = 0.95, rounded = 3) {
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

  n <- as.integer(n)
  final_df <- data.frame(0,0,0)
  colnames(final_df)[1] <- paste0("G-coef")
  row.names(final_df)[1] <- paste0(col.scores, ", n = ", n)
  comps$dvar <- c(comps[1,2], comps[2,2]/n, comps[3,2]/n)
  G <- comps[1, ncol(comps)]/sum(comps$dvar)
  final_df[1,1] <- round(G, as.integer(rounded))

  n_p <- length(unique(data.small$Person))
  n_i <- length(unique(data.small$Trial))
  conf.level <- as.numeric(conf.level)

  ANOVA <- summary(stats::aov(Measure ~ as.factor(Person)*as.factor(Trial), data = data.small))
  M_p <- ANOVA[[1]][[3]][[1]]
  M_i <- ANOVA[[1]][[3]][[2]]
  M_pi <- ANOVA[[1]][[3]][[3]]
  alpha <- (1-conf.level)/2
  df_p <- n_p - 1
  df_i <- n_i - 1
  df_pi <- df_p*df_i

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

  final_df[1,2] <- round(lower.bound, as.integer(rounded))
  colnames(final_df)[2] <- "Lower Bound"
  final_df[1,3] <- round(upper.bound, as.integer(rounded))
  colnames(final_df)[3] <- "Upper Bound"

  return(final_df)
}
