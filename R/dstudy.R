dstudy2 <- function(data, col.scores, from, to, by, rounded = 3) {
  if (!is.na(as.integer(col.scores)) & col.scores == as.integer(col.scores)) {
    data.small <- data %>%
      select(1:2, as.integer(col.scores))

    col.scores <- colnames(data)[col.scores]
  }
  else {
    data.small <- data %>%
      select(1:2, col.scores)
  }

  colnames(data.small)[3] <- "Measure"
  formula2 <- Measure ~ (1|Person) + (1|Trial)
  comps <- data.frame(gstudy(data = data.small, formula2)$components)
  y <- c("Person", "Trial", "Residual")

  comps <- comps %>%
    slice(match(y, source))

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
