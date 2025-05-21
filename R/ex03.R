r <- function(num) {
  checkmate::assertNumeric(num, len = 1, any.missing = FALSE)
  bias <- 1023
  bits <- numToBits(num) |> as.integer()

  S <- ifelse(bits[[64]] == 0, "+", "-")
  E <- rev(bits[53:63])
  M <- rev(bits[1:52])
  
  # special case: +/- Inf
  if (all(E == 1) & all(M == 0)) {
    return(paste0(S, "Inf"))
  }
  
  # special case: NaN
  if (all(E == 1) & any(M == 1)) {
    return("NaN")
  }
  
  # special case: +/- zero
  if (all(E == 0) & all (M == 0)) {
    return(print(sprintf("%s 0 * %s", S, f)))
  }
  
  # rest of cases
  if (all(E == 0)) {
    # denormalized
    E10 <- -1022
    f <- paste0(c("0.", M), collapse = "")
  } else {
    # normalized
    E10 <- sum(rev(E)*2^(seq(0, 10))) - bias
    f <- paste0(c("1.", M), collapse = "")
  }
  
  # return message
  print(sprintf("%s 2^%d * %s", S, E10, f))
}

# testcases
r(1.125)
r(-1.125)
r(22)
r(0)
r(-0)
r(8e-323)
