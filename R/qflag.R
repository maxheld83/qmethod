#flags Q sorts automatically according to the given loadings matrix
qflag <- function(loa = loa, nstat, threshold = "0.05", allow.confounded = FALSE) {
  # Input validation ===========================================================
  thresholds.available <- c(
    "none",  # all will be flagged TRUE
    "0.05",  # aka FLAGGING CRITERIA 1) qsorts which factor loading is higher than the threshold for pval >0.95
    "0.01"  # Q-Sorts which factor loading is higher than the threshold for p-value < 0.05
  )
  assert_that(is.string(threshold))
  assert_that(threshold %in% thresholds.available)
  assert_that(is.flag(allow.confounded))
  if(threshold != "0.05" | allow.confounded) {  # warn if things are non-standard
    message("You have chosen options that are not standard Q methodology.")
  }

  # calculate number of Q sorts and number of statements
  nqsorts <- nrow(loa)

  # set up empty flagged df
  flagged <- loa

  # implementing threshold
  thold <- switch(EXPR = threshold,
    "none" = NA,  # NA makes sense because NA is also logical
    "0.05" = 1.96/sqrt(nstat),  # this is the accepted truncated variant https://en.wikipedia.org/wiki/1.96,
    "0.01" = 2.58/sqrt(nstat)
  )
  if (threshold == "none") {
    flagged[ , ] <- TRUE  # assign all true, that's what no flagging is
  } else {
    flagged <- abs(loa) > thold
  }
  confounds <- rowSums(flagged) > 1

  # deal with confounded q-sorts (if any)
  if(any(confounds, na.rm = TRUE)) {
    if(allow.confounded) {  # here: confounding is allowed
      message(sum(confounds), "/", nqsorts, " Q-sorts are confounded and are flagged on more than one factor.")
    } else {# here: confounding is disallowed
      # here comes fka FLAGGING CRITERIA 2) qsorts which square loading is higher than the sum of square loadings of the same q-sort in all other factors
      highest <- flagged  # take same format as flagged
      highest[,] <- FALSE  # make them all false, preliminary
      for (i in 1:nrow(highest)) {
        highest[i, which.max((loa^2)[i, ])] <- TRUE
      }
      flagged <- flagged & highest
      message(sum(confounds), " confounded Q-sorts were resolved by flagging only on the factor with the highest squared loading for that Q-sort.")
    }
  }

  # test for noflags
  noflag <- rowSums(flagged) == 0
  if(any(noflag, na.rm = TRUE)) {
    warning(sum(noflag), " Q-sorts were not flagged on any factor, and will be excluded from the following analysis.")
  }

  # take care of special case of no flagging, allowed confounding
  if(threshold == "none" & allow.confounded) {
    message("No flagging was done, all Q-sorts will be included in the following analysis.")
  }

  return(flagged)
}
