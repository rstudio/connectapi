context("utils")

test_that("safequery handles values correctly", {
  pref <- "prefixed"
  nullval = NULL
  expect_identical(safe_query(nullval, pref), "")
  
  oneval <- "blah"
  expect_identical(safe_query(oneval, pref), paste0(pref, oneval))
  
  moreval <- c("blah", "blah2")
  expect_identical(safe_query(moreval, pref), paste0(pref, paste(moreval, collapse = "|")))
  
  morenull <- c(NULL, NULL)
  expect_identical(safe_query(morenull, pref, "|"), "")
})
