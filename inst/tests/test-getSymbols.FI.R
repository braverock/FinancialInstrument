context("getSymbols.FI")

from <- as.Date('2012-01-01')
to <- from + 9
BID <- xts(1:10, as.Date(from:to))
tmpdir <- file.path(tempdir(), "BID")
saveSymbols.days("BID", tmpdir, extension="RData")
rm(BID)
# make sure getSymbols.FI works if the directory has the same name as the Symbol
test_that("getSymbols.FI base_dir=Symbol", {
  expect_identical(NROW(getSymbols("BID", dir=tmpdir, 
                                   extension="RData", auto.assign=FALSE, 
                                   from=from, to=to, days_to_omit=NULL,
                                   verbose=FALSE)), 10L)
})
