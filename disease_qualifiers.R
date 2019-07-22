suspicion <- function(x) {
  assert_that(is.string(x))
  structure(list(suspicion = x), class = "prescription_element")
}

exposure <- function(x) {
  assert_that(is.string(x), msg = "The exposure should be a string")
  structure(list(exposure = x), class = "disease_qualifier")
}

score <- function(x) {
  assert_that(is.count(x) | are_equal(x, 0), msg = "Score should be a single whole number 0 or larger")
  structure(list(score = x #, score_type = "something"
  ), class = "disease_qualifier")
}

severity <- function(x) {
  assert_that(is.string(x))
  structure(list(severity = x), class = "disease_qualifier")
}

# test_that("Suspicion modifer works", {
#   rp("PTZ") + suspicion("Sepsis")
#   rp("PTZ") + egfr(5) + suspicion("Sepsis")
#   rp("PTZ") + egfr(5) + suspicion("Pseudomonas")
#   rp("PTZ") + egfr(5) + suspicion("Endocarditis")
# })
