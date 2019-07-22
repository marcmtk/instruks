egfr <- function(x) {
  assert_that(x >= 0 & x < 200)
  structure(list(egfr = x), class = "prescription_element")
}

weight <- function(x) {
  assert_that(is.count(x) & x >= 35 & x < 300)
  structure(list(weight = x), class = "prescription_element")
}

liver_impaired <- function(x) {
  is.flag(x)
  structure(list(liver_impaired = TRUE), class = "prescription_element")
}

# test_that("Patient qualifiers work", {
#   rp("PTZ") + suspicion("Sepsis")
#   rp("PTZ") + egfr(5) + suspicion("Sepsis")
#   rp("PTZ") + egfr(5) + suspicion("Pseudomonas")
#   rp("PTZ") + egfr(5) + suspicion("Endocarditis")
# })
