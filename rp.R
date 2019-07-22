rp <- function(drug) {
  UseMethod("rp")
}

rp.character <- function(drug) {
  assert_that(length(drug) == 1L)
  if(stringr::str_detect(drug, "\\+")) {
    drugs <- stringr::str_split(drug, "[^[:alnum:]]+", simplify = TRUE)
    map(drugs, rp)
  } else {
    is.string(drug)
    defaults <- list(
      suspicion = "Infection",
      egfr = 100,
      weight = 80,
      liver_impaired = FALSE
    )
    structure(c(drug = drug, defaults), class = "prescription")
  }
}

rp.treatment <- function(t) {
  rpp <- rp(t$drug_regimen)
  t$rp <- list(rpp)
}

resolve_prescription <- function(obj) {
  assert_that(class(obj) == "prescription")
  assert_that(is.string(obj$drug))
  assert_that(is.string(obj$suspicion))
  assert_that(obj$suspicion %in% drug_doses$indication)

  drug_dose <- filter(drug_doses,
                      drug == obj$drug,
                      egfr_low <= obj$egfr, obj$egfr < egfr_high,
                      weight_low < obj$weight, obj$weight <= weight_high,
                      liver_impairment == obj$liver_impaired,
                      indication == obj$suspicion) %>%
    left_join(drug_full_names, by = "drug")
  assert_that(is.data.frame(drug_dose))
  assert_that(nrow(drug_dose) == 1)

  as.character(glue_data(drug_dose, "Rp. {admin_form}. {drug_full} {dose} for {indication}"))
}

print.prescription <- function(x) {
  text <- resolve_prescription(x)
  cat(text)
  cat("\n")
  invisible(text)
}

"+.prescription" <- function(prescription_obj, new_element) {
  if (class(new_element) == "prescription_element") {
    assert_that(class(prescription_obj) == "prescription")
    assert_that(class(new_element) == "prescription_element")
    assert_that(length(new_element) == 1)
    assert_that(names(new_element) %in% c("suspicion", "exposure", "egfr", "weight", "liver_impairment"))

    prescription_obj[names(new_element)] <- new_element

    structure(prescription_obj, class = "prescription")
  } else if(class(new_element) == "prescription") {
    structure(list(prescription_obj, new_element), class = "prescription_list")
  }
}

# expect_error(rp())
# expect_is(rp("PTZ"), "prescription")
# test_that("PTZ is dosed correctly in a variety of situations", {
#   expect_equal(rp("PTZ"), "Rp. iv. Piperacillin/Tazobactam 4g x 3 for Infection")
#   expect_equal(rp("PTZ") + egfr(40),   "Rp. iv. Piperacillin/Tazobactam 4g x 2 for Infection")
#   expect_equal(rp("PTZ") + egfr(5),    "Rp. iv. Piperacillin/Tazobactam 4g x 1 for Infection")
#   expect_equal(rp("PTZ") + egfr(50),   "Rp. iv. Piperacillin/Tazobactam 4g x 3 for Infection")
#   expect_equal(rp("PTZ") + weight(80), "Rp. iv. Piperacillin/Tazobactam 4g x 3 for Infection")
#   expect_equal(rp("PTZ") + suspicion("Pseudomonas"), "Rp. iv. Piperacillin/Tazobactam 4g x 4 for Pseudomonas")
# })
