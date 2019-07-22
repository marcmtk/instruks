treat <- function(disease_shorthand) {
  is.string(disease_shorthand)
  assert_that(disease_shorthand %in% treatments$disease | disease_shorthand %in% disease_shorthands$alias,
              msg = "disease_shorthand not recognized as either shorthand or disease name")
  x <- translate_disease_shorthand(disease_shorthand)
  x <- fully_define_pneumonia(x)
  x <- left_join(x, treatments, by = c("disease", "setting", "severity",
                                       "atypical_suspicion", "cave_penicillin")) %>%
    mutate(rp = map(drug_regimen, rp), rp_text = map_chr(rp, resolve_prescription))
  structure(x, class = c("treatment", "tbl_df", "tbl", "data.frame"))
}

translate_disease_shorthand <- function(x) {
  results <- filter(disease_shorthands, alias == x) %>%
    spread(qualifier_type, qualifier, convert = TRUE) %>%
    select(-alias)
  if(nrow(results) == 0L) results <- tibble(disease = x)
  return(results)
}

fully_define_pneumonia <- function(x) {
  defaults <- list(
    setting = "Community acquired",
    severity = "Mild",
    atypical_suspicion = FALSE,
    cave_penicillin = FALSE
  )
  disease_args <- c("disease", "setting", "severity", "atypical_suspicion", "cave_penicillin")
  missing_args <- disease_args[disease_args %notin% names(x)]
  fully_defined_disease <- as_tibble(c(x, defaults[missing_args])) #structure(c(x, defaults[missing_args]), class = "disease")
}

"+.treatment" <- function(treatment, disease_qualifier) {
  assert_that(any(class(treatment) == "treatment"))
  assert_that(class(disease_qualifier) == "disease_qualifier")
  assert_that(length(disease_qualifier) == 1)
  assert_that(names(disease_qualifier) %in% c("exposure", "severity", "score"))

  treatment[names(disease_qualifier)] <- disease_qualifier

  structure(treatment, class = "treatment")
}

print.treatment <- function(t) {
  x <- glue_data(t, "For {setting} {disease} is:\n {rp_text}")
  cat(x)
  invisible(t)
}

# Tests

# expect_equal(filter(treatments, setting == "Community", severity == "Mild", atypical_suspicion == TRUE, cave_penicillin == FALSE)$drug_regimen,
#              "Clari")
# expect_equal(treat("CAP")$disease, "CAP")
# expect_s3_class(treat("CAP"), "treatment")
#
#
# expect_equal(exposure("Aircondition")$exposure, "Aircondition")
# expect_s3_class(exposure("Aircondition"), "disease_qualifier")
#
# expect_error(treat("CAP") + exposure("Aircondition") + exposure("Birds"))
#
# expect_equal(treat("CAP")$text, "Rp. iv. benzylpenicillin 1 MIE x 4")
