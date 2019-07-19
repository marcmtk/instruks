library(assertthat)
library(testthat)
library(tibble)
library(dplyr)
library(tidyr)
library(glue)
library(forcats)

`%notin%` <- Negate(`%in%`)

######################
### Rp. part
######################

drug_full_names <- tribble(
  ~drug, ~drug_full,
  "PTZ", "Piperacillin/Tazobactam",
  "Clari", "Clarithromycin",
  "PeniG", "Benzylpenicillin"
)

drug_doses <- tribble(
  ~egfr_range, ~weight_range, ~liver_impairment, ~drug, ~dose, ~admin_form, ~indication,
  "50-200", "35-80" , "Any", "PTZ"  , "4g x 3"    , "iv", "Infection",
  "10-50" , "35-80" , "Any", "PTZ"  , "4g x 2"    , "iv", "Infection", 
  "00-10" , "35-80" , "Any", "PTZ"  , "4g x 1"    , "iv", "Infection",
  "00-10" , "35-80" , "Any", "PTZ"  , "4g x 1"    , "iv", "Sepsis / Pseudomonas",
  "50-200", "35-80" , "Any", "PTZ"  , "4g x 4"    , "iv", "Sepsis / Pseudomonas",
  "10-50" , "35-80" , "Any", "PTZ"  , "4g x 2"    , "iv", "Sepsis / Pseudomonas",
  "30-200", "35-130", "Any", "Clari", "500 mg x 2", "iv / po", "Infection",
  "50-200", "35-100", "Any", "PeniG", "2 MIE x 4", "iv", "Infection / Sepsis",
  "50-200", "35-100", "Any", "PeniG", "1 MIE x 4", "iv", "Pneumonia"
) %>% 
  mutate(liver_impairment = if_else(liver_impairment == "Any", "TRUE, FALSE", liver_impairment)) %>% 
  separate_rows(liver_impairment, convert = TRUE) %>% 
  separate_rows(indication) %>% 
  tidyr::separate(egfr_range, c("egfr_low", "egfr_high"), convert = TRUE) %>% 
  tidyr::separate(weight_range, c("weight_low", "weight_high"), convert = TRUE)

"+.prescription" <- function(prescription_obj, new_element) {
  if (class(new_element) == "prescription_element") {
    assert_that(class(prescription_obj) == "prescription")
    assert_that(class(new_element) == "prescription_element")
    assert_that(length(new_element) == 1)
    assert_that(names(new_element) %in% c("suspicion", "exposure", "egfr", "weight", "liver_impairment"))
    assert_that(names(new_element) %notin% names(prescription_obj))
    
    structure(c(prescription_obj, new_element), class = "prescription")
  } else if(class(new_element) == "prescription") {
    structure(list(prescription_obj, new_element), class = "prescription_list")
  }
}

suspicion <- function(x) structure(list(suspicion = x), class = "prescription_element")
egfr <- function(x) structure(list(egfr = x), class = "prescription_element")
weight <- function(x) structure(list(weight = x), class = "prescription_element")
liver_impaired <- function(x) structure(list(liver_impaired = TRUE), class = "prescription_element")


rp("PTZ") + suspicion("Sepsis")
rp("PTZ") + egfr(5) + suspicion("Sepsis")
rp("PTZ") + egfr(5) + suspicion("Pseudomonas")
rp("PTZ") + egfr(5) + suspicion("Endocarditis")

print.prescription <- function(x) { 
  defaults <- list(
    suspicion = "Infection",
    egfr = 100,
    weight = 80,
    liver_impaired = FALSE
  )
  prescription_args <- c("drug", "suspicion", "egfr", "weight", "liver_impaired")
  missing_args <- prescription_args[prescription_args %notin% names(x)]
  fully_defined_prescription <<- structure(c(x, defaults[missing_args]), class = "prescription")
  text <- resolve_prescription(fully_defined_prescription)
  cat(text)
  invisible(text)
}

rp <- function(drug) {
  is.string(drug)
  is.string(suspicion)
  structure(list(drug = drug), class = "prescription")
}

resolve_prescription <- function(obj) {
  assert_that(class(obj) == "prescription")
  is.string(obj$drug)
  is.count(obj$egfr); assert_that(obj$egfr >= 0 & obj$egfr < 200)
  is.count(obj$weight); assert_that(obj$weight >= 35 & obj$weight < 300)
  is.flag(obj$liver_impaired)
  is.string(obj$suspicion); assert_that(obj$suspicion %in% drug_doses$indication)
  
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

expect_error(rp())
expect_is(rp("PTZ"), "prescription")
test_that("PTZ is dosed correctly in a variety of situations", {
  expect_equal(rp("PTZ"), "Rp. iv. Piperacillin/Tazobactam 4g x 3 for Infection")
  expect_equal(rp("PTZ", egfr = 40),   "Rp. iv. Piperacillin/Tazobactam 4g x 2 for Infection")
  expect_equal(rp("PTZ", egfr = 5),    "Rp. iv. Piperacillin/Tazobactam 4g x 1 for Infection")
  expect_equal(rp("PTZ", egfr = 50),   "Rp. iv. Piperacillin/Tazobactam 4g x 3 for Infection")
  expect_equal(rp("PTZ", weight = 80), "Rp. iv. Piperacillin/Tazobactam 4g x 3 for Infection")
  expect_equal(rp("PTZ", suspicion = "Pseudomonas"), "Rp. iv. Piperacillin/Tazobactam 4g x 4 for Pseudomonas")
})

#test_that("High dose PTZ is correct", rp("PTZ") + suspect_microbe("Pseudomonas aeruginosa") == "Rp. iv. Piperacillin/Tazobactam 4g x 4") # Accepterer at den failer indtil videre. https://github.com/tidyverse/ggplot2/blob/5e8699d9d1463faaa4b489923bc4ef7c51f0afab/R/plot-construction.r
#test_that("High dose PTZ is correct", rp("PTZ") + suspect_disease("Sepsis") == "Rp. iv. Piperacillin/Tazobactam 4g x 4")

######################
### Treatment part
######################

disease_shorthands <- tribble(
  ~disease, ~qualifier, ~qualifier_type, ~alias,
  "Pneumonia", "Community acquired", "setting",  "CAP",
  "Pneumonia", "Hospital acquired", "setting", "HAP",
  "Pneumonia", "Ventilator associated", "setting", "VAP",
  "Pneumonia", "Aspiration", "setting", "Aspiration pneumonia",
  "Pneumonia", TRUE, "atypical_suspicion", "Atypical pneumonia"
)

translate_disease_shorthand <- function(x) {
  filter(disease_shorthands, alias == x) %>% 
    spread(qualifier_type, qualifier, convert = TRUE) %>% 
    select(-alias)
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

translate_disease_shorthand("CAP")
translate_disease_shorthand("Atypical pneumonia")
class(translate_disease_shorthand("CAP"))
translate_disease_shorthand("CAP") %>% left_join(treatments_var3)
translate_disease_shorthand("CAP") %>% fully_define_pneumonia()
translate_disease_shorthand("Aspiration pneumonia") %>% fully_define_pneumonia() %>% left_join(treatments_var3)
translate_disease_shorthand("HAP") %>% fully_define_pneumonia() %>% left_join(treatments_var3)

treatments <- tribble(
  ~disease, ~qualifier, ~severity, ~drug,
  "Pneumonia", "default", "mild", "PeniG"#,
  # "Pneumonia", "default", "moderate", "PeniG",
  # "Pneumonia", "default", "severe", "PTZ + Clari",
  # "Pneumonia", "atypical", "Any", "Clari"
)

treatments_var2 <- tribble(
  ~disease, ~drug, ~qualifiers,
  "Pneumonia", "PeniG", "Mild+Community, Moderate+Community"
) %>% 
  separate_rows(qualifiers, sep = ", ?")

treatments_var3 <- tribble(
  ~ disease,  ~ setting,  ~ severity,  ~ atypical_suspicion,  ~ cave_penicillin, ~ drug_regimen, #CAVE penicillin is 
  "Pneumonia", "Community acquired", "Mild", FALSE, FALSE, "PeniG",
  "Pneumonia", "Community acquired", "Mild", FALSE, TRUE, "Clari",
  "Pneumonia", "Community acquired", "Mild", TRUE, FALSE, "Clari",
  "Pneumonia", "Community acquired", "Mild", TRUE, TRUE, "Clari", 
  "Pneumonia", "Community acquired", "Moderate", FALSE, FALSE, "PeniG + Clari",
  "Pneumonia", "Community acquired", "Moderate", FALSE, TRUE, "Moxi",
  "Pneumonia", "Community acquired", "Moderate", TRUE, FALSE, "PeniG + Clari",
  "Pneumonia", "Community acquired", "Moderate", TRUE, TRUE, "Moxi",
  "Pneumonia", "Community acquired", "Severe", FALSE, FALSE, "PTZ + Clari",
  "Pneumonia", "Community acquired", "Severe", FALSE, TRUE, "Mero + Clari",
  "Pneumonia", "Community acquired", "Severe", TRUE, FALSE, "PTZ + Clari",
  "Pneumonia", "Community acquired", "Severe", TRUE, TRUE, "Mero + Clari",
  "Pneumonia", "Hospital acquired", "Mild", FALSE, FALSE, "",
  "Pneumonia", "Hospital acquired", "Mild", FALSE, TRUE, "",
  "Pneumonia", "Hospital acquired", "Mild", TRUE, FALSE, "",
  "Pneumonia", "Hospital acquired", "Mild", TRUE, TRUE, "",
  "Pneumonia", "Hospital acquired", "Moderate", FALSE, FALSE, "",
  "Pneumonia", "Hospital acquired", "Moderate", FALSE, TRUE, "",
  "Pneumonia", "Hospital acquired", "Moderate", TRUE, FALSE, "",
  "Pneumonia", "Hospital acquired", "Moderate", TRUE, TRUE, "",
  "Pneumonia", "Hospital acquired", "Severe", FALSE, FALSE, "",
  "Pneumonia", "Hospital acquired", "Severe", FALSE, TRUE, "",
  "Pneumonia", "Hospital acquired", "Severe", TRUE, FALSE, "",
  "Pneumonia", "Hospital acquired", "Severe", TRUE, TRUE, "",
  "Pneumonia", "Ventilator associated", "Mild", FALSE, FALSE, "",
  "Pneumonia", "Ventilator associated", "Mild", FALSE, TRUE, "",
  "Pneumonia", "Ventilator associated", "Mild", TRUE, FALSE, "",
  "Pneumonia", "Ventilator associated", "Mild", TRUE, TRUE, "",
  "Pneumonia", "Ventilator associated", "Moderate", FALSE, FALSE, "",
  "Pneumonia", "Ventilator associated", "Moderate", FALSE, TRUE, "",
  "Pneumonia", "Ventilator associated", "Moderate", TRUE, FALSE, "",
  "Pneumonia", "Ventilator associated", "Moderate", TRUE, TRUE, "",
  "Pneumonia", "Ventilator associated", "Severe", FALSE, FALSE, "",
  "Pneumonia", "Ventilator associated", "Severe", FALSE, TRUE, "",
  "Pneumonia", "Ventilator associated", "Severe", TRUE, FALSE, "",
  "Pneumonia", "Ventilator associated", "Severe", TRUE, TRUE, "",
  "Pneumonia", "Aspiration", "Mild", FALSE, FALSE, "No AB",
  "Pneumonia", "Aspiration", "Mild", FALSE, TRUE, "No AB",
  "Pneumonia", "Aspiration", "Mild", TRUE, FALSE, "No AB",
  "Pneumonia", "Aspiration", "Mild", TRUE, TRUE, "No AB",
  "Pneumonia", "Aspiration", "Moderate", FALSE, FALSE, "No AB",
  "Pneumonia", "Aspiration", "Moderate", FALSE, TRUE, "No AB",
  "Pneumonia", "Aspiration", "Moderate", TRUE, FALSE, "No AB",
  "Pneumonia", "Aspiration", "Moderate", TRUE, TRUE, "No AB",
  "Pneumonia", "Aspiration", "Severe", FALSE, FALSE, "Cefur + Metro",
  "Pneumonia", "Aspiration", "Severe", FALSE, TRUE, "Cefur + Metro"#,
  #"Pneumonia", "Aspiration", "Severe", TRUE, FALSE, "", # Not actually a thing
  #"Pneumonia", "Aspiration", "Severe", TRUE, TRUE, ""   # Not actually a thing
)
expect_equal(filter(treatments_var3, setting == "Community", severity == "Mild", atypical_suspicion == TRUE, cave_penicillin == FALSE)$drug_regimen,
             "Clari")


treat <- function(disease_shorthand) {
  is.string(disease_shorthand)
  
  structure(list(disease = disease_shorthand), class = "treatment")
}
expect_equal(treat("CAP")$disease, "CAP")
expect_s3_class(treat("CAP"), "treatment")

exposure <- function(x) {
  expect(is.string(x), "The exposure should be a string")
  structure(list(exposure = x), class = "disease_qualifier")
}
expect_equal(exposure("Aircondition")$exposure, "Aircondition")
expect_s3_class(exposure("Aircondition"), "disease_qualifier")

score <- function(x) {
  expect(is.count(x) | are_equal(x, 0), "Score should be a single whole number 0 or larger")
  structure(list(score = x #, score_type = "something"
                 ), class = "disease_qualifier")
}

"+.treatment" <- function(treatment, disease_qualifier) {
  assert_that(class(treatment) == "treatment")
  assert_that(class(disease_qualifier) == "disease_qualifier")
  assert_that(length(disease_qualifier) == 1)
  assert_that(names(disease_qualifier) %in% c("exposure", "severity", "score"))
  assert_that(names(disease_qualifier) %notin% names(treatment))
  
  structure(c(treatment, disease_qualifier), class = "treatment")
}
treat("CAP") + exposure("Aircondition") 
expect_error(treat("CAP") + exposure("Aircondition") + exposure("Birds"))
treat("CAP") + score(5)

expect_equal(treat("CAP")$text, "Rp. iv. benzylpenicillin 1 MIE x 4")

treatment_aliases %>% 
  filter(alias == "CAP") %>% 
  glue_data("Treatment for {disease} with qualifiers '{qualifier}' is")

specified_disease <- #structure(
  list(
  disease = "Pneumonia",
  qualifiers = c("Community acquired", "Aircondition", "Cave penicillin"),
  severity = "mild")#,
  #class = "disease"
)

expand.grid(disease = "Pneumonia", 
            setting = fct_inorder(c("Community", "Hospital", "Ventilator", "Aspiration")),
            severity = fct_inorder(c("Mild", "Moderate", "Severe")),
            atypical_suspicion = c(FALSE, TRUE),
            cave_penicillin = c(FALSE, TRUE)) %>% 
  arrange(setting, severity, atypical_suspicion, cave_penicillin) %>% View
