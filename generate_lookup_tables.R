###
# Drugs and doses
###

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
  "10-50" , "35-100", "Any", "PeniG", "2 MIE x 3", "iv", "Infection / Sepsis",
  "00-10" , "35-100", "FALSE", "PeniG", "1 MIE x 2", "iv", "Infection / Sepsis",
  "50-200", "35-100", "Any", "PeniG", "1 MIE x 4", "iv", "Pneumonia",
  "10-50" , "35-100", "Any", "PeniG", "1 MIE x 3", "iv", "Pneumonia",
  "00-10" , "35-100", "FALSE", "PeniG", "1 MIE x 2", "iv", "Pneumonia",
  "50-200", "100-200", "Any", "PeniG", "2 MIE x 4", "iv", "Pneumonia",
  "00-30" , "35-100", "TRUE", "PeniG", "0.5 MIE x 3", "iv", "Infection / Sepsis / Pneumonia"
) %>%
  mutate(liver_impairment = if_else(liver_impairment == "Any", "TRUE, FALSE", liver_impairment)) %>%
  separate_rows(liver_impairment, convert = TRUE) %>%
  separate_rows(indication) %>%
  tidyr::separate(egfr_range, c("egfr_low", "egfr_high"), convert = TRUE) %>%
  tidyr::separate(weight_range, c("weight_low", "weight_high"), convert = TRUE)

###
# Treatments
###

disease_shorthands <- tribble(
  ~disease, ~qualifier, ~qualifier_type, ~alias,
  "Pneumonia", "Community acquired", "setting",  "CAP",
  "Pneumonia", "Hospital acquired", "setting", "HAP",
  "Pneumonia", "Ventilator associated", "setting", "VAP",
  "Pneumonia", "Aspiration", "setting", "Aspiration pneumonia",
  "Pneumonia", TRUE, "atypical_suspicion", "Atypical pneumonia"
)

treatments <- tribble(
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
