## code to prepare `DATASET` dataset goes here

cancer_types = c(
  "All subtypes" = "all cancer sites",
  "Bladder" = "bladder",
  "Brain and other nervous system" = "brain & ons",
  "Breast (female)" = "breast (female)",
  "Breast (female in situ)" = "breast (female in situ)",
  "Cervix" = "cervix",
  "Childhood (ages <15, all subtypes)" = "childhood (ages <15, all sites)",
  "Childhood (ages <20, all subtypes)" = "childhood (ages <20, all sites)",
  "Colon and rectum" = "colon & rectum",
  "Esophagus" = "esophagus",
  "Kidney and renal pelvis" = "kidney & renal pelvis",
  "Leukemia" = "leukemia",
  "Liver and bile duct" = "liver & bile duct",
  "Lung and bronchus" = "lung & bronchus",
  "Melanoma of the skin" = "melanoma of the skin",
  "Non-hodgkin lymphoma" = "non-hodgkin lymphoma",
  "Oral cavity and pharynx" = "oral cavity & pharynx",
  "Ovary" = "ovary",
  "Pancreas" = "pancreas",
  "Prostate" = "prostate",
  "Stomach" = "stomach",
  "Thyroid" = "thyroid",
  "Uterus (corpus and not otherwise specified)" = "uterus (corpus & uterus, nos)"
)

races = c(
  "All races/ethnicities (including Hispanic)" =  "All Races (includes Hispanic)",
  "White (non-Hispanic)",
  "Black (non-Hispanic)",
  "American Indian / Alaska Native (non-Hispanic)",
  "Asian / Pacific Islander (non-Hispanic)",
  "Hispanic (Any Race)"
)

sexes = c(
  "Any sex" = "both sexes",
  "Male" = "males",
  "Female" = "females"
)


ages = c(
  "All ages" = "all ages",
  "Ages <50" = "ages <50",
  "Ages 50+" = "ages 50+",
  "Ages <65" = "ages <65",
  "Ages 65+" = "ages 65+",
  "Ages <15" = "ages <15",
  "Ages <20" = "ages <20"
)

stages = c(
  "All stages" = "all stages",
  "Late stage (regional & distant)" = "late stage (regional & distant)"
)

years = c(
  "Latest 5 year average" = "latest 5 year average"
)

usethis::use_data(cancer_types, races, sexes, ages, stages, years,
                  overwrite = TRUE, internal = TRUE)

