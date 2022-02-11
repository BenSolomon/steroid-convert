require(tidyverse)

# Converts between mg/kg and mg/m2 dosing based on bsa
# Exports a list with elements bsa, mgkg, mgm2
steroid_unit_conv <- function(wt, ht, dose, unit){
  bsa <- 0.016667 * (wt^0.5) * (ht^0.5)
  if (unit == "mgkg"){
    mass <- dose*wt
    mgkg <- dose
    mgm2 <- mass/bsa
  } else if (unit == "mgm2") {
    mass <- dose*bsa
    mgkg <- dose/wt
    mgm2 <- dose
  } else {
    error("Invalid unit")
  }
  return(list("bsa"=bsa, "mgkg" = mgkg, "mgm2" = mgm2))
}
# steroid_unit_conv(wt = 18.7, ht = 89, dose = 1, unit = "mgm2")


# List containing steroid conversion tables based on 
# glucocorticoid or mineralocorticoid potency
list_conv <- list(
  "cort" = tribble(~ drug, ~ rel_potent,
                   "hct", 1,
                   "pred", 4,
                   "meth", 5,
                   "dex", 25) %>%
    mutate(rel_dose = 1 / rel_potent),
  
  "mineral" = tribble(~ drug, ~ rel_potent,
                      "hct", 1,
                      "pred", 0.8,
                      "meth", 0.5,
                      "dex", 0) %>%
    mutate(rel_dose = 1 / rel_potent)
)

# Converts a steroid drug dose to its equivalent
# hydrocortisone gluco/mineralocorticoid dose
hct_equiv <- function(dose, drug, effect){
  df_conv <- list_conv[[effect]]
  tibble(drug = drug, dose = dose) %>% 
    left_join(df_conv, by = "drug") %>% 
    mutate(hct_dose = dose/rel_dose) %>% 
    pull(hct_dose)
}
# hct_equiv(dose = 1, drug = "dex", effect = "cort")

# Creates table displaying relative potency and equivalent
# dosages of different steroids to the one used as input
steroid_drug_conv <- function(dose, drug, effect){
  df_conv <- list_conv[[effect]]
  hct_dose <- hct_equiv(dose, drug, effect)
  df_conv %>% 
    mutate(equiv_dose = hct_dose * rel_dose) %>% 
    select(-rel_dose)
}
# steroid_drug_conv(1, "dex", effect = "cort")


# Creates table displaying equivalent mg/kg and mg/m2 dosing
# Requires height in cm and weight in kg
# Uses Mosteller formula
steroid_conv_table <- function(wt, ht, dose, drug, unit, effect){
  steroid_drug_conv(dose, drug, effect) %>% 
    mutate(output = map(equiv_dose, function(x){
      out <- steroid_unit_conv(wt = wt, ht = ht, dose = x, unit = unit)
      data.frame(mgkg = out$mgkg, mgm2 = out$mgm2)
    })) %>% 
    unnest(output) %>% 
    select(-equiv_dose) %>% 
    mutate_at(c("mgkg", "mgm2"), round, 3)
}
# steroid_conv_table(wt=18.7, ht=89, dose=10, type="hct", unit="mgm2", effect = "mineral")