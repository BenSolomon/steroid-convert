require(tidyverse)
library(ggrepel)

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
  } else if (unit == "mg") {
    mass <- dose
    mgkg <- dose/wt
    mgm2 <- mass/bsa
  } else {  
    error("Invalid unit")
  }
  return(list("bsa"=bsa, "mgkg" = mgkg, "mgm2" = mgm2, "mg" = mass))
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
      data.frame(mg = out$mg, mgkg = out$mgkg, mgm2 = out$mgm2)
    })) %>% 
    unnest(output) %>% 
    select(-equiv_dose) %>% 
    mutate_at(c("mg", "mgkg", "mgm2"), round, 3)
}
# steroid_conv_table(wt=18.7, ht=89, dose=10, type="hct", unit="mgm2", effect = "mineral")


# Plots number line with relative steroid dosing
gg_steroid_scale <- function(hct_mgm2_dose, y_adj = 0.2){
  
  df_annotation <- tribble(
    ~dose, ~feature,
    10, "Physiologic",
    25, "Stress dose",
    50, "Extra stress dose",
    100, "Super stress dose"
  )
  
  if (hct_mgm2_dose <= 100) {
    df_numberLine <- tibble(
      dose = seq(0,100,by=5)
    )    
  } else {
    df_numberLine <- tibble(
      dose = seq(0,hct_mgm2_dose,by=5)
    )
  }
  
  df <- df_annotation %>% 
    full_join(df_numberLine, by = "dose")
  
  ggplot(df, aes(x = dose, y = 0, label = feature))+
    geom_point(shape = "|", size = 3) +
    geom_point(data = df %>% drop_na(), size = 2) +
    geom_path()+
    geom_point(aes(x = hct_mgm2_dose), color = "black", size = 4)+
    geom_point(aes(x = hct_mgm2_dose), color = "red", size = 2)+
    ggrepel::geom_label_repel(data = df %>% drop_na(), 
                              aes(label = sprintf("%smg/m2/d", dose)), 
                              nudge_y = -y_adj, hjust = 0.5, 
                              xlim=c(0, Inf), segment.linetype = "dotted") +
    ggrepel::geom_label_repel(nudge_y = y_adj, vjust = 0.5,
                              xlim=c(0, Inf), segment.linetype = "dotted") +
    theme_void() +
    scale_y_continuous(lim = c(-y_adj*1.5,y_adj*1.5)) +
    scale_x_continuous(expand = expansion(mult = c(0.05,0.2)), "Hydrocortisone equivalent") +
    theme(axis.title.x = element_text(face = "bold"))
}

# # VERTICAL VERSION
# gg_steroid_scale <- function(hct_mgm2_dose){
#   
#   df_annotation <- tribble(
#     ~dose, ~feature,
#     10, "Physiologic",
#     25, "Stress dose",
#     50, "Extra stress dose",
#     100, "Super stress dose"
#   )
#   
#   if (hct_mgm2_dose <= 100) {
#     df_numberLine <- tibble(
#       dose = seq(0,100,by=5)
#     )    
#   } else {
#     df_numberLine <- tibble(
#       dose = seq(0,hct_mgm2_dose,by=5)
#     )
#   }
#   
#   df <- df_annotation %>% 
#     full_join(df_numberLine, by = "dose")
#   
#   ggplot(df, aes(y = dose, x = 0, label = feature))+
#     geom_point(shape = "-", size = 5) +
#     geom_point(data = df %>% drop_na()) +
#     geom_path()+
#     geom_point(aes(y = hct_mgm2_dose), color = "black", size = 4)+
#     geom_point(aes(y = hct_mgm2_dose), color = "red", size = 2)+
#     geom_text(data = df %>% drop_na(), aes(label = sprintf("%smg/m2/d", dose)), nudge_x = -0.05, hjust =1) +
#     geom_text(nudge_x = 0.05, hjust = 0) +
#     theme_void() +
#     scale_x_continuous(lim = c(-1,1)) +
#     theme(axis.title.y = element_text(angle = 90, face = "bold"),
#           plot.margin = margin(2, 2, 2, 2,unit = "pt"))+
#     labs(y="Equivalent hydrocortisone dose")
# }