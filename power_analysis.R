# load packages
library(tidyverse)
library(pwr)
library(lme4)
library(simr)
library(readxl)
library(formattable)


# set colours
customGreen0 = "#DeF7E9"

customGreen = "#71CA97"



### load and clean data

# ANO 5000 - 20 000 dictionary
ano_dictionary <- read_excel("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/ano_5000_20000_dictionary.xlsx")

# ellenberg-type values data
cwm_ano <- read_rds("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/results_ANO.RDS")

cwm_ano_original <- cwm_ano$original|> 
  select(globalid, ano_flate_id:ano_punkt_id, CC1:richness) |> 
  select(-contains("2"))

# species data
species_ano <- read_csv2("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/ANO_sp.csv")

# create species richness variable
species_richness <- species_ano |> 
  group_by(parentglobalid) |> 
  summarise(species_richness = n_distinct(art_navn),
            plot_cover = sum(art_dekning))



# geo data
geo_ano_raw <- read_csv2("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/ANO_geo.csv")

# check which plots in geo_ano don't have data in geo_ano_original
geo_ano_raw |> 
  # join to ellenberg index data
  tidylog::anti_join(cwm_ano_original, by = join_by(globalid, ano_flate_id, ano_punkt_id)) |> 
  group_by(ano_flate_id) |> 
  summarise(n = n_distinct(ano_punkt_id))


# data preparation
geo_ano_raw <- geo_ano_raw |> 
  # join to ellenberg index data
  tidylog::left_join(cwm_ano_original, by = join_by(globalid, ano_flate_id, ano_punkt_id)) |> 
  # remove description from mapping name variables
  mutate(kartleggingsenhet_1m2 = str_remove(kartleggingsenhet_1m2, " .*"),
         kartleggingsenhet_250m2 = str_remove(kartleggingsenhet_250m2, " .*")) |> 
  # assign mapping name from 1m^2 to 250m^2 when only present at 1m^2 scale
  tidylog::mutate(kartleggingsenhet_250m2 = if_else(is.na(kartleggingsenhet_250m2), kartleggingsenhet_1m2, kartleggingsenhet_250m2)) |> 
  # join 1:20 000 values to the 1:5000 data
  tidylog::left_join(ano_dictionary, by = join_by(kartleggingsenhet_1m2 == kartleggingsenhet_1m2_5000 ))  |> 
  rename(kartleggingsenhet_1m2_20000 = kartleggingsenhet_20000) |> 
  tidylog::left_join(ano_dictionary, by = join_by(kartleggingsenhet_250m2 == kartleggingsenhet_1m2_5000 ))  |> 
  rename(kartleggingsenhet_250m2_20000 = kartleggingsenhet_20000) |> 
  # gather response variables
  pivot_longer(cols = c("fremmedarter_total_dekning", "busker_dekning", "vedplanter_total_dekning", "Light1", "Moist1", "pH1", "Nitrogen1", "richness"), names_to = "response_variable_names", values_to = "response_variable_values") |> 
  select(globalid, ano_flate_id, ano_punkt_id, kartleggingsenhet_1m2, kartleggingsenhet_1m2_20000, kartleggingsenhet_250m2, kartleggingsenhet_250m2_20000, response_variable_names, response_variable_values) |> 
  # filter out fremmedarter_total_dekning for the moment
  filter(!response_variable_names == "fremmedarter_total_dekning") |> 
  # change site and point ids to factors
  mutate(ano_flate_id = as.factor(ano_flate_id),
         ano_punkt_id = as.factor(ano_punkt_id))


### general exploration
# join species richness data to geo data
#geo_ano |> 
#  tidylog::left_join(species_richness, by = c("globalid" = "parentglobalid")) ## FAR TOO MANY SITES ONLY IN GEO DATA!!
#
#
## how many of those that don't match, still have an estimation of plant cover in the geo data
#geo_ano |> tidylog::anti_join(species_richness, by = c("globalid" = "parentglobalid")) |> 
#  filter(!is.na(karplanter_dekning))

# how many sites have fewer than 18 points?  ---> 38 sites. This is normal.
geo_ano_raw |> group_by(ano_flate_id) |> 
  summarise(n = n_distinct(ano_punkt_id)) |> 
  filter(n < 18)

geo_ano <- geo_ano_raw |> 
  pivot_longer(cols = c("kartleggingsenhet_1m2":"kartleggingsenhet_250m2_20000"), names_to = "scale", values_to = "kartleggingsenhet") |>
  mutate(resolution = if_else(grepl("20000", scale), 20000, 5000),
         analysis_type = if_else(grepl("1m2", scale), "1m2", "250m2")) |> 
  select(-scale) |> 
  filter(!is.na(kartleggingsenhet)) |> 
  # separate out main and secondary veg types
  mutate(main_kartleggingsenhet = str_extract(kartleggingsenhet, "^[^-]+"),
         secondary_kartleggingsenhet = str_extract(kartleggingsenhet, "(?<=-).*")) |> 
  # filter out unused vegetation types and NAs
  tidylog::filter(main_kartleggingsenhet %in% c("V1", "V2", "V3", "V6", "V8", "V9"),
         !is.na(response_variable_values),
         !is.na(secondary_kartleggingsenhet)) |> 
  # create agglomerate category for rare nature types
  group_by(resolution, analysis_type) |> 
  mutate(secondary_kartleggingsenhet_agglo = case_when(
    (main_kartleggingsenhet == "V1" & secondary_kartleggingsenhet %in% c("C-3", "C-4", "C-7", "C-8")) | 
      (main_kartleggingsenhet == "V2" & secondary_kartleggingsenhet %in% c("C-2", "C-3")) | 
      (main_kartleggingsenhet == "V3") | 
      (main_kartleggingsenhet == "V6" & secondary_kartleggingsenhet %in% c("C-2", "C-4", "C-6", "C-8"))  | 
      (main_kartleggingsenhet == "V8" & secondary_kartleggingsenhet %in% c("C-2", "C-3"))  |
      (main_kartleggingsenhet == "V9" & secondary_kartleggingsenhet %in% c("C-2", "C-3")) ~ "rare",
    TRUE ~ "common"
  ),
        kartleggingsenhet_agglo = paste(main_kartleggingsenhet, secondary_kartleggingsenhet_agglo, sep = "-")
  )

geo_ano |> 
  group_by(kartleggingsenhet, resolution, analysis_type, response_variable_names) |> 
  summarise(count = n())


geo_ano_general <- geo_ano |> 
  group_by(kartleggingsenhet, resolution, analysis_type) |>  
  mutate(n_sites = n_distinct(ano_flate_id), n_points = n_distinct(ano_punkt_id)) |> 
  group_by(kartleggingsenhet, resolution, analysis_type, ano_flate_id, n_sites, n_points) |> 
  summarise(n_points_in_site = n_distinct(ano_punkt_id), .groups = 'drop') |> 
  group_by(kartleggingsenhet, resolution, analysis_type, n_sites, n_points) |> 
  tidylog::summarise(mean_n_plots_site = mean(n_points_in_site),
            median_n_plots_site = median(n_points_in_site), 
            max_n_plots_site = max(n_points_in_site), 
            percent_plots_site_greater_than_1 = ((sum(n_points_in_site > 1)/sum(n_points_in_site))*100), .groups = 'drop')




### analyses

# Set-up for all vegetation types
# set up the parameters
geo_ano_analysis <- geo_ano |> 
  group_by(kartleggingsenhet, resolution, analysis_type, response_variable_names) |> 
  summarise(mean_control = mean(response_variable_values, na.rm = TRUE),
            sd_dat = sd(response_variable_values, na.rm = TRUE),
            .groups = "drop") |>
  mutate(delta_1 = mean_control*0.01,
         delta_5 = mean_control*0.05,
            delta_10 = mean_control*0.10,
            effect_1 = delta_1/sd_dat,
            effect_5 = delta_5/sd_dat,
            effect_10 = delta_10/sd_dat,
            f2_1 = (effect_1^2)/(1 - effect_1^2), 
            f2_5 = (effect_5^2)/(1 - effect_5^2), 
            f2_10 = (effect_10^2)/(1 - effect_10^2)) |> 
  
  # pivot to long format
  select(kartleggingsenhet, resolution, analysis_type, response_variable_names, f2_1, f2_5, f2_10) |> 
  tidylog::pivot_longer(
    cols = starts_with("f2_"),
    names_to = "delta_level",
    names_prefix = "f2_",
    values_to = "f2"
  ) |> 
  
  #filter out NAs
  filter(!is.na(f2),
         f2 > 0) |>
  
  # expand to cross with power levels
  crossing(power = c(0.6, 0.8))

# power function that handles NAs
safe_pwr <- possibly(
  function(f2, power) {
    pwr.f2.test(u = 1, v = NULL, f2 = f2, power = power, sig.level = 0.05) # we leave v as NULL because this is what we want to calculate
  },
  otherwise = NULL
)

geo_ano_results <- geo_ano_analysis |>
  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
         n_plots = round(n_plots, digits = 0)) |> 
  select(-test_result) |> 
  # filter for response variables measured at the correct scales
  tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light1", "Moist1", "Nitrogen1", "pH1", "richness"))|(analysis_type == "250m2" & response_variable_names %in% c("vedplanter_total_dekning", "busker_dekning")))

# Set-up for agglomerated vegetation types
geo_ano_analysis_agglo <- geo_ano |> 
  group_by(kartleggingsenhet_agglo, resolution, analysis_type, response_variable_names) |> 
  summarise(mean_control = mean(response_variable_values, na.rm = TRUE),
            sd_dat = sd(response_variable_values, na.rm = TRUE),
            .groups = "drop") |>
  mutate(delta_1 = mean_control*0.01,
         delta_5 = mean_control*0.05,
         delta_10 = mean_control*0.10,
         effect_1 = delta_1/sd_dat,
         effect_5 = delta_5/sd_dat,
         effect_10 = delta_10/sd_dat,
         f2_1 = (effect_1^2)/(1 - effect_1^2), 
         f2_5 = (effect_5^2)/(1 - effect_5^2), 
         f2_10 = (effect_10^2)/(1 - effect_10^2)) |> 
  
  # pivot to long format
  select(kartleggingsenhet_agglo, resolution, analysis_type, response_variable_names, f2_1, f2_5, f2_10) |> 
  tidylog::pivot_longer(
    cols = starts_with("f2_"),
    names_to = "delta_level",
    names_prefix = "f2_",
    values_to = "f2"
  ) |> 
  
  #filter out NAs
  filter(!is.na(f2),
         f2 > 0) |>
  
  # expand to cross with power levels
  crossing(power = c(0.6, 0.8))

geo_ano_results_agglo <- geo_ano_analysis_agglo |>
  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
         n_plots = round(n_plots, digits = 0)) |> 
  select(-test_result) |> 
  # filter for response variables measured at the correct scales
  tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light1", "Moist1", "Nitrogen1", "pH1", "richness"))|(analysis_type == "250m2" & response_variable_names %in% c("vedplanter_total_dekning", "busker_dekning")))






## figures
geo_ano_results_figures <- geo_ano_results |> 
  select(kartleggingsenhet, resolution, analysis_type, response_variable_names, n_plots, power, delta_level) |> 
  tidylog::left_join(geo_ano_general) |> 
  mutate(feasibility = ifelse(n_plots < n_points, "feasible", "not feasible")) 


