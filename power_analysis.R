# load packages
library(tidyverse)
library(pwr)
library(lme4)
library(simr)
library(readxl)
library(formattable)
library(sf)


### load and clean data

# ANO 5000 - 20 000 dictionary
ano_dictionary <- read_excel("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/ano_5000_20000_dictionary.xlsx")


# Spatial files

# ANO_all file
ANO_all <- read_rds("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/spatial_files/ANO_all.RDS")


# read in bioclimatic regions
bioclim_regions <- st_read("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/spatial_files/bioclimaticRegions.gpkg")|> 
  st_as_sf() |> 
  st_transform(crs = st_crs(ANO_all))

# ellenberg-type values data - select 'original' layer
results_ano <- read_rds("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/spatial_files/results_ANO.RDS")$original

# add geometry to the results file
st_geometry(results_ano) <- st_geometry(ANO_all)


# read in regional geometry file
reg <- st_read("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/wetlands/data/spatial_files/regions.shp", quiet = T)  |> 
  st_as_sf() |> 
  st_transform(crs = st_crs(ANO_all))

# change region names to something R-friendly and merge region with Norway spatial data
#reg$region
reg$region <- c("Northern_Norway", "Central_Norway", "Eastern_Norway", "Western_Norway", "Southern_Norway")



# join region info to results_ANO
results_ano_raw <- st_join(results_ano, reg, left = TRUE) |> 
  # join bioclimatic regions to results_ANO
  st_join(bioclim_regions, left = TRUE) |> 
  st_drop_geometry() |> 
  tibble() |> 

  # remove description from mapping name variables
  mutate(kartleggingsenhet_1m2 = str_remove(kartleggingsenhet_1m2, " .*"),
         kartleggingsenhet_250m2 = str_remove(kartleggingsenhet_250m2, " .*")) |> 
  
  # assign mapping name from 1m^2 to 250m^2 when only present at 1m^2 scale
  tidylog::mutate(kartleggingsenhet_250m2 = if_else(is.na(kartleggingsenhet_250m2), kartleggingsenhet_1m2, kartleggingsenhet_250m2)) |> 
  
  pivot_longer(cols = c("karplanter_feltsjikt", "Light1", "Moist1", "pH1", "Nitrogen1", "moser_dekning"), names_to = "response_variable_names", values_to = "response_variable_values") |> #"fremmedarter_total_dekning", 
  select(globalid, ano_flate_id, ano_punkt_id, kartleggingsenhet_1m2, kartleggingsenhet_250m2, response_variable_names, response_variable_values, region, BCregion) |> 
  # change site and point ids to factors
  mutate(ano_flate_id = as.factor(ano_flate_id),
         ano_punkt_id = as.factor(ano_punkt_id)) |> 
  mutate(response_variable_names = str_remove(response_variable_names, "1"))




# how many sites have fewer than 18 points?  ---> 38 sites. This is normal.
results_ano_raw |> group_by(ano_flate_id) |> 
  tidylog::summarise(n = n_distinct(ano_punkt_id)) |> 
  filter(n < 18)



geo_ano <- results_ano_raw |> 
  # stack the resolutions and analysis type
  #pivot_longer(cols = c("kartleggingsenhet_1m2":"kartleggingsenhet_250m2"), names_to = "analysis_type", values_to = "kartleggingsenhet") |>
  #mutate(#resolution = if_else(grepl("20000", scale), 20000, 5000),
  #       analysis_type = if_else(grepl("1m2", analysis_type), "1m2", "250m2")) |> 
  tidylog::filter(!is.na(kartleggingsenhet_1m2)) |> 
  
  # separate out main and secondary veg types
  mutate(hovedtype = str_extract(kartleggingsenhet_1m2, "^[^-]+"),
         secondary_kartleggingsenhet = str_extract(kartleggingsenhet_1m2, "(?<=-).*"))

# total number of ANO sites
n_sites <- geo_ano |> 
  summarise(n = n_distinct(ano_flate_id)) |> 
  pull(n)


geo_ano_agglo <- geo_ano |> 
  # create agglomerate category for rare nature types
  mutate(secondary_kartleggingsenhet_agglo = case_when(
    (hovedtype == "V1" & secondary_kartleggingsenhet %in% c("C-3", "C-4", "C-7", "C-8")) | 
      (hovedtype == "V2" & secondary_kartleggingsenhet %in% c("C-2", "C-3")) | 
      (hovedtype == "V3") | 
      (hovedtype == "V6" & secondary_kartleggingsenhet %in% c("C-2", "C-4", "C-6", "C-8"))  | 
      (hovedtype == "V8" & secondary_kartleggingsenhet %in% c("C-2", "C-3"))  |
      (hovedtype == "V9" & secondary_kartleggingsenhet %in% c("C-2", "C-3")) ~ "rare",
    TRUE ~ "common"
  ),
        kartleggingsenhet_agglo = paste(hovedtype, secondary_kartleggingsenhet_agglo, sep = "-"),
        all = paste(hovedtype, "all", sep = "-")
  ) |> 
  # stack kartleggingsenheter
  tidylog::pivot_longer(c("all", kartleggingsenhet_agglo), names_to = "grp", values_to = "grouping") |> 
  # filter out NAs
  tidylog::filter(!is.na(hovedtype)) |>  # !is.na(response_variable_values),
  select(-grp)



#### detectability of wetland types in the whole ANO database

all_hovedtype <- unique(geo_ano_agglo$grouping)

all_site_veg <- expand_grid(
  ano_flate_id = unique(geo_ano_agglo$ano_flate_id),
  grouping = all_hovedtype
)

# mark which combinations were actually detected
site_veg_long <- all_site_veg  |> 
  tidylog::left_join(
    geo_ano_agglo  |> 
      distinct(ano_flate_id, grouping)  |>  
      mutate(detected = TRUE),
    by = c("ano_flate_id", "grouping")
  ) %>%
  mutate(detected = replace_na(detected, FALSE))

search_pattern = c("V1", "V2", "V3", "V6", "V8", "V9")

# what is the likelihood of detection of the vatmark hovedtyper if you randomly visited *any* ANO site (at the national, regional or bioclimatic scale)
site_detectability <- site_veg_long  |> 
  group_by(grouping, detected) %>%
  tally()  |> 
  pivot_wider(
    names_from = detected,
    values_from = n,
    names_prefix = "detected_",
    values_fill = 0
  )  |> 
  mutate(
    total = detected_TRUE + detected_FALSE,
    detectability = (detected_TRUE / total) * 100
  )  |> 
  ungroup() |> 
  tidylog::filter(str_detect(grouping, paste(search_pattern, collapse = "|")))




# filter out unused vegetation types and NAs
geo_ano_vat <- tidylog::filter(geo_ano_agglo, hovedtype %in% c("V1", "V2", "V3", "V6", "V8", "V9")) |> 
  #filter out the duplicates (where all of the kartleggingsenheter are rare or common)
  filter(!grepl("common", grouping)) |> 
  tidylog::filter(!grouping %in% c("V3-all", "V6-all", "V9-all"))


  
geo_ano_general <- geo_ano_vat |> 
  group_by(region, grouping) |>  
  mutate(n_sites = n_distinct(ano_flate_id), n_points = n_distinct(ano_punkt_id)) |> 
  group_by(region, grouping, ano_flate_id, n_sites, n_points) |> 
  summarise(n_points_in_site = n_distinct(ano_punkt_id)) |> 
  group_by(region, grouping, n_sites, n_points) |> 
  tidylog::summarise(mean_n_plots_site = mean(n_points_in_site),
            median_n_plots_site = median(n_points_in_site), 
            max_n_plots_site = max(n_points_in_site), 
            percent_plots_site_greater_than_1 = ((sum(n_points_in_site > 1)/sum(n_points_in_site))*100), .groups = 'drop')



# extract total number of observations
geo_ano_vat <- geo_ano_vat |>
  group_by(response_variable_names) |> 
  mutate(total_obs = n_distinct(ano_punkt_id)) |> 
  ungroup() |> 
  mutate(response_variable_names = str_replace_all(response_variable_names, "_", "_"))




### analyses

# power function that handles NAs
safe_pwr <- possibly(
  function(f2, power) {
    pwr.f2.test(u = 1, v = NULL, f2 = f2, power = power, sig.level = 0.05) # we leave v as NULL because this is what we want to calculate
  },
  otherwise = NULL
)


# prepare data for analysis at national, regional and bioclimatic scales
geo_ano_analysis <- geo_ano_vat |>
  group_by(BCregion, grouping, response_variable_names, total_obs) |> 
  mutate(sd_dat = sd(response_variable_values, na.rm = TRUE)) |> 
  group_by(grouping, response_variable_names, total_obs) |> 
  tidylog::summarise(sd_dat_mean = mean(sd_dat, na.rm = TRUE),
                     n_obs = n_distinct(ano_punkt_id),
                     obs_threshold = n_obs/total_obs,
                     range_vals = max(response_variable_values, na.rm = TRUE) - min(response_variable_values, na.rm = TRUE),
                     sd_threshold = range_vals*0.15,
                     mean_control = mean(response_variable_values, na.rm = TRUE),
                     .groups = "drop") |>
  mutate(delta_1 = mean_control*0.01,
         delta_5 = mean_control*0.05,
         effect_1 = delta_1/sd_dat_mean,
         effect_5 = delta_5/sd_dat_mean,
         f2_1 = (effect_1^2)/(1 - effect_1^2), 
         f2_5 = (effect_5^2)/(1 - effect_5^2), 
         delta_flag = if_else(delta_1 < 0.05, "low", "ok")) |> 
  
  # pivot to long format and transform delta to numeric
  select(grouping, response_variable_names, f2_1, f2_5, delta_flag, sd_dat_mean, n_obs, obs_threshold, sd_threshold, total_obs) |> 
  tidylog::pivot_longer(
    cols = starts_with("f2_"),
    names_to = "delta_level",
    names_prefix = "f2_",
    values_to = "f2"
  ) |> 
  mutate(delta_level = as.numeric(delta_level)) |> 
  
  #filter out NAs
  filter(!is.na(f2),
         f2 > 0) |>
  
  # expand to cross with power levels
  crossing(power = c(0.6, 0.8))


# stage 1: national scale power analyses
geo_ano_results <- geo_ano_analysis |>
  #filter(grouping_value == "national") |> 
  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
         n_plots = round(n_plots, digits = 0)) #|> 
  #select(-test_result, -grouping_value) |> 
  # filter for response variables measured at the correct scales
  #tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light", "Moist", "Nitrogen", "pH", "richness"))|(analysis_type == "250m2" & response_variable_names %in% c("vedplanter_total_dekning"))) #, "busker_dekning" removing busker dekning for the moment



## stage 2: geographical regions
#geo_ano_results_regional <- geo_ano_analysis |>
#  filter(grepl("Norway", grouping_value)) |> 
#  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
#         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
#         n_plots = round(n_plots, digits = 0)) |> 
#  select(-test_result) |> 
#  rename("Region" = grouping_value) |> 
#  # filter for response variables measured at the correct scales
#  tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light", "Moist", "Nitrogen", "pH", "richness"))|(analysis_type == "250m2" & #response_variable_names %in% c("vedplanter_total_dekning"))) # , "busker_dekning"
#
#
#
## stage 3: Bioclimatic reagions
#geo_ano_results_bioclimatic <- geo_ano_analysis |>
#  filter(!grepl("Norway", grouping_value), grouping_value != "national") |> 
#  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
#         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
#         n_plots = round(n_plots, digits = 0)) |> 
#  select(-test_result) |> 
#  rename("Bioclimatic_region" = grouping_value) |> 
#  # filter for response variables measured at the correct scales
#  tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light", "Moist", "Nitrogen", "pH", "richness"))|(analysis_type == "250m2" & #response_variable_names %in% c("vedplanter_total_dekning"))) # , "busker_dekning"
#






# write code to determine the detectability at regional and bioclimatic scales given the predictions at the national level.






### figures
#geo_ano_results_figures <- geo_ano_results |> 
#  select(region, hovedtype, resolution, analysis_type, response_variable_names, n_plots, power, delta_level) |> 
#  tidylog::left_join(geo_ano_general) |> 
#  mutate(feasibility = ifelse(n_plots < n_points, "feasible", "not feasible")) 


