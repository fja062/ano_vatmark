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
  #select(globalid, ano_flate_id:ano_punkt_id, CC1:region) |> 
  #select(-contains("2")) |> 
  
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
  pivot_longer(cols = c("busker_dekning", "vedplanter_total_dekning", "Light1", "Moist1", "pH1", "Nitrogen1", "richness"), names_to = "response_variable_names", values_to = "response_variable_values") |> #"fremmedarter_total_dekning", 
  select(globalid, ano_flate_id, ano_punkt_id, kartleggingsenhet_1m2, kartleggingsenhet_1m2_20000, kartleggingsenhet_250m2, kartleggingsenhet_250m2_20000, response_variable_names, response_variable_values, region, BCregion) |> 
  # change site and point ids to factors
  mutate(ano_flate_id = as.factor(ano_flate_id),
         ano_punkt_id = as.factor(ano_punkt_id)) |> 
  mutate(response_variable_names = str_remove(response_variable_names, "1"))




# how many sites have fewer than 18 points?  ---> 38 sites. This is normal.
results_ano_raw |> group_by(ano_flate_id) |> 
  summarise(n = n_distinct(ano_punkt_id)) |> 
  filter(n < 18)



geo_ano <- results_ano_raw |> 
  # stack the resolutions and analysis type
  pivot_longer(cols = c("kartleggingsenhet_1m2":"kartleggingsenhet_250m2_20000"), names_to = "scale", values_to = "kartleggingsenhet") |>
  mutate(resolution = if_else(grepl("20000", scale), 20000, 5000),
         analysis_type = if_else(grepl("1m2", scale), "1m2", "250m2")) |> 
  select(-scale) |> 
  tidylog::filter(!is.na(kartleggingsenhet)) |> 
  # separate out main and secondary veg types
  mutate(hovedtype = str_extract(kartleggingsenhet, "^[^-]+"),
         secondary_kartleggingsenhet = str_extract(kartleggingsenhet, "(?<=-).*"))

# total number of ANO sites
n_sites <- geo_ano |> 
  summarise(n = n_distinct(ano_flate_id)) |> 
  pull(n)

geo_ano |> 
  group_by(hovedtype) |> 
  summarise(n = n_distinct(ano_flate_id)/n_sites) |> 
  ggplot(aes(x = hovedtype, y = n)) + 
  geom_col()


geo_ano_agglo <- geo_ano |> 
  # create agglomerate category for rare nature types
  group_by(resolution, analysis_type) |> 
  mutate(secondary_kartleggingsenhet_agglo = case_when(
    (hovedtype == "V1" & secondary_kartleggingsenhet %in% c("C-3", "C-4", "C-7", "C-8")) | 
      (hovedtype == "V2" & secondary_kartleggingsenhet %in% c("C-2", "C-3")) | 
      (hovedtype == "V3") | 
      (hovedtype == "V6" & secondary_kartleggingsenhet %in% c("C-2", "C-4", "C-6", "C-8"))  | 
      (hovedtype == "V8" & secondary_kartleggingsenhet %in% c("C-2", "C-3"))  |
      (hovedtype == "V9" & secondary_kartleggingsenhet %in% c("C-2", "C-3")) ~ "rare",
    TRUE ~ "common"
  ),
        kartleggingsenhet_agglo = paste(hovedtype, secondary_kartleggingsenhet_agglo, sep = "-")
  ) |> 
  ungroup() |> 
  # filter out NAs
  filter(!is.na(response_variable_values),
         !is.na(hovedtype))


# stack kartlegging levels into one column: all, rare, common.
geo_ano_vat_detect <- geo_ano_agglo |> 
  # filter out unused vegetation types and NAs
  tidylog::filter(hovedtype %in% c("V1", "V2", "V3", "V6", "V8", "V9"))


# detectability of wetland types in the whole ANO database
geo_ano_agglo_detect <- geo_ano_agglo #|> 
  #filter(resolution == 5000, analysis_type == "1m2") <--- should we filter here or not?

# expand dataset to get all possible combinations
all_site_veg <- expand_grid(
  ano_flate_id = unique(geo_ano_agglo_detect$ano_flate_id),
  hovedtype = unique(geo_ano_agglo_detect$hovedtype))

# mark which combinations were actually detected
site_veg_long <- all_site_veg %>%
  tidylog::left_join(
    geo_ano_agglo_detect |> distinct(ano_flate_id, hovedtype) |>  mutate(detected = TRUE),
    by = c("ano_flate_id", "hovedtype")
  )  |> 
  mutate(detected = replace_na(detected, FALSE))


# what is the likelihood of detection of the vatmark hovedtyper if you randomly visited *any* ANO site 
site_detectability <- site_veg_long |> 
  group_by(hovedtype) |> 
  count(detected) |> 
  pivot_wider(names_from = detected, values_from = n) |> 
  mutate(n = sum(`TRUE`, `FALSE`),
         detectability = (`TRUE`/sum(`TRUE`, `FALSE`))*100) |> 
  tidylog::filter(hovedtype %in% c("V1", "V2", "V3", "V6", "V8", "V9"))

geo_ano_agglo |> 
  mutate(n_national_plots = n_distinct(ano_punkt_id)) |> 
  group_by(region) |> 
  mutate(n_region_plots = n_distinct(ano_punkt_id)) |> 
  group_by(BCregion) |> 
  mutate(n_BCregion_plots = n_distinct(ano_punkt_id)) |> 
  group_by(hovedtype) |> 
  mutate(n_national_hovedtype_plots = n_distinct(ano_punkt_id)) |> 
  group_by(region, hovedtype) |> 
  mutate(n_region_hovedtype_plots = n_distinct(ano_punkt_id)) |> 
  group_by(BCregion, hovedtype) |> 
  mutate(n_BCregion_hovedtype_plots = n_distinct(ano_punkt_id)) |> 
  ungroup() |> 
  distinct(hovedtype, region, BCregion, n_national_plots, n_region_plots, n_BCregion_plots, n_national_hovedtype_plots, n_region_hovedtype_plots, n_BCregion_hovedtype_plots) |> 
  mutate(national_detectability = (n_national_hovedtype_plots/n_national_plots)*100,
         regional_detectability = (n_region_hovedtype_plots/n_region_plots)*100,
         bioclimatic_detectability = (n_BCregion_hovedtype_plots/n_BCregion_plots)*100) |> 
  tidylog::filter(hovedtype %in% c("V1", "V2", "V3", "V6", "V8", "V9"))


# detectability of wetland types in the ANO vatmark database
geo_ano_vat_detect |> 
  filter(resolution == 5000, analysis_type == "1m2") |> 
  mutate(n_national_plots = n_distinct(ano_punkt_id)) |> 
  group_by(region) |> 
  mutate(n_region_plots = n_distinct(ano_punkt_id)) |> 
  group_by(BCregion) |> 
  mutate(n_BCregion_plots = n_distinct(ano_punkt_id)) |> 
  group_by(hovedtype) |> 
  mutate(n_national_hovedtype_plots = n_distinct(ano_punkt_id)) |> 
  group_by(region, hovedtype) |> 
  mutate(n_region_hovedtype_plots = n_distinct(ano_punkt_id)) |> 
  group_by(BCregion, hovedtype) |> 
  mutate(n_BCregion_hovedtype_plots = n_distinct(ano_punkt_id)) |> 
  ungroup() |> 
  distinct(hovedtype, region, BCregion, n_national_plots, n_region_plots, n_BCregion_plots, n_national_hovedtype_plots, n_region_hovedtype_plots, n_BCregion_hovedtype_plots) |> 
  mutate(national_detectability = (n_national_hovedtype_plots/n_national_plots)*100,
         regional_detectability = (n_region_hovedtype_plots/n_region_plots)*100,
         bioclimatic_detectability = (n_BCregion_hovedtype_plots/n_BCregion_plots)*100)



geo_ano_vat <- geo_ano_vat_detect |> 
  mutate(kartleggingsenhet_all = paste(hovedtype, "all", sep = "-")) |> 
  tidylog::pivot_longer(c("kartleggingsenhet_all", "kartleggingsenhet_agglo"), names_to = "ddd", values_to = "grouping") |> 
  select(-ddd) |> 
  #filter out the duplicates (where all of the kartleggingsenheter are rare or common)
  tidylog::filter(!grouping %in% c("V3-all", "V6-all", "V9-all"))







  
  
  geo_ano_vat |> 
  group_by(hovedtype, resolution, analysis_type, response_variable_names) |> 
  summarise(count = n())


geo_ano_general <- geo_ano_vat |> 
  group_by(region, hovedtype, resolution, analysis_type) |>  
  mutate(n_sites = n_distinct(ano_flate_id), n_points = n_distinct(ano_punkt_id)) |> 
  group_by(region, hovedtype, resolution, analysis_type, ano_flate_id, n_sites, n_points) |> 
  summarise(n_points_in_site = n_distinct(ano_punkt_id), .groups = 'drop') |> 
  group_by(region, hovedtype, resolution, analysis_type, n_sites, n_points) |> 
  tidylog::summarise(mean_n_plots_site = mean(n_points_in_site),
            median_n_plots_site = median(n_points_in_site), 
            max_n_plots_site = max(n_points_in_site), 
            percent_plots_site_greater_than_1 = ((sum(n_points_in_site > 1)/sum(n_points_in_site))*100), .groups = 'drop')



# extract total number of observations
geo_ano_vat <- geo_ano_vat |>
  group_by(response_variable_names, resolution, analysis_type) |> 
  mutate(total_obs = n_distinct(ano_punkt_id)) |> 
  ungroup() |> 
  mutate(response_variable_names = str_replace_all(response_variable_names, "_", "_")) |> 
  
  # filter out busker_dekning for the moment
  filter(!response_variable_names == "busker_dekning")

### analyses

# power function that handles NAs
safe_pwr <- possibly(
  function(f2, power) {
    pwr.f2.test(u = 1, v = NULL, f2 = f2, power = power, sig.level = 0.05) # we leave v as NULL because this is what we want to calculate
  },
  otherwise = NULL
)


# analyses in three stages:
# stage 1: national scale power analyses
geo_ano_analysis_national <- geo_ano_vat |>
  group_by(grouping, resolution, analysis_type, response_variable_names, total_obs) |> 
  tidylog::summarise(n_obs = n_distinct(ano_punkt_id),
                     obs_threshold = n_obs/total_obs,
                     range_vals = max(response_variable_values) - min(response_variable_values),
                     sd_threshold = range_vals*0.15,
                     mean_control = mean(response_variable_values, na.rm = TRUE),
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
  
  # pivot to long format and transform delta to numeric
  select(grouping, resolution, analysis_type, response_variable_names, f2_1, f2_5, f2_10, sd_dat, n_obs, obs_threshold, sd_threshold, total_obs) |> 
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


geo_ano_results_national <- geo_ano_analysis_national |>
  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
         n_plots = round(n_plots, digits = 0)) |> 
  select(-test_result) |> 
  # filter for response variables measured at the correct scales
  tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light", "Moist", "Nitrogen", "pH", "richness"))|(analysis_type == "250m2" & response_variable_names %in% c("vedplanter_total_dekning"))) #, "busker_dekning" removing busker dekning for the moment




# stage 2: geopolitical reagions
geo_ano_analysis_regional <- geo_ano_vat |>
  group_by(region, grouping, resolution, analysis_type, response_variable_names, total_obs) |> 
  tidylog::summarise(n_obs = n_distinct(ano_punkt_id),
                     obs_threshold = n_obs/total_obs,
                     range_vals = max(response_variable_values) - min(response_variable_values),
                     sd_threshold = range_vals*0.15,
            mean_control = mean(response_variable_values, na.rm = TRUE),
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
  
  # pivot to long format and transform delta to numeric
  select(region, grouping, resolution, analysis_type, response_variable_names, f2_1, f2_5, f2_10, sd_dat, n_obs, obs_threshold, sd_threshold) |> 
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


geo_ano_results_regional <- geo_ano_analysis_regional |>
  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
         n_plots = round(n_plots, digits = 0)) |> 
  select(-test_result) |> 
  # filter for response variables measured at the correct scales
  tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light", "Moist", "Nitrogen", "pH", "richness"))|(analysis_type == "250m2" & response_variable_names %in% c("vedplanter_total_dekning"))) # , "busker_dekning"





# stage 3: Bioclimatic reagions
geo_ano_analysis_bioclimatic <- geo_ano_vat |>
  group_by(BCregion, grouping, resolution, analysis_type, response_variable_names, total_obs) |> 
  tidylog::summarise(n_obs = n_distinct(ano_punkt_id),
                     obs_threshold = n_obs/total_obs,
                     range_vals = max(response_variable_values) - min(response_variable_values),
                     sd_threshold = range_vals*0.15,
                     mean_control = mean(response_variable_values, na.rm = TRUE),
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
  
  # pivot to long format and transform delta to numeric
  select(BCregion, grouping, resolution, analysis_type, response_variable_names, f2_1, f2_5, f2_10, sd_dat, n_obs, obs_threshold, sd_threshold) |> 
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


geo_ano_results_bioclimatic <- geo_ano_analysis_bioclimatic |>
  mutate(test_result = map2(f2, power, ~ safe_pwr(.x, .y)),
         n_plots = map_dbl(test_result, ~ if (is.null(.x)) NA_real_ else .x$u + .x$v + 1),
         n_plots = round(n_plots, digits = 0)) |> 
  select(-test_result) |> 
  # filter for response variables measured at the correct scales
  tidylog::filter((analysis_type == "1m2" & response_variable_names %in% c("Light", "Moist", "Nitrogen", "pH", "richness"))|(analysis_type == "250m2" & response_variable_names %in% c("vedplanter_total_dekning"))) # , "busker_dekning"





# write code to determine the detectability at regional and bioclimatic scales given the predictions at the national level.




### figures
#geo_ano_results_figures <- geo_ano_results |> 
#  select(region, hovedtype, resolution, analysis_type, response_variable_names, n_plots, power, delta_level) |> 
#  tidylog::left_join(geo_ano_general) |> 
#  mutate(feasibility = ifelse(n_plots < n_points, "feasible", "not feasible")) 


