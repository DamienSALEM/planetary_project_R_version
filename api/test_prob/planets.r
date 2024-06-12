library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(stringr)

use_planets <- function(data) {
  # Convertir la liste de données en DataFrame
  df_inter <- as.data.frame(data)
  rownames(df_inter) <- df_inter$planete_number
  df_inter$planete_number <- NULL

  # Drop colonnes inutiles
  compact_data <- df_inter %>%
    select(-P_GEO_ALBEDO, -P_DETECTION_MASS, -P_DETECTION_RADIUS, -P_ALT_NAMES, -P_ATMOSPHERE,
           -S_DISC, -S_MAGNETIC_FIELD, -P_TEMP_MEASURED, -P_TPERI, -P_DENSITY, -P_ESCAPE, -P_GRAVITY,
           -P_POTENTIAL, -P_OMEGA, -P_INCLINATION, -P_IMPACT_PARAMETER, -P_HILL_SPHERE, -P_UPDATE, -P_MASS)

  # Supprimer les colonnes contenant 'ERROR'
  error_columns <- compact_data %>%
    select(contains("ERROR")) %>%
    colnames()
  df_cleaned <- compact_data %>%
    select(-all_of(error_columns))

  # Remplir les valeurs manquantes avec la moyenne
  mean_values <- df_cleaned %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    as.list()

  compact_data <- df_cleaned %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean_values[[cur_column()]], .)))

  # Identifier les colonnes catégorielles
  categorical_columns <- compact_data %>%
    select(where(is.character)) %>%
    colnames()

  # Créer un indexeur pour chaque colonne catégorielle
  for (column in categorical_columns) {
    levels <- unique(compact_data[[column]])
    compact_data[[paste0(column, "_indexed")]] <- as.integer(factor(compact_data[[column]], levels = levels))
  }

  # Supprimer les anciennes colonnes catégorielles
  compact_data <- compact_data %>%
    select(-all_of(categorical_columns))

  # Remplir les valeurs manquantes avec 0
  compact_data <- compact_data %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

  # Supprimer les colonnes sélectionnées
  cols_to_remove <- c('S_HZ_OPT_MIN_imputed', 'P_FLUX_MAX', 'P_TEMP_EQUIL_imputed', 'S_NAME_HD_indexed_imputed', 
                      'P_MASS_LIMIT_imputed', 'P_FLUX_MIN', 'S_LOG_G_LIMIT', 'S_SNOW_LINE_imputed', 'P_HABZONE_CON_imputed', 
                      'S_NAME_HIP_indexed_imputed', 'P_TEMP_EQUIL_MAX', 'S_HZ_CON_MIN_imputed', 'P_APASTRON_imputed', 
                      'P_SEMI_MAJOR_AXIS_LIMIT_imputed', 'S_HZ_CON0_MIN_imputed', 'S_TYPE_indexed_imputed', 'S_HZ_CON_MAX', 
                      'S_TIDAL_LOCK_imputed', 'P_INCLINATION_LIMIT_imputed', 'P_PERIASTRON_imputed', 'S_DEC_imputed', 
                      'P_ECCENTRICITY_LIMIT_imputed', 'S_METALLICITY_imputed', 'S_DEC_TXT_indexed', 'S_ABIO_ZONE_imputed', 
                      'S_HZ_CON1_MAX', 'P_DISTANCE_EFF_imputed', 'P_TEMP_SURF_MAX_imputed', 'S_TYPE_TEMP_indexed_imputed', 
                      'S_MAG_imputed', 'S_HZ_OPT_MAX', 'S_CONSTELLATION_ENG_indexed_imputed', 'P_ECCENTRICITY_imputed', 
                      'S_HZ_CON1_MIN', 'S_SNOW_LINE', 'S_METALLICITY_LIMIT_imputed', 'S_RA_TXT_indexed', 'P_TEMP_EQUIL_MIN', 
                      'P_HABZONE_OPT_imputed', 'S_CONSTELLATION_ABR_indexed', 'P_TEMP_EQUIL_MAX_imputed', 'S_AGE_imputed', 
                      'S_HZ_CON0_MAX_imputed', 'P_ESI_imputed', 'S_RA_imputed', 'S_HZ_CON_MIN', 'S_LOG_LUM_imputed', 
                      'S_CONSTELLATION_ENG_indexed', 'P_APASTRON', 'P_FLUX_MAX_imputed', 'P_TEMP_SURF_MAX', 'P_DISTANCE', 
                      'S_RA_TXT_indexed_imputed', 'S_HZ_CON0_MIN', 'P_YEAR_imputed', 'P_TYPE_indexed_imputed', 
                      'S_LUMINOSITY_imputed', 'P_PERIOD_imputed', 'S_TEMPERATURE_imputed', 'P_HABITABLE_imputed', 
                      'S_RA_STR_indexed_imputed', 'S_TEMPERATURE_LIMIT_imputed', 'S_CONSTELLATION_indexed_imputed', 
                      'S_HZ_CON0_MAX', 'P_DISCOVERY_FACILITY_indexed_imputed', 'S_HZ_CON1_MIN_imputed', 'S_LOG_G_LIMIT_imputed', 
                      'P_RADIUS_LIMIT_imputed', 'S_LOG_G_imputed', 'S_AGE_LIMIT_imputed', 'P_NAME_indexed_imputed', 
                      'S_RADIUS_imputed', 'P_DETECTION_indexed_imputed', 'P_TEMP_SURF_MIN_imputed', 'P_TYPE_TEMP_indexed_imputed', 
                      'P_PERIOD_LIMIT_imputed', 'P_RADIUS_imputed', 'P_TEMP_EQUIL_MIN_imputed', 'P_MASS_ORIGIN_indexed_imputed', 
                      'S_HZ_CON_MAX_imputed', 'P_DISTANCE_EFF', 'S_DEC_TXT_indexed_imputed', 'S_NAME_indexed_imputed', 
                      'S_CONSTELLATION_ABR_indexed_imputed', 'P_FLUX_MIN_imputed', 'P_PERIASTRON', 'P_TEMP_SURF_imputed', 
                      'S_DEC_STR_indexed_imputed', 'S_MASS_imputed', 'S_HZ_OPT_MAX_imputed', 'P_FLUX_imputed', 
                      'P_SEMI_MAJOR_AXIS_imputed', 'P_DISTANCE_imputed', 'S_DISTANCE_imputed', 'S_HZ_CON1_MAX_imputed', 
                      'P_TEMP_SURF_MIN')
  compact_data <- compact_data %>%
    select(-all_of(cols_to_remove))

  return(compact_data)
}
