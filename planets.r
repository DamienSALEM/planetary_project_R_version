library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(stringr)

use_planets <- function(data) {

  # Convertir la liste de données en DataFrame
  df_inter <- as.data.frame(t(data))
  rownames(df_inter) <- df_inter$planete_number
  df_inter$planete_number <- NULL

  # Supprimer les colonnes contenant 'ERROR'
  error_columns <- df_inter %>%
    select(contains("ERROR")) %>%
    colnames()
  df_cleaned <- df_inter %>%
    select(-all_of(error_columns))

  # Remplir les valeurs manquantes avec la moyenne (ou 0 si une seule ligne)
  if (nrow(df_cleaned) > 1) {
    mean_values <- df_cleaned %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      as.list()
    df_inter <- df_cleaned %>%
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean_values[[cur_column()]], .)))
  } else {
    df_inter <- df_cleaned %>%
      mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  }

  # Identifier les colonnes catégorielles
  categorical_columns <- df_inter %>%
    select(where(is.character)) %>%
    colnames()

  # Créer un indexeur pour chaque colonne catégorielle
  for (column in categorical_columns) {
    levels <- unique(df_inter[[column]])
    df_inter[[paste0(column, "_indexed")]] <- as.integer(factor(df_inter[[column]], levels = levels))
  }

  # Supprimer les anciennes colonnes catégorielles
  df_inter <- df_inter %>%
    select(-all_of(categorical_columns))

  # Remplir les valeurs manquantes avec 0 pour toutes les colonnes
  df_inter <- df_inter %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
  return(df_inter)
}
