#' @title Limpiar los datos de la base de la Fundación Cardio Infantil
#' @export
clean_filmarray_data <- function(filmarray_data) {
  data_clean <- filmarray_data
  names(data_clean) <-
    epitrix::clean_labels(names(data_clean))
  data_clean <- clean_filmarray_age(data_clean)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  col_epiweek <- config::get(file = config_path,
                             "filmarray_data")$epiweek$col_valid
  index_col <- which(names(data_clean) %in% col_epiweek)
  if (length(index_col) > 0) {
    data_clean[[col_epiweek]] <- as.numeric(data_clean[[col_epiweek]])
  }
  return(data_clean)
}

#' @title Estandarizar las edades de los datos de la base de datos de la
#' Fundación Cardio Infantil
#' @export
clean_filmarray_age <- function(filmarray_data,
                                col_age = "edad") {
  data_age_clean <- filmarray_data
  data_age_clean[[col_age]] <- tolower(data_age_clean[[col_age]])
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_clean_age_groups <-
    config::get(file = config_path, "filmarray_data")$age_groups$cols_clean
  col_age_groups <- config::get(file = config_path,
                                "filmarray_data")$age_groups$col_valid
  index_col <- which(cols_clean_age_groups %in% names(data_age_clean))
  if (length(index_col) > 0) {
    col_clean <- cols_clean_age_groups[index_col]
    names(data_age_clean)[names(data_age_clean) == col_clean] <- col_age_groups
    data_age_clean[[col_age_groups]] <-
      tolower(data_age_clean[[col_age_groups]])
  }
  if (col_age %in% colnames(data_age_clean)) {
    patterns <- c(" año", " mes", " dia", " día")
    space_indexes <- NULL
    for (pattern in patterns) {
      aux <- which(stringr::str_detect(data_age_clean[[col_age]],
                                       stringr::fixed(pattern)))
      if (length(aux) > 0) {
        space_indexes <-
          c(space_indexes, aux)
      }
    }
    if (length(space_indexes) != nrow(data_age_clean)) {
      total_indexes <- seq_len(nrow(data_age_clean))
      missing_indexes <- setdiff(total_indexes, space_indexes)
      data_age_clean[[col_age]][missing_indexes] <-
        sapply(data_age_clean[[col_age]][missing_indexes],
               add_character, char = " ")
    }
    data_age_clean[[col_age_groups]][data_age_clean[[col_age_groups]]
                                     == "60 y mas"] <- "60 y más"
  }
  return(data_age_clean)
}

#' @title Limpiar los datos relacionados a Sars-Cov-2
#' @export
clean_sars_data <- function(report_data) {
  names(report_data) <-
    epitrix::clean_labels(names(report_data), sep = "")
  report_data$resultadonuevocoronavirussarscov2vegeneral <-
    epitrix::clean_labels(
      report_data$resultadonuevocoronavirussarscov2vegeneral)
  report_data$rangodeedadvegeneral <-
    epitrix::clean_labels(report_data$rangodeedadvegeneral)
  report_data$virusdetectadosvegeneral <-
    epitrix::clean_labels(report_data$virusdetectadosvegeneral)
  return(report_data)
}

#' @title Limpiar los datos de la base de Otros Virus
#' @export
clean_data_other_viruses <- function(report_data) {
  names(report_data) <- epitrix::clean_labels(names(report_data), sep = "")
  report_data$resultadonuevocoronavirussarscov2vegeneral <-
    epitrix::clean_labels(
      report_data$resultadonuevocoronavirussarscov2vegeneral)
  report_data$rangodeedadvegeneral <- epitrix::clean_labels(
    report_data$rangodeedadvegeneral)
  report_data$clasificacionvegeneral <- epitrix::clean_labels(
    report_data$clasificacionvegeneral)
  report_data$inmunofluorescenciarealizadavegeneral <- epitrix::clean_labels(
    report_data$inmunofluorescenciarealizadavegeneral)
  report_data$eventovegeneral <- epitrix::clean_labels(
    report_data$eventovegeneral)
  report_data$virusdetectadosvegeneral <- epitrix::clean_labels(
    report_data$virusdetectadosvegeneral)
  report_data$rangodeedadvegeneral <- epitrix::clean_labels(
    report_data$rangodeedadvegeneral)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_clean_epiweek <- config::get(file = config_path,
                                    "other_viruses")$epiweek$cols_clean
  col_epiweek <- config::get(file = config_path,
                             "other_viruses")$epiweek$col_valid
  index_col <- which(cols_clean_epiweek %in% names(report_data))
  if (length(index_col) > 0) {
    col_clean <- cols_clean_epiweek[index_col]
    names(report_data)[names(report_data) == col_clean] <- col_epiweek
    report_data[[col_epiweek]] <- as.numeric(report_data[[col_epiweek]])
  }
  report_data$influenzaaporrtpcrvegeneral <- epitrix::clean_labels(
    report_data$influenzaaporrtpcrvegeneral)
  report_data$influenzabporrtpcrvegeneral <- epitrix::clean_labels(
    report_data$influenzabporrtpcrvegeneral)
  report_data$virussincitialrespiratoriovsrporrtpcrvegeneral <-
    epitrix::clean_labels(
      report_data$virussincitialrespiratoriovsrporrtpcrvegeneral)
  report_data$adenovirusadvporrtpcrvegeneral <-
    epitrix::clean_labels(
      report_data$adenovirusadvporrtpcrvegeneral)
  report_data <- clean_sars_data(report_data = report_data)
  return(report_data)
}

#' @title Limpiar los datos de la base de Tosferina
#' @export
clean_tosferina_data <- function(report_data) {
  names(report_data) <-
    epitrix::clean_labels(names(report_data))
  col_names <- names(report_data)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_clean_age_groups <-
    config::get(file = config_path, "tosferina_data")$age_groups$cols_clean
  col_age_groups <- config::get(file = config_path,
                                "tosferina_data")$age_groups$col_valid
  index_col <- which(cols_clean_age_groups %in% col_names)
  if (length(index_col) > 0) {
    col_clean <- cols_clean_age_groups[index_col]
    names(report_data)[names(report_data) == col_clean] <- col_age_groups
  }
  cols_clean_result <-
    config::get(file = config_path,
                "tosferina_data")$result_interpretation$cols_clean
  col_result <-
    config::get(file = config_path,
                "tosferina_data")$result_interpretation$col_valid
  index_col <- which(cols_clean_result %in% col_names)
  if (length(index_col) > 0) {
    col_clean <- cols_clean_result[index_col]
    names(report_data)[names(report_data) == col_clean] <- col_result
    report_data[[col_result]] <-
      epitrix::clean_labels(report_data[[col_result]])
  }
  col_gender <-
    config::get(file = config_path,
                "tosferina_data")$gender$col_valid
  if (col_gender %in% col_names) {
    report_data[[col_gender]] <-
      epitrix::clean_labels(report_data[[col_gender]])
  }
  return(report_data)
}



















#' @title Limpiar espacios en los nombres de las columnas
#'
#' @description
#' Reemplaza los espacios y caracteres especiales en los nombres de las columnas 
#' por guiones bajos, asegurando una nomenclatura estandarizada.
#'
#' @param dataset Un dataset con nombres de columna a limpiar.
#' @return Un dataset con nombres de columna estandarizados.
#' @export
clean_colnames_spaces <- function(df) {
  colnames(df) <- epitrix::clean_labels(colnames(df))
  return(df)
}

#' @title Eliminar sufijos numéricos en los nombres de las columnas
#'
#' @description
#' Remueve los sufijos numéricos en los nombres de las columnas que siguen el 
#' formato `...1`, `...2`, `...3`, común en datos importados desde archivos CSV o Excel.
#'
#' @param dataset Un dataset con nombres de columna que pueden contener sufijos numéricos.
#' @return Un dataset con nombres de columna sin sufijos numéricos.
#' @export
clean_colnames_suffixes <- function(df) {
  colnames(df) <- gsub("\\.\\.\\.[0-9]+$", "", colnames(df))
  return(df)
}


#' @title Rellenar valores faltantes en una columna
#'
#' @description
#' Usa `tidyr::fill()` para rellenar los valores faltantes en la columna especificada,
#' propagando los valores hacia abajo.
#'
#' @param dataset Un dataset que contiene la columna a rellenar.
#' @param column_name Nombre de la columna a rellenar (como variable sin comillas).
#' @return Un dataset con los valores de la columna completados.
#' @export
fill_down_column <- function(dataset, column_name) {
  dataset <- dataset %>%
    tidyr::fill({{ column_name }}, .direction = "down")
  
  # Devolver el data frame limpio
  return(dataset)
}

#' @title Limpiar datos históricos epidemiológicos
#'
#' @description
#' Aplica varias transformaciones al dataset de datos históricos:
#' - Remueve sufijos en nombres de columnas (`clean_colnames_suffixes`).
#' - Estandariza los nombres de las columnas (`janitor::clean_names`).
#' - Rellena valores faltantes en las columnas `ano` y `periodo_epidemiologico` (`fill_down_column`).
#'
#' @param dataset Un dataset con datos históricos sin procesar.
#' @return Un dataset limpio y listo para análisis.
#' @export
clean_historic_data <- function(dataset) {
  dataset <- dataset %>%
    clean_colnames_suffixes() %>%
    janitor::clean_names() %>%  
    fill_down_column("ano") %>%
    fill_down_column("periodo_epidemiologico")
  
  return(dataset)
  
}
