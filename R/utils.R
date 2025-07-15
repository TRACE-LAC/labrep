#' @title Convertir una fila a un encabezado
#' @keywords internal
row_to_header <- function(data, row_num = 1) {
  if (!is.null(data)) {
    names(data) <- as.character(unlist(data[row_num, ]))
    data[-row_num, ]
  }
}

#' @title Generar las categorias de edad
#' @export
generate_age_categories <- function(dataset) {
  # print("holllaa")
  if (!any(names(dataset) == "grupo_edad")) {
    data_ages <- cbind(dataset, grupo_edad = NA)
    data_ages[, ncol(data_ages)] <- sapply(data_ages$edad,
                                           define_age_category)
    return(data_ages)
  } else {
    return(dataset)
  }
}

#' @title Definir las categorias de edad
#' @keywords internal
define_age_category <- function(age) {
  # print("holllaa")
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_conditionals <- config::get(file = config_path,
                                       "age_categories")$conditionals
  category_labels <- config::get(file = config_path,
                                 "age_categories")$categories
  # print(category_labels)
  age_values <- unlist(strsplit(age, " ", fixed = TRUE))
  # print(age_values)
  category <- category_labels[1]
  # print(category)
  if ("años" %in% age_values) {
    i <- 1
    for (conditional in category_conditionals) {
      if (eval(parse(text = conditional))) {
        category <- category_labels[i]
        # print("SOYYY CATEGORIA")
        # print(category)
      }
      i <- i + 1
    }
  }
  return(category)
}

#' @title Agregar caracter
#' @keywords internal
add_character <- function(value, char) {
  init_pos <- regexpr("años|año|mes|meses|día|días|dias", value)[1]
  if (!is.na(init_pos) && init_pos > -1) {
    value <- paste0(substring(value, 1, init_pos - 1),
                    char,
                    substring(value, init_pos))
  }
  return(value)
}

#' @title Obtener el top de virus por grupo de edad
#' @export
get_top_virues_age <- function(dataset,
                               col_age = "grupo_edad",
                               age_groups = c("< 2 AÑOS",
                                              "2 A 4 AÑOS")) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <- config::get(file = config_path,
                                 "age_categories")$labels
  categories <- config::get(file = config_path,
                            "age_categories")$categories
  categories <- epitrix::clean_labels(categories)
  viruses <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_age) %in% age_groups) %>%
    dplyr::arrange(!!dplyr::sym(col_age), "casos")
  viruses[[col_age]] <- epitrix::clean_labels(viruses[[col_age]])
  top_viruses <- viruses %>%
    dplyr::group_by(!!dplyr::sym(col_age)) %>%
    dplyr::top_n(3, wt = .data$casos) %>%
    dplyr::mutate(!!dplyr::sym(paste0(col_age, "_etiqueta")) :=
                    category_labels[match(!!dplyr::sym(col_age),
                                          categories)]) %>%
    dplyr::ungroup()
  top_viruses_ordered <- data.frame()
  text <- ""
  for (age in categories) {
    age_group_rows <- which(top_viruses[[col_age]] == age)
    if (length(age_group_rows) > 0) {
      age_group_values <-
        top_viruses[which(top_viruses[[col_age]] == age), ]
      age_group_values <- age_group_values %>%
        dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
      for (i in seq_len(nrow(age_group_values))) {
        age_group <- age_group_values[i, ]
        if (i < nrow(age_group_values)) {
          text <- paste0(text, " ",
                         age_group$grupo_edad_etiqueta,
                         " se presentan casos de ",
                         age_group$etiqueta,
                         " (",
                         age_group$porcentaje,
                         "%), ")
        } else {
          text <- paste0(text, "y ",
                         age_group$grupo_edad_etiqueta,
                         " se presentan casos de ",
                         age_group$etiqueta,
                         " (",
                         age_group$porcentaje,
                         "%).")
        }
      }
    }
  }
  return(top_viruses_ordered)
}

#' @title Obtener el grupo de edad con mayor número de casos
#' @keywords internal
get_max_age_group <- function(dataset, col_name) {
  age_max <- -1
  age <-
    stringr::str_split(dataset[[col_name]],
                       stringr::fixed("_"))[[1]]
  if (length(age) <= 3) {
    age_max <- as.numeric(age[1])
  } else {
    age_max <- as.numeric(age[3])
    age_max <- age_max + 1
  }
  return(age_max)
}

#' @title Obtener el porcentaje por grupo de edad
#' @export
get_perc_viruses_age <- function(dataset,
                                 col_name = "grupo_edad") {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  vs_age_group_4 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[1:2]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym(col_name)))
  perc_group_4 <-
    round((sum(vs_age_group_4$casos) * 100) / sum(dataset$casos), 1)
  return(perc_group_4)
}

#' @title Obtener el texto del consolidado de los virus por grupo de edad
#' @export
get_cons_viruses_age_text <- function(dataset,
                                      col_name = "grupo_edad",
                                      text_group) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  text_cons_virues <- NULL
  vs_age_group_2 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[1]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  vs_age_group_2 <- vs_age_group_2[1:3, ]
  vs_age_group_4 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[2]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  vs_age_group_4 <- vs_age_group_4[1:3, ]
  text_cons_virues <-
    paste0(text_cons_virues,
           get_text_viruses(dataset = vs_age_group_2,
                            tam = nrow(vs_age_group_2)),
           text_group,
           get_text_viruses(dataset = vs_age_group_4,
                            tam = nrow(vs_age_group_4)))
  return(text_cons_virues)
}

#' @title Obtener el texto de tosferina por grupo de edad
#' @export
get_tosferina_text_sex <- function(dataset, figure) {
  percentage_female <-
    dataset[which(dataset$genero == "Femenino"), ]$porcentaje
  percentage_male <-
    dataset[which(dataset$genero == "Masculino"), ]$porcentaje
  sex_major <- c("femenino", percentage_female)
  sex_less <- c("masculino", percentage_male)
  if (isTRUE(percentage_female < percentage_male)) {
    sex_major <- c("masculino", percentage_male)
    sex_less <- c("femenino", percentage_female)
  }
  text_sex <- paste0(
    "Con respecto al género, ",
    "se observa que el ", sex_major[2], "%", " corresponden al género ",
    sex_major[1], " y el ", sex_less[2], "%",
    " al género ", sex_less[1], "(figura ", figure,
    ")."
  )
  text_values <- list(
    text = text_sex, major = sex_major,
    less = sex_less
  )
  return(text_values)
}

#' @title Obtener el texto de la proporción acumulada con Sars CoV 2
#' @export
get_prop_text <- function(dataset) {
  top_viruses <- dataset %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  top_viruses <- top_viruses[1:3, ]
  text_viruses <- get_text_viruses(dataset = top_viruses,
                                   tam = nrow(top_viruses))
  return(text_viruses)
}

#' @title Obtener el texto de los virus por grupo de edad
#' @export
get_text_viruses <- function(dataset, tam) {
  text_viruses <- NULL
  for (i in seq(1:tam)) {
    virus <- dataset[i, ]
    if (i < tam) {
      text_viruses <- paste0(text_viruses, virus$etiqueta,
                             " (", virus$porcentaje, " %), ")
    } else {
      token <- " y "
      if (startsWith(virus$etiqueta, prefix = "I")) {
        token <- " e "
      }
      text_viruses <- paste0(substr(text_viruses,
                                    1,
                                    nchar(text_viruses) - 2), token,
                             virus$etiqueta, " (", virus$porcentaje, " %)")
    }
  }
  return(text_viruses)
}

#' @title Completar las categorías de edad
#' @export
complete_age_categories <- function(data_grouped,
                                    event_name,
                                    event_label) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  categorie_labels <- config::get(file = config_path,
                                  "age_categories")$age_categories
  for (label in categorie_labels) {
    if (!any(data_grouped == label) || is.na(any(data_grouped == label))) {
      new_row <- data.frame(grupo_edad = label,
                            casos = 0,
                            porcentaje = 0,
                            evento = event_name,
                            etiqueta = event_label)
      data_grouped <- rbind(data_grouped, new_row)
    }
  }
  return(data_grouped)
}

#' @title Obtener los valores de los subtipos de un virus
#' @export
get_subtypes_values <- function(viruses, subtypes) {
  subtypes_values <- NULL
  for (virus in viruses) {
    if (virus$name %in% subtypes) {
      values <- virus$other_viruses$values
      if (any(values != "", na.rm = TRUE)) {
        subtypes_values <- c(subtypes_values, values)
      }
    }
  }
  return(subtypes_values)
}

#' @title Obtener los subtipos de la Influenza
#' @export
get_influenza_viruses <- function(viruses, events) {
  influeza_viruses <- viruses
  remove_index <- NULL
  for (i in seq(1, length(viruses))) {
    virus <- viruses[[i]]
    if (!(virus$name %in% events)) {
      remove_index <- c(remove_index, i)
    }
  }
  influeza_viruses <- influeza_viruses[-remove_index]
  return(influeza_viruses)
}

#' @title Obtener la tabla de casos por semana epidemiológica de Tosferina
#' @export
get_table_epiweek_tosferina <- function(report_data, epiweek) {
  table_data <- data.frame(SE = report_data$semana_epidemiologica,
                           positivos = report_data$porcentaje)
  table_data$SE <- as.numeric(table_data$SE)
  table_data <- table_data %>%
    dplyr::arrange(.data$SE <= as.numeric(epiweek))
  table_data <- table_data %>%
    dplyr::arrange(.data$SE)
  return(table_data)
}

#' @title Añadir las semanas epidemiológicas faltantes
#' @export
add_missing_weeks <- function(dataset, col_epiweek,
                              col_casos = "casos",
                              col_total = "total_casos",
                              col_porcentaje = "porcentaje") {
  max_epiweek <-
    max(as.numeric(dataset[[col_epiweek]]), na.rm = TRUE)
  if (max_epiweek < 53) {
    diff_epiweek <- 53 - max_epiweek
    dataset_aux <- data.frame(semana =
                                  seq(max_epiweek + 1, 53),
                                col_casos = rep(0, diff_epiweek),
                                col_total = rep(0, diff_epiweek),
                                col_porcentaje = rep(0.00, diff_epiweek))
    names(dataset_aux) <- c(col_epiweek, col_casos, col_total, col_porcentaje)
    dataset <- rbind(dataset, dataset_aux)
  }
  return(dataset)
}

#' @title Convertir grupos de edad a columnas
#' @export
convert_age_groups_as_cols <- function(dataset) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  category_labels <- c("etiqueta", category_labels)
  data_groups <- dataset %>%
    dplyr::select(.data$etiqueta, .data$grupo_edad, .data$casos) %>% # Seleccionar columnas relevantes
    tidyr::pivot_wider(
      names_from = grupo_edad, # Columna que se convierte en encabezados
      values_from = casos      # Valores que llenan la tabla
    )
  cols_order <- factor(colnames(data_groups),
                       levels = category_labels)
  data_groups <- data_groups %>%
    dplyr::select(dplyr::all_of(levels(cols_order)))
  return(data_groups)
}

#' @title Remover NaN
#' @export
remove_nan <- function(dataset, col_name) {
  dataset_without_nan <- data.frame()
  na_values <- which(is.na(
    dataset[[col_name]]))
  if (length(na_values) > 0) {
    dataset_without_nan <-
      dataset[-na_values, ]
    return(dataset_without_nan)
  }
  return(dataset)
}

#' @title Obtener el total de casos
#' @export
get_total_cases <- function(data_grouped = NULL,
                            col_name = NULL,
                            join = TRUE) {
  col_total_cases <- 0
  if (!is.null(data_grouped) && !is.null(col_name)) {
    col_total_cases <- data_grouped %>%
      dplyr::group_by(!!dplyr::sym(col_name)) %>%
      dplyr::summarise(total_casos = sum(.data$casos))
    if (join) {
      data_grouped <- data_grouped %>%
        dplyr::left_join(col_total_cases, by = col_name)
      data_grouped$total_casos[is.na(data_grouped$total_casos)] <- 0
      return(data_grouped)
    }
  }
  return(col_total_cases)
}

#' @title Obtener el total de muestras
#' @export
get_total_samples <- function(data_grouped = NULL,
                              report_data = NULL,
                              col_name = NULL,
                              join = TRUE) {
  samples <- data.frame()
  if (!is.null(col_name)) {
    if (!is.null(report_data)) {
      samples <- report_data %>%
        dplyr::group_by(!!dplyr::sym(col_name)) %>%
        summarise(total_muestras = n(), .groups = "drop")
      
    } else if (!is.null(data_grouped)) {
      samples <- data_grouped %>%
        dplyr::group_by(!!dplyr::sym(col_name)) %>%
        dplyr::mutate(total_muestras = sum(unique(.data$total_muestras)))
    }
    if (join && !is.null(data_grouped)) {
      data_grouped <- data_grouped %>%
        dplyr::left_join(samples, by = col_name)
      data_grouped$total_muestras[is.na(data_grouped$total_muestras)] <- 0
      return(data_grouped)
    }
  }
  return(samples)
}

#' @title Adicionar los indicadores como total casos,
#' total muestras y positividad
#' @export
add_indicators <- function(data_grouped,
                           report_data = NULL,
                           col_name = NULL,
                           total_cases = TRUE,
                           total_samples = FALSE,
                           positivity = FALSE,
                           remove_nan = TRUE,
                           join = TRUE) {
  if (total_cases) {
    vals_total_cases <- get_total_cases(data_grouped = data_grouped,
                                        col_name = col_name,
                                        join = join)
    if (join) {
      data_grouped <- vals_total_cases
    }
  }
  if (total_samples) {
    samples <- report_data %>%
      dplyr::group_by(!!dplyr::sym(col_name)) %>%
      summarise(total_muestras = n(), .groups = "drop")
    if (join) {
      data_grouped <- data_grouped %>%
        dplyr::left_join(samples, by = col_name)
      data_grouped$total_muestras[is.na(data_grouped$total_muestras)] <- 0
    }
  }
  if (positivity) {
    data_grouped <- data_grouped %>%
      dplyr::mutate(positividad =
                      round((.data$total_casos / .data$total_muestras) * 100,
                            digits = 2))
    data_grouped$positividad[is.na(data_grouped$positividad)] <- 0
  }
  if (remove_nan) {
    data_grouped <-
      remove_nan(dataset = data_grouped,
                 col_name = col_name)
  }
  return(data_grouped)
}

#' @title Obtener las columnas de un data.frame según los resultados de
#' str_detect y una validación de NANs
#' @export
get_rows_valid_str_detect <- function(cases_virus,
                                      col_name,
                                      values,
                                      is_not = FALSE) {
  if (!all(is.na(cases_virus[[col_name]])) && !all(values == "")) {
    if (is_not) {
      valid_values <- !stringr::str_detect(
        cases_virus[[col_name]],
        paste(values, collapse = "|"))
    } else {
      valid_values <- stringr::str_detect(
        cases_virus[[col_name]],
        paste(values, collapse = "|"))
    }
    if (any(valid_values, na.rm = TRUE)) {
      cases_virus <- cases_virus[which(!is.na(valid_values) & valid_values), ]
    } else if (!is_not) {
      cases_virus <- cases_virus[0, ]
    }
  } else if (!is_not) {
    cases_virus <- cases_virus[0, ]
  }
  return(cases_virus)
}

#' @title Obtener los valores consolidados de los virus que en los datos se
#' encuentran en múltiples columnas, como por ejemplo Parainfluenza y Otros
#' coronavirus
#' @export
get_consolidated_viruses <- function(dataset,
                                     col_name = "grupo_edad",
                                     percentage = TRUE,
                                     total_cases = TRUE) {
  evento <- NULL
  etiqueta <- NULL
  total_casos <- 0
  data_parainfluenza <- data.frame()
  
  if (nrow(dataset) > 1) {
    evento <- dataset[["evento"]][1]
    etiqueta <- dataset[["etiqueta"]][1]
    data_parainfluenza <- dplyr::group_by(dataset, !!dplyr::sym(col_name)) %>%
      dplyr::summarise(casos = sum(.data$casos))
    t_casos <- sum(data_parainfluenza$casos)
    #print(t_casos)
  }
  
  if (percentage) {
    data_parainfluenza <- data_parainfluenza %>% dplyr::mutate(
      porcentaje = round((.data$casos / t_casos) * 100, 1)
    )
  }
  
  data_parainfluenza$evento <- evento
  data_parainfluenza$etiqueta <- etiqueta
  
  if (total_cases) {
    data_parainfluenza$total_casos <- t_casos
  }
  
  return(data_parainfluenza)
}

#' @title Renombrar los valores de una o más filas de un data.frame
#' @export
rename_row <- function(dataset, col_name,
                       original_names,
                       new_name) {
  for (group in original_names) {
    dataset[[col_name]][dataset[[col_name]] == group]  <-
      new_name
  }
  return(dataset)
}

#' @title Completar las semana epidemiológicas de un data.frame
#' @export
complete_epiweeks <- function(dataset,
                              col_epiweek) {
  comp_dataset <- dataset
  col_names <- names(comp_dataset)
  if (col_epiweek %in% col_names) {
    for (i in 1:53) {
      if (!any(comp_dataset[[col_epiweek]]) == i
          || is.na(any(comp_dataset[[col_epiweek]] == i))) {
        new_row <- data.frame(semanaepidemiologicavegeneral = i,
                              casos = 0,
                              evento = event_name,
                              etiqueta = event_label)
        if ("total_casos" %in% col_names) {
          total_row <- data.frame(total_casos = 0)
          new_row <- cbind(new_row, total_row)
        } else if ("total_casos" %in% col_names) {
          samples_row <- data.frame(total_muestras = 0)
          new_row <- cbind(new_row, samples_row)
        } else if ("porcentaje" %in% col_names) {
          per_row <- data.frame(porcentaje = 0.0)
          new_row <- cbind(new_row, per_row)
        }
        comp_dataset <- rbind(comp_dataset, new_row)
      }
    }
  }
  return(comp_dataset)
}
