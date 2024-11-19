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
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_conditionals <- config::get(file = config_path,
                                       "age_categories")$conditionals
  category_labels <- config::get(file = config_path,
                                 "age_categories")$categories
  age_values <- unlist(strsplit(age, " ", fixed = TRUE))
  category <- category_labels[1]
  print(age)
  if ("años" %in% age_values) {
    i <- 1
    for (conditional in category_conditionals) {
      if (eval(parse(text = conditional))) {
        category <- category_labels[i]
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
  if (init_pos > -1) {
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
      other_vrs <- virus$other_viruses
      subtypes_values <- c(subtypes_values, other_vrs$values)
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
add_missing_weeks <- function(dataset, col_epiweek) {
  max_epiweek <-
    max(as.numeric(dataset[[col_epiweek]]))
  if (max_epiweek < 53) {
    diff_epiweek <- 53 - max_epiweek
    dataset_aux <- data.frame()
    dataset_aux <- rbind(dataset_aux,
                         data.frame(semana =
                                  seq(max_epiweek + 1, 53),
                                casos = rep(0, diff_epiweek),
                                total_casos = rep(0, diff_epiweek),
                                porcentaje = rep(0, diff_epiweek)))
    names(dataset_aux)[names(dataset_aux)
                       == "semana"] <- col_epiweek
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
      names_from = .data$grupo_edad, # Columna que se convierte en encabezados
      values_from = .data$casos      # Valores que llenan la tabla
    )
  cols_order <- factor(colnames(data_groups),
                       levels = category_labels)
  data_groups <- data_groups %>%
    dplyr::select(dplyr::all_of(levels(cols_order)))
  return(data_groups)
}
