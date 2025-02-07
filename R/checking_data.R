#' @title Agrupar los datos por columnas, ordenarlas y definir un
#' porcentaje
#' @export
group_columns_total <- function(disease_data,
                                event_name = "adenovirus",
                                col_names,
                                wt_percentage = FALSE,
                                total_cases = 0,
                                event_label = NULL,
                                sum_cases = FALSE,
                                col_order = NULL,
                                etiqueta = TRUE) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  categorie_labels <- config::get(file = config_path,
                                  "age_categorie_labels")
  if (!sum_cases) {
    disease_data_grouped  <- disease_data %>% dplyr::group_by(
      dplyr::across(dplyr::all_of(col_names))) %>%
      dplyr::summarise(casos = dplyr::n(), .groups = "drop")   
  } else {
    disease_data_grouped <-
      disease_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>%
      dplyr::summarise(casos = sum(.data$casos), .groups = "drop")
  }
  if (wt_percentage) {
    if (total_cases == 0) {
      total_cases <- sum(disease_data_grouped$casos)
    }
    disease_data_grouped  <-  disease_data_grouped %>%
      dplyr::mutate(porcentaje =
                      round((disease_data_grouped$casos / total_cases) * 100,
                            1))
  }
  if (!("etiqueta" %in% col_names) && etiqueta) {
    disease_data_grouped  <-  disease_data_grouped %>%
      dplyr::mutate(evento = event_name, etiqueta = event_label) 
  }
  if (is.null(event_label)) {
    for (label in categorie_labels) {
      if (!any(disease_data_grouped == label) ||
          is.na(any(disease_data_grouped == label))) {
        new_row <- data.frame(grupo_edad = label, casos = 0, porcentaje  = 0,
                              evento = event_name, etiqueta = event_label)
        disease_data_grouped <- rbind(disease_data_grouped, new_row)
      }
    }
  }
  if ("semanaepidemiologicavegeneral" %in% col_names) {
    for (i in 1:52) {
      if (!any(disease_data_grouped$semanaepidemiologicavegeneral == i)
          || is.na(any(disease_data_grouped$semanaepidemiologicavegeneral
                       == i))) {
        if ("porcentaje" %in% col_names) {
          new_row <- data.frame(semanaepidemiologicavegeneral = i,
                                casos = 0,
                                porcentaje  = 0,
                                evento = event_name,
                                etiqueta = event_label)
        } else {
          new_row <- data.frame(semanaepidemiologicavegeneral = i,
                                casos = 0,
                                evento = event_name,
                                etiqueta = event_label)
        }
        disease_data_grouped <- rbind(disease_data_grouped, new_row)
      }
    }
  }
  if (!is.null(col_order)) {
    disease_data_grouped <- disease_data_grouped %>%
      dplyr::arrange(dplyr::desc(!!dplyr::sym(col_order)))
  }
  return(disease_data_grouped)
}

#' @title Obtener los casos de los virus de la base de datos filmarray de
#' base de datos de la Fundación Cardio Infantil
#' @export
get_cases_filmarray <- function(report_data,
                                positive_value = "DETECTADO",
                                age_groups = TRUE,
                                epiweek = NULL,
                                vrs_influenza = NULL) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_viruses <- config::get(file = config_path, "viruses")
  col_age_groups <- config::get(file = config_path,
                                "filmarray_data")$age_groups$col_valid
  col_epiweek <- config::get(file = config_path,
                             "filmarray_data")$epiweek$col_valid
  if (!is.null(vrs_influenza)) {
    cols_viruses <-
      get_influenza_viruses(viruses = cols_viruses,
                            events = vrs_influenza)
  }
  if (!is.null(epiweek)  && epiweek != "all") {
    col_epiweek <- config::get(file = config_path,
                               "filmarray_data")$epiweek$col_valid
    report_data <-
      report_data[which(report_data[[col_epiweek]]
                              == as.numeric(epiweek)), ]
  }
  viruses_age_group <- data.frame()
  for (virus in cols_viruses) {
      filmarray_vrs <- virus$filmarray
      if (all(filmarray_vrs$col_name %in% names(report_data)) &&
          !("" %in% filmarray_vrs$values)) {
        for (col_name in filmarray_vrs$col_name) {
          positive_cases <-
            report_data[report_data[[col_name]]
                        == filmarray_vrs$values, ]
          if (!is.null(epiweek)) {
              positive_cases <- group_columns_total(disease_data = positive_cases,
                                                    event_name = virus$name,
                                                    col_names = col_epiweek,
                                                    event_label = virus$label) 
          }
          if (age_groups) {
            positive_cases_age_group <-
              group_columns_total(positive_cases,
                                  col_age_groups,
                                  event_name = virus$name,
                                  wt_percentage = TRUE,
                                  total_cases = nrow(positive_cases),
                                  event_label = virus$label)
            positive_cases_age_group <-
              complete_age_categories(data_grouped = positive_cases_age_group,
                                      event_name = virus$name,
                                      event_label = virus$label)
            positive_cases_age_group$total_casos <-
              nrow(positive_cases)
            viruses_age_group <-
              rbind(viruses_age_group, positive_cases_age_group)
          } else {
            positive_cases$total_casos <-
              nrow(positive_cases)
            viruses_age_group <-
              rbind(viruses_age_group, positive_cases)
          }
        }
      }
  }
  na_values <- which(is.na(
    viruses_age_group[[col_age_groups]]))
  if (length(na_values) > 0) {
    viruses_age_group <-
      viruses_age_group[-na_values, ]
  }
  sd_values <- which(viruses_age_group[[col_age_groups]]
                      == "SD")
  if (length(sd_values) > 0) {
    viruses_age_group <-
      viruses_age_group[-sd_values, ]
  }
  return(viruses_age_group)
}

#' @title Obtener la distribución de casos por semana epidemiológicas
#' @export
get_distribution_epiweek <- function(report_data,
                                     positive_value = "DETECTADO",
                                     col_epiweek = "se") {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  column_names <- config::get(file = config_path,
                              "respiratory_virus_column_names")
  names <- config::get(file = config_path,
                       "respiratory_virus_names")
  viruses_age_group <- data.frame()
  i <- 1
  for (column in column_names) {
    positive_cases <-
      report_data[eval(parse(text = paste0("report_data$",
                                           column,
                                           " == ", '"',
                                           positive_value, '"'))), ]
    positive_cases_age_group <-
      group_columns_total(positive_cases,
                          col_group_age,
                          event_name = column,
                          wt_percentage = TRUE,
                          total_cases = nrow(positive_cases),
                          event_label = names[i])
    viruses_age_group <-
      rbind(viruses_age_group, positive_cases_age_group)
    i <- i + 1
  }
  na_values <- which(is.na(
    viruses_age_group[[col_epiweek]]))
  if (length(na_values) > 0) {
    viruses_age_group <-
      viruses_age_group[-na_values, ]
  }
  sd_values <- which(viruses_age_group[[col_epiweek]]
                     == "SD")
  if (length(sd_values) > 0) {
    viruses_age_group <-
      viruses_age_group[-sd_values, ]
  }
  return(viruses_age_group)
}

#' @title Generar los grupos de edad para la base de datos Otro Virus
#' @export
generate_age_groups_viruses <- function(report_data,
                                     event_name = "adenovirus",
                                     wt_percentage = FALSE,
                                     total_cases = 0,
                                     event_label) {
  data_grouped  <- report_data %>% dplyr::group_by(
    dplyr::across(
      dplyr::all_of("rangodeedadvegeneral"))) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  third_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_5_y_9_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_10_y_14_anos"][1], 
        na.rm = TRUE)
  four_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_15_y_19_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_20_y_29_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_30_y_39_anos"][1],
        na.rm = TRUE)
  five_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_40_y_49_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_50_y_59_anos"][1], 
        na.rm = TRUE)
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "1_ano"]  <- "< 2 años"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_1_y_4_anos"]  <- "2 a 4 años"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "60_y_mas_anos"]  <- "60 y más"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_5_y_9_anos"]  <- "5 a 14 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "5 a 14 años"] <-
    third_group_age
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_15_y_19_anos"]  <- "15 a 39 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "15 a 39 años"] <-
    four_group_age
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_40_y_49_anos"]  <- "40 a 59 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "40 a 59 años"] <-
    five_group_age
  
  if (length(which(stringr::str_detect(data_grouped$rangodeedadvegeneral,
                                       "_"))) > 0) {
    data_grouped <-
      data_grouped[-which(stringr::str_detect(data_grouped$rangodeedadvegeneral,
                                              "_")), ]
  }
  if (length(which(is.na(data_grouped$rangodeedadvegeneral))) > 0) {
    data_grouped <-
      data_grouped[-which(is.na(data_grouped$rangodeedadvegeneral)), ]
  }
  colnames(data_grouped)[colnames(data_grouped) == "rangodeedadvegeneral"] <-
    "grupo_edad"
  if (total_cases > 0) {
    data_grouped  <-  data_grouped %>% dplyr::mutate(
      porcentaje = round((data_grouped$casos/total_cases)*100, 1))
  } else {
    data_grouped  <-  data_grouped %>% dplyr::mutate(
      porcentaje = 0.0)
  }
  data_grouped  <-  data_grouped %>%
    dplyr::mutate(evento = event_name, etiqueta = event_label)
  data_grouped <-
    complete_age_categories(data_grouped = data_grouped,
                            event_name = event_name,
                            event_label = event_label)
  
  return(data_grouped)
  
}

#' @title Obtener la distribución de casos de la base de datos Otros Virus
#' @export
get_cases_other_viruses <- function(report_data,
                                    epiweek = NULL,
                                    age_groups = FALSE,
                                    vrs_influenza = NULL) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_viruses <- config::get(file = config_path, "viruses")
  invalid_results <- config::get(file = config_path,
                                 "other_viruses")$invalid_results
  positive_cases <- data.frame()
  col_epiweek <- config::get(file = config_path,
                             "other_viruses")$epiweek$col_valid
  if (!is.null(epiweek) && epiweek != "all") {
    report_data <- report_data %>%
      dplyr::filter(!!dplyr::sym(col_epiweek) == as.numeric(epiweek))
  }
  if (!is.null(vrs_influenza)) {
    cols_viruses <-
      get_influenza_viruses(viruses = cols_viruses,
                            events = vrs_influenza)
  }
  for (virus in cols_viruses) {
    other_vrs <- virus$other_viruses
    if (other_vrs$col_name %in% names(report_data) &&
        !("" %in% other_vrs$values)) {
      cases_virus <- report_data
      if ("valid_result" %in% names(other_vrs)) {
        for (value in invalid_results$values) {
          if (value != "") {
            cases_virus <- cases_virus[which(!stringr::str_detect(
              invalid_results$col_name, value)), ]
          }
        }
      }
      if ("col_subtypes" %in% names(other_vrs)) {
        subtypes_values <-
          get_subtypes_values(viruses = cols_viruses,
                                        subtypes = virus$subtypes)
        other_vrs$values <- subtypes_values
        cases_virus <- cases_virus[
          which(stringr::str_detect(cases_virus[[other_vrs$col_name]],
                                     other_vrs$original_value)), ]
      }
      positive_cases_virus <- data.frame()
      for (value in other_vrs$values) {
        if (value != "") {
          if ("col_subtypes" %in% names(other_vrs)) {
            cases_virus <- cases_virus[which(!stringr::str_detect(
              cases_virus[[other_vrs$col_name]], value)), ]
          } else {
            cases_virus <- cases_virus[which(stringr::str_detect(
              cases_virus[[other_vrs$col_name]], value)), ]
          }
          if (!is.null(epiweek)) { 
            cases_virus <- group_columns_total(disease_data = cases_virus,
                                         event_name = virus$name,
                                         col_names = col_epiweek,
                                         event_label = virus$label)  
          }
          positive_cases_virus <- rbind(positive_cases_virus, cases_virus)
        }
      }
      if (age_groups) {
        cases_age_groups <-
          generate_age_groups_viruses(report_data = cases_virus,
                                      event_name = virus$name,
                                      wt_percentage = TRUE,
                                      total_cases = nrow(cases_virus),
                                      event_label = virus$label)
        cases_age_groups$total_casos <- nrow(cases_virus)
        positive_cases <- rbind(positive_cases, cases_age_groups)
      } else {
        positive_cases_virus$total_casos <- nrow(cases_virus)
        positive_cases <- rbind(positive_cases, positive_cases_virus)
      }
    }
  }
  return(positive_cases)
}

#' @title Obtener la distribución de casos de las base de datos Filmarray
#' de la Fundación Cardio Infantil y Otros Virus
#' @export
get_dist_fci_other_vrs <- function(fci_data, vrs_data) {
  dist_fci_other_vrs <- rbind(fci_data, vrs_data)
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("evento", "grupo_edad","etiqueta")))) %>%
    dplyr::summarise(casos = sum(casos),
                     total_casos = sum(total_casos),
                     .groups = "drop")
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::mutate(porcentaje =
                    round((.data$casos * 100)/.data$total_casos))
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::select(.data$grupo_edad,
                  .data$casos,
                  .data$porcentaje,
                  .data$evento,
                  .data$etiqueta)
  return(dist_fci_other_vrs)
}

#' @title Obtener la distribución de casos de Sars CoV 2
#' @export
get_cases_sars <- function(report_data,
                           positive_value = "DETECTADO",
                           age_groups = FALSE,
                           epiweek = NULL) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  col_epiweek <- config::get(file = config_path,
                             "other_viruses")$epiweek$col_valid
  viruses_age_group <- data.frame()
  positive_cases_sars <-
    report_data[report_data$resultadonuevocoronavirussarscov2vegeneral ==
                  "positivo_para_nuevo_coronavirus_sars_cov_2", ]
  positive_cases_virusvgeneral <- report_data[which(stringr::str_detect(
    report_data$virusdetectadosvegeneral, "covid_19")), ]
  positive_cases_sars <- rbind(positive_cases_sars,
                               positive_cases_virusvgeneral)
  if (!is.null(epiweek)) {
    sars_epiweeks <-
      group_columns_total(positive_cases_sars,
                          event_name = "sars",
                          col_names = col_epiweek,
                          event_label = "SARS CoV 2")
    sars_epiweeks <-
      get_cases_epiweeks(report_data = report_data,
                         data_grouped = sars_epiweeks,
                         col_epiweek = col_epiweek)
    sars_epiweeks$evento <- "sars"
    sars_epiweeks$etiqueta <- "SARS CoV 2"
    if (epiweek != "all") {
      sars_epiweeks <- sars_epiweeks %>%
        dplyr::filter(!!dplyr::sym(col_epiweek) == as.numeric(epiweek))
    }
    return(sars_epiweeks)
  }
  if (age_groups) {
    sars_age_groups <-
      generate_age_groups_viruses(positive_cases_sars,
                                  event_name = "sars",
                                  wt_percentage = TRUE,
                                  total_cases = nrow(positive_cases_sars),
                                  event_label = "SARS CoV 2")
    return(sars_age_groups)
  }
  return(positive_cases_sars)
}

#' @title Obtener la distribución de casos de las bases de datos Filmarray y
#' Otros Virus
#' @export
get_distribution_age_vr_sars <- function(data_vr, data_sars) {
  distribution_age_vr_sars <- rbind(data_vr, data_sars)
  return(distribution_age_vr_sars)
}

#' @title Obtener la distribución de casos ESI incluyendo Sars CoV 2
#' Otros Virus
#' @export
get_distribution_esi_sars <- function(report_data,
                                      epiweek = 0,
                                      test = "bio_molecular") {
  viruses_age_group <- data.frame()
  report_data$resultadonuevocoronavirussarscov2vegeneral <- epitrix::clean_labels(
    report_data$resultadonuevocoronavirussarscov2vegeneral)
  report_data$rangodeedadvegeneral <- epitrix::clean_labels(
    report_data$rangodeedadvegeneral)
  report_data$clasificacionvegeneral <- epitrix::clean_labels(
    report_data$clasificacionvegeneral)
  report_data$fluorescenciavegeneral <- epitrix::clean_labels(
    report_data$fluorescenciavegeneral)
  report_data$eventovegeneral <- epitrix::clean_labels(
    report_data$eventovegeneral)
  report_data$virusdetectadosvegeneral <- epitrix::clean_labels(
    report_data$virusdetectadosvegeneral)
  positive_cases_sars <- report_data
  positive_cases_sars <- positive_cases_sars[
    positive_cases_sars$eventovegeneral == "esi_irag_centinela_345", ]
  positive_cases_sars <- positive_cases_sars[
    positive_cases_sars$clasificacionvegeneral == "ambulatorio", ]
  if (test == "bio_molecular") {
    positive_cases_sars <- dplyr::filter(positive_cases_sars , 
                                         .data$fluorescenciavegeneral ==
                                           "la_prueba_no_se_realiza" | 
                                           is.na(.data$fluorescenciavegeneral))
  } else if (test == "fluorescencia") {
    positive_cases_sars <- dplyr::filter(positive_cases_sars , 
                                         .data$fluorescenciavegeneral !=
                                           "la_prueba_no_se_realiza" & 
                                           !is.na(.data$fluorescenciavegeneral))
  }
  positive_cases_sars <- positive_cases_sars[
    positive_cases_sars$resultadonuevocoronavirussarscov2vegeneral 
    == "positivo_para_nuevo_coronavirus_sars_cov_2", ]
  positive_cases_sars <- positive_cases_sars[which(
    !is.na(
      positive_cases_sars$fechainiciodesintomasvegeneral)), ]
  
  viruses_age_group <- generate_age_groups_viruses(positive_cases_sars)
  
  return(viruses_age_group)
  
}

#' @title Obtener la distribución de casos de la vigilancia
#' Otros Virus
#' @export
get_distribution_surveillance <- function(report_data,
                                          epiweek = 0,
                                          include_sars = FALSE,
                                          surveillance_type = "esi",
                                          test = NULL) {
  viruses_age_group <- data.frame()
  report_data_esi <- report_data
  report_data_sars <- report_data
  report_data_others <- report_data
  report_data_irag <- report_data
  if (!is.null(surveillance_type)) {
    if (surveillance_type == "esi") {
      report_data_esi <- report_data_esi[
        report_data_esi$eventovegeneral == "esi_irag_centinela_345", ]
      report_data_esi <- report_data_esi[
        report_data_esi$clasificacionvegeneral == "ambulatorio", ]
    } else if (surveillance_type == "irag_grave") {
      report_data_esi <- report_data_esi[
        report_data_esi$eventovegeneral == "esi_irag_centinela_345", ]
      report_data_esi <- report_data_esi[
        which(stringr::str_detect(
          report_data_esi$clasificacionvegeneral, "hospitalizado")), ]
    } else if (surveillance_type == "irag_inusitado") {
      report_data_esi <- report_data_esi[
        report_data_esi$eventovegeneral == "irag_inusitado_348", ]
    }
  }
  if (epiweek > 0) {
    report_data_esi <- report_data_esi[
      report_data_esi$semanaepidemiologicavegeneral == epiweek, ]
  }
  report_data_esi <- report_data_esi[which(
    !is.na(
      report_data_esi$fechainiciodesintomasvegeneral)), ]
  
  if (!is.null(test)) {
    if (test == "bio_molecular") {
      positive_cases_sars <- dplyr::filter(report_data_esi ,
                                           .data$fluorescenciavegeneral == "la_prueba_no_se_realiza" || 
                                             is.na(.data$fluorescenciavegeneral))
    } else if (test == "fluorescencia") {
      report_data_esi <-
        dplyr::filter(
          report_data_esi,
          .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
            .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
            .data$fluorescenciavegeneral != "muestra_escasa_de_células" &&
            .data$fluorescenciavegeneral != "muestra_insuficiente_no_se_procesa"
          && .data$fluorescenciavegeneral 
          != "no_se_procesa_ifi_tiempo_de_toma_de_muestra_superior_a_7_dias_nota_2" &&
            !is.na(.data$fluorescenciavegeneral))
    }
  }
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  column_names <- config::get(file = config_path, "respiratory_virus_detected")
  names <- config::get(file = config_path, "respiratory_virus_detected_names")
  i <- 1
  for (column in column_names) {
    positive_cases <- report_data_esi[which(stringr::str_detect(
      report_data_esi$virusdetectadosvegeneral, column)), ]
    positive_cases_age_group <- generate_age_groups_viruses(positive_cases, 
                                                         event_name = column, 
                                                         wt_percentage = TRUE, 
                                                         total_cases = nrow(positive_cases), 
                                                         event_label = names[i])
    viruses_age_group <- rbind(viruses_age_group, positive_cases_age_group)
    i <- i + 1
  }
  if (include_sars) {
    positive_cases <- report_data_esi[
      report_data_esi$resultadonuevocoronavirussarscov2vegeneral 
      == "positivo_para_nuevo_coronavirus_sars_cov_2", ]
    positive_cases_virusvgeneral <- report_data_esi[which(stringr::str_detect(
      report_data_esi$virusdetectadosvegeneral, "covid_19")), ]
    if (nrow(positive_cases_virusvgeneral) > 1) {
      positive_cases <- rbind(positive_cases, positive_cases_virusvgeneral)
    }
    positive_cases_age_group <-
      generate_age_groups_viruses(positive_cases,
                                  event_name = "sars",
                                  wt_percentage = TRUE,
                                  total_cases = nrow(positive_cases),
                                  event_label = "SARS CoV 2")
    viruses_age_group <- rbind(viruses_age_group, positive_cases_age_group)
  }
  return(viruses_age_group)
}

#' @title Obtener la distribución de casos por tipo de prueba
#' @export
get_distribution_test <- function(report_data,
                                  epiweek = 0,
                                  include_sars = FALSE,
                                  test = NULL,
                                  col_epiweek = NULL) {
  
  viruses_epiweek_group <- data.frame()
  report_data_test <- data.frame()
  report_data_irag_grave <- report_data[
    report_data$eventovegeneral == "esi_irag_centinela_345", ]
  report_data_irag_grave <- report_data_irag_grave[
    which(stringr::str_detect(
      report_data_irag_grave$clasificacionvegeneral, "hospitalizado")), ]
  report_data_irag <- report_data[
    report_data$eventovegeneral == "irag_inusitado_348", ]
  
  report_data_test <- rbind(report_data_test, report_data_irag_grave)
  report_data_test <- rbind(report_data_test, report_data_irag)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  if (is.null(col_epiweek)) {
    col_epiweek <- config::get(file = config_path,
                               "other_viruses")$epiweek$col_valid
    index_col <- which(col_epiweek %in% names(report_data_test))
    if (length(index_col) == 0) {
        stop("Por favor ingrese el nombre de la variable o ",
             "columna que corresponde ",
           " a la semana epidemiologica")
    }
  }
  if (epiweek > 0) {
    report_data_test <- report_data_test[
      report_data_test[[col_epiweek]] == epiweek, ]
  }
  cases_epiweeks <-
    get_cases_epiweeks(report_data = report_data,
                       data_grouped = report_data_test,
                       col_epiweek = col_epiweek,
                       table = FALSE)
  
  cases_epiweeks <- 
    add_missing_weeks(dataset = cases_epiweeks,
                      col_epiweek = col_epiweek)
  viruses_epiweeks <- get_cases_other_viruses(report_data = report_data_test,
                                              epiweek = "all")
  if (include_sars) {
    sars_epiweeks <- get_cases_sars(report_data = report_data_test,
                                    epiweek = "all")
    viruses_epiweeks <-
      rbind(viruses_epiweeks, sars_epiweeks)
  }
  if (length(which(is.na(viruses_epiweeks[[col_epiweek]]))) > 0) {
    viruses_epiweeks <-
      viruses_epiweeks[-which(is.na(viruses_epiweeks[[col_epiweek]])), ]
  }
  distribution_epiweeks <- list(cases_epiweeks = cases_epiweeks,
                                viruses_epiweeks = viruses_epiweeks)
  return(distribution_epiweeks)
}

#' @title Obtener la distribución de casos por semana epidemiológica
#' @export
get_cases_epiweeks <- function(report_data,
                               data_grouped,
                               col_epiweek,
                               diseases_epiweek, table = FALSE) {
  if (table) {
    table_epiweeks <- data.frame(Semana = data_grouped[[col_epiweek]],
                                 Positivos = data_grouped$porcentaje)
    return(table_epiweeks)
  }
  total_positive_cases <- data.frame()
  total_cases_epiweeks <- report_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(col_epiweek))) %>%
    dplyr::summarise(total_casos = n())
  cases_epiweeks <- data_grouped %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(col_epiweek))) %>%
    dplyr::summarise(casos = n())
  cases_epiweeks <- dplyr::inner_join(cases_epiweeks,
                                      total_cases_epiweeks,
                                      by = col_epiweek)
  cases_epiweeks <- cases_epiweeks %>%
    dplyr::mutate(porcentaje =
                    round((.data$casos * 100)/.data$total_casos, 1))
  return(cases_epiweeks)
}

#' @title Obtener la proporción acumulada de los virus
#' @export
get_viruses_cumulative_proportion <- function(report_data) {
  viruses_cumulative_proportion  <- data.frame()
  col_names <- c("evento", "etiqueta")
  col_names_others <- c("evento", "etiqueta")
  col_names_cumulative <- "etiqueta"
  viruses_proportion <- report_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>%
    dplyr::summarise(casos = sum(.data$casos))
  total_cases <- sum(viruses_proportion$casos)
  viruses_proportion <- viruses_proportion %>%
    dplyr::mutate(porcentaje =
                    round((.data$casos * 100) / total_cases, 1))
  return(viruses_proportion)
}

#' @title Obtener la proporción acumulada de los virus  por semana
#' epidemiológica
#' @export
get_cases_prop_epiweek <- function(filmarray_data,
                                   other_virs_data,
                                   include_sars = TRUE,
                                   epiweek) {
  cases_filmarray <- data.frame()
  cases_other_virs <- data.frame()
  distribution_influenza <- data.frame()
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  col_epiweek <- config::get(file = config_path,
                             "other_viruses")$epiweek$col_valid
  total_other_virs <- 0
  total_filmarray <- 0
  if (nrow(filmarray_data) > 0) {
    cases_filmarray <- get_cases_filmarray(report_data = filmarray_data,
                                           epiweek = epiweek,
                                           age_groups = FALSE)
    col_epiweek_filmarray <- config::get(file = config_path,
                               "filmarray_data")$epiweek$col_valid
    cases_filmarray <-
      dplyr::rename(cases_filmarray,
                    "semanaepidemiologicavegeneral" =
                    !!dplyr::sym(col_epiweek_filmarray))
    epiweek_filmarray <- filmarray_data %>%
      dplyr::filter(!!dplyr::sym(col_epiweek_filmarray) == epiweek)
    total_filmarray <- nrow(epiweek_filmarray)
  }
  if (nrow(other_virs_data) > 0) {
    cases_other_virs <- get_cases_other_viruses(report_data = other_virs_data,
                                                epiweek = epiweek,
                                                age_groups = FALSE)
    if (include_sars) {
      cases_sars <- get_cases_sars(report_data = other_virs_data,
                                   age_groups = FALSE,
                                   epiweek = epiweek)
      cases_sars <- cases_sars %>%
        dplyr::select(names(cases_other_virs))
      cases_other_virs <- rbind(cases_other_virs, cases_sars)
    }
    epiweek_other_virs <- other_virs_data %>%
      dplyr::filter(!!dplyr::sym(col_epiweek) == epiweek)
    total_other_virs <- nrow(epiweek_other_virs)
  }
  cases_epiweek <- rbind(cases_filmarray, cases_other_virs)
  total_samples <- total_filmarray + total_other_virs
  cases_epiweek <-
    group_columns_total(cases_epiweek,
                        event_name = "",
                        col_names = c("evento",
                                      "etiqueta"),
                        wt_percentage = TRUE,
                        total_cases = sum(cases_epiweek$casos),
                        event_label = "",
                        sum_cases = TRUE)
  proportion_epiweek <-
    list(cases_epiweek = cases_epiweek,
         total_samples = total_samples)
  return(proportion_epiweek)
}

#' @title Obtener la distribución de casos de Influenza y sus subtipos
#' @export
get_cases_influenza <- function(filmarray_data,
                                other_virs_data) {
  cases_filmarray <- data.frame()
  cases_other_virs <- data.frame()
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  events <- config::get(file = config_path, "influeza_events")
  col_epiweek <- config::get(file = config_path,
                      "other_viruses")$epiweek$col_valid
  if (nrow(filmarray_data) > 0) {
    cases_filmarray <- get_cases_filmarray(report_data = filmarray_data,
                                           epiweek = "all",
                                           vrs_influenza = events,
                                           age_groups = FALSE)
    col_epiweek_filmarray <- config::get(file = config_path,
                                         "filmarray_data")$epiweek$col_valid
    cases_filmarray <-
      dplyr::rename(cases_filmarray,
                    "semanaepidemiologicavegeneral" =
                   !!dplyr::sym(col_epiweek_filmarray))
    filmarray_data <-
      dplyr::rename(filmarray_data,
                    "semanaepidemiologicavegeneral" =
                   !!dplyr::sym(col_epiweek_filmarray))
  }
  if (nrow(other_virs_data) > 0) {
    cases_other_virs <- get_cases_other_viruses(report_data = other_virs_data,
                                                epiweek = "all",
                                                vrs_influenza = events,
                                                age_groups = FALSE)
  }
  cases_influenza <- rbind(cases_filmarray, cases_other_virs)
  cases_influenza <-
    group_columns_total(cases_influenza,
                        event_name = "",
                        col_names = c(col_epiweek,
                                      "etiqueta"),
                        wt_percentage = TRUE,
                        total_cases = sum(cases_influenza$casos),
                        event_label = "",
                        sum_cases = TRUE)
  filmarray_epiweeks <- filmarray_data %>%
    dplyr::select(!!dplyr::sym(col_epiweek))
  other_virs_epiweeks <- other_virs_data %>%
    dplyr::select(!!dplyr::sym(col_epiweek))
  data_epiweeks <- rbind(filmarray_epiweeks, other_virs_epiweeks)
  cases_epiweeks <-
    get_cases_epiweeks(report_data = data_epiweeks,
                       data_grouped = cases_influenza,
                       col_epiweek = col_epiweek)
  cases_epiweeks <- add_missing_weeks(dataset = cases_epiweeks,
                                       col_epiweek = col_epiweek)
  distribution_epiweeks <- list(cases_epiweeks = cases_epiweeks,
                                influenza_epiweeks = cases_influenza)
  return(distribution_epiweeks)
}

#' @title Obtener la distribución de casos de Tosferina
#' @export
get_cases_tosferina <- function(report_data, result = "positivo",
                                column = "semana_epidemiologica",
                                col_results = "interpretacion_del_resultado") {
  cases <- report_data
  tosferina_cases <- data.frame()
  na_values <- which(is.na(report_data[[col_results]]))
  if (length(na_values) > 0) {
    cases <- report_data[-na_values, ]
  }
  muestra_values <- grep("muestra", cases[[col_results]], fixed = TRUE)
  if (length(muestra_values) > 0) {
    cases <- cases[-muestra_values, ]
  }
  cases_grouped <- cases %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(column))) %>%
    dplyr::summarise(total_casos = dplyr::n(), .groups = "drop")
  positive_cases <- cases[grep(result, cases[[col_results]]), ]
  positive_grouped <- positive_cases %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(column))) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  data_grouped <- merge(x = positive_grouped, y = cases_grouped,
                        by = column, all.x = TRUE)
  data_grouped <- data_grouped %>%
    dplyr::mutate(porcentaje =
                    round((data_grouped$casos / data_grouped$total_casos) * 100,
                          1))
  if (column == "semana_epidemiologica") {
    for (week in setdiff(1:53, data_grouped[[column]])) {
      new_row <- data.frame(semana_epidemiologica = week,
                            casos = 0,
                            porcentaje  = 0, total_casos = 0)
      data_grouped <- rbind(data_grouped, new_row)
    }
  }
  if (column == "grupo_edad") {
    config_path <- system.file("extdata", "config.yml", package = "labrep")
    category_labels <-
      config::get(file = config_path,
                  "tosferina_data")$age_groups$labels
    for (label in category_labels) {
      if (!any(data_grouped == label) || is.na(any(data_grouped == label))) {
        new_row <- data.frame(grupo_edad = label, casos = 0, porcentaje  = 0,
                              total_casos = 0)
        data_grouped <- rbind(data_grouped, new_row)
      }
    }
    report_data[[column]] <- tolower(report_data[[column]])
    tosferina_cases <-
      get_cases_epiweeks(report_data = report_data,
                         data_grouped = data_grouped,
                         col_epiweek = column)
  }
  data_grouped <- data_grouped %>%
    dplyr::mutate(evento = "tosferina",
                  etiqueta = "tosferina",
                  interpretacion_del_resultado = "Positivo para Bordetella")
  if (nrow(tosferina_cases) > 0) {
    distribution_age_group <- list(cases_age_group = data_grouped,
                                   tosferina_cases = tosferina_cases)
    return(distribution_age_group)
  }
  return(data_grouped)
}

#' @title Obtener los resultados de casos de Tosferina
#' @export
get_results_tosferina <- function(report_data, results = "positivo",
                                  columns =  "semana_epidemiologica",
                                  col_results =
                                    "interpretacion_del_resultado") {
  columns <- c(columns, col_results)
  cases <- report_data[grepl(paste(c("positivo", "negativo"), collapse = "|"),
                             report_data[[col_results]]), ]
  na_values <- which(is.na(cases[[col_results]]))
  if (length(na_values) > 0) {
    cases <- cases[-na_values, ]
  }
  muestra_values <- grep("muestra", cases[[col_results]], fixed = TRUE)
  if (length(muestra_values) > 0) {
    cases <- cases[-muestra_values, ]
  }
  total_cases <- nrow(cases)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  col_age_groups <- config::get(file = config_path,
                                "tosferina_data")$age_groups$col_valid
  if (col_age_groups %in% columns) {
    sd_values <- grep("SD", cases[[col_age_groups]], fixed = TRUE)
    if (length(sd_values) > 0) {
      cases <- cases[-sd_values, ]
    }
  }
  data_grouped <- cases %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(columns))) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  data_grouped <- data_grouped %>%
    dplyr::mutate(porcentaje = round((data_grouped$casos / total_cases) *
                                       100, 1))
  data_grouped[[col_results]][grep("positivo",
                                   data_grouped[[col_results]],
                                  fixed = TRUE)] <-
    "Positivo para Bordetella"
  data_grouped[[col_results]][grep("negativo",
                                   data_grouped[[col_results]],
                            fixed = TRUE)] <-
    "Negativo para Bordetella"
  col_gender <-
    config::get(file = config_path,
                "tosferina_data")$gender$col_valid
  if (col_gender %in% columns) {
    data_grouped[[col_gender]] <-
      stringr::str_to_title(data_grouped[[col_gender]])
    if (length(unique(data_grouped[[col_gender]])) == 1) {
      gender <- "Femenino"
      if (gender %in% data_grouped) {
        gender <- "Masculino"
      }
      new_row <- data.frame(genero = gender,
                            casos = 0,
                            porcentaje = 0,
                            interpretacion_del_resultado =
                              "Positivo para Bordetella")
      data_grouped <- rbind(data_grouped, new_row)
    }
  }
  if (any(columns == "grupo_edad")) {
    data_grouped[["grupo_edad"]] <- tolower(data_grouped[["grupo_edad"]])
  }
  data_grouped <- data_grouped %>% dplyr::mutate(evento = "tosferina",
                                                 etiqueta = "tosferina")
  return(data_grouped)
}






#' @title Obtener tiempos epidemiológicos de datos históricos
#'
#' @description 
#' Transforma la tabla de la base de datos 'VIRUS RESPIRATORIOS 2022 A 2024' 
#' para generar 2 tablas que facilitan las operaciones para las funciones de visualización. 
#' Se crea una tabla con los datos adecuados para un gráfico de línea. 
#' Se crea una tabla con las columnas adecuadas para un gráfico de barras apiladas.
#'
#' @param dataset_epi_times Data frame con datos históricos que incluyen las columnas 
#'        `ano`, `periodo_epidemiologico`, `de_positividad` y las de distintos tipos de virus.
#' 
#' @return Una lista con dos data frames:
#' - `stacked_data`: Datos en formato largo con tipo de virus y número de casos por semana.
#' - `line_data`: Serie de tiempo con la positividad semanal.
#'
#' @export
get_historic_epi_times <- function(dataset_epi_times) {
  data <- dataset_epi_times 
  stacked_data <- data %>%
    tidyr::pivot_longer(cols = .data$a_h1n1_pdm09:.data$otros_virus, 
                        names_to = "Virus_Type", 
                        values_to = "Cases") %>%
    dplyr::mutate(YearWeek = paste(.data$ano,
                                   sprintf("%02d",
                                           .data$periodo_epidemiologico),
                                   sep = "-"))
  # Prepare line data for the line chart, ensuring YearWeek is created consistently
  line_data <- data %>%
    dplyr::mutate(YearWeek = paste(.data$ano, sprintf("%02d",
                                                      .data$periodo_epidemiologico),
                                   sep = "-")) %>%
    dplyr::select(.data$YearWeek, Percent_Positivity =
                    .data$percent_de_positividad) %>%
    tidyr::drop_na(.data$Percent_Positivity) # Remove any NA values in Percent_Positivity
  
  historic_data <- list(stacked_data = stacked_data,
                        line_data = line_data)
  return(historic_data)
}

