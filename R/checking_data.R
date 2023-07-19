group_columns_total <- function(disease_data,
                                event_name = "adenovirus",
                                col_names,
                                wt_percentage = FALSE,
                                total_cases = 0,
                                event_label = NULL,
                                sum_cases = FALSE) {
  config_path <- "C:/Users/geral/Documents/TRACE/tmp/sivirep/inst/extdata/config.yml"
  categorie_labels <- config::get(file = config_path, "age_categorie_labels")
  
  if (!sum_cases) {
    disease_data_grouped  <- disease_data %>% dplyr::group_by(
      dplyr::across(dplyr::all_of(col_names))) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")   
  } else {
    disease_data_grouped  <- disease_data %>% dplyr::group_by(
      dplyr::across(dplyr::all_of(col_names))) %>% dplyr::summarise(casos = sum(.data$casos),
                                                                    .groups = "drop")
  }
  
  if (wt_percentage) {
    disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(porcentaje = round((disease_data_grouped$casos/total_cases)*100, 1))
    if (!("etiqueta" %in% col_names)) {
      disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(evento = event_name, etiqueta = event_label) 
    }
  }
  
  if (is.null(event_label)) {
    for (label in categorie_labels) {
      if (!any(disease_data_grouped == label) || is.na(any(disease_data_grouped == label))) {
        new_row <- data.frame(grupo_edad = label, casos = 0, porcentaje  = 0, evento = event_name, etiqueta = event_label)
        disease_data_grouped <- rbind(disease_data_grouped, new_row)
      }
    }
  }
  
  if ("semanaepidemiologicavegeneral" %in% col_names) {
    for (i in 1:52) {
      if (!any(disease_data_grouped$semanaepidemiologicavegeneral == i) 
          || is.na(any(disease_data_grouped$semanaepidemiologicavegeneral == i))) {
        new_row <- data.frame(semanaepidemiologicavegeneral = i, casos = 0, porcentaje  = 0, evento = event_name, etiqueta = event_label)
        disease_data_grouped <- rbind(disease_data_grouped, new_row)
      }
    }
  }
  return(disease_data_grouped)
}

get_distribution_age_group <- function(report_data, positive_value = "DETECTADO") {
  config_path <- "C:/Users/geral/Documents/TRACE/tmp/sivirep/inst/extdata/config.yml"
  column_names <- config::get(file = config_path, "respiratory_virus_column_names")
  names <- config::get(file = config_path, "respiratory_virus_names")
  viruses_by_age_group <- data.frame()
  
  i <- 1
  for (column in column_names) {
    positive_cases <- report_data[eval(parse(text = paste0("report_data$", column, " == ", '"', positive_value, '"'))), ]
    positive_cases_by_age_group <- group_columns_total(positive_cases, "grupo_edad", event_name = column, wt_percentage = TRUE, total_cases = nrow(positive_cases), event_label = names[i])
    viruses_by_age_group <- rbind(viruses_by_age_group, positive_cases_by_age_group)
    i <- i + 1
  }
  viruses_by_age_group <- viruses_by_age_group[-which(is.na(viruses_by_age_group$grupo_edad)), ]
  viruses_by_age_group <- viruses_by_age_group[-which(viruses_by_age_group$grupo_edad == "SD"), ]
  return(viruses_by_age_group)
}

generate_age_groups_sars <- function(report_data,
                                     event_name = "adenovirus",
                                     wt_percentage = FALSE,
                                     total_cases = 0,
                                     event_label) {
  
  data_grouped  <- report_data %>% dplyr::group_by(
    dplyr::across(
      dplyr::all_of("rangodeedadvegeneral"))) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  
  third_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral == "entre_5_y_9_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral == "entre_10_y_14_anos"][1], 
        na.rm = TRUE)
  
  four_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral == "entre_15_y_19_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral == "entre_20_y_29_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral == "entre_30_y_39_anos"][1],
        na.rm = TRUE)
  
  five_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral == "entre_40_y_49_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral == "entre_50_y_59_anos"][1], 
        na.rm = TRUE)
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral == "1_ano"]  <- "<2 años"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral == "entre_1_y_4_anos"]  <- "2 a 4 años"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral == "60_y_mas_anos"]  <- "60 y más"
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral == "entre_5_y_9_anos"]  <- "5 a 14 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "5 a 14 años"]  <- third_group_age
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral == "entre_15_y_19_anos"]  <- "15 a 39 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "15 a 39 años"]  <- four_group_age
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral == "entre_40_y_49_anos"]  <- "40 a 59 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "40 a 59 años"]  <- five_group_age
  
  if (length(which(stringr::str_detect(data_grouped$rangodeedadvegeneral, "_"))) > 0) {
    data_grouped <- data_grouped[-which(stringr::str_detect(data_grouped$rangodeedadvegeneral, "_")), ]
  }
  
  if (length(which(is.na(data_grouped$rangodeedadvegeneral))) > 0) {
    data_grouped <- data_grouped[-which(is.na(data_grouped$rangodeedadvegeneral)), ]
  }
  
  colnames(data_grouped)[colnames(data_grouped) == "rangodeedadvegeneral"] <-
    "grupo_edad"
  
  if (total_cases > 0) {
    data_grouped  <-  data_grouped %>% dplyr::mutate(
      porcentaje = round((data_grouped$casos/total_cases)*100, 1))
  }
  else {
    data_grouped  <-  data_grouped %>% dplyr::mutate(
      porcentaje = 0.0)
  }
  
  data_grouped  <-  data_grouped %>% dplyr::mutate(evento = event_name, etiqueta = event_label)
  
  config_path <- "C:/Users/geral/Documents/TRACE/tmp/sivirep/inst/extdata/config.yml"
  categorie_labels <- config::get(file = config_path, "age_categorie_labels")
  
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

get_distribution_age_group_sars <- function(report_data, positive_value = "DETECTADO") {
  
  viruses_age_group <- data.frame()
  report_data$resultadonuevocoronavirussarscov2vegeneral <- epitrix::clean_labels(report_data$resultadonuevocoronavirussarscov2vegeneral)
  report_data$rangodeedadvegeneral <- epitrix::clean_labels(report_data$rangodeedadvegeneral)
  positive_cases_sars <- report_data[report_data$resultadonuevocoronavirussarscov2vegeneral == "positivo_para_nuevo_coronavirus_sars_cov_2", ]
  
  positive_cases_virusvgeneral <- report_data[which(stringr::str_detect(
    report_data$virusdetectadosvegeneral, "covid_19")), ]
  
  positive_cases_sars <- rbind(positive_cases_sars, positive_cases_virusvgeneral)
  
  viruses_age_group <- generate_age_groups_sars(positive_cases_sars,
                                                event_name = "sars", 
                                                wt_percentage = TRUE, 
                                                total_cases = nrow(positive_cases_sars), 
                                                event_label = "SARS CoV 2")
  
  return(viruses_age_group)
}

get_distribution_age_vr_sars <- function(data_vr, data_sars) {
  distribution_age_vr_sars <- rbind(data_vr, data_sars)
  return(distribution_age_vr_sars)
}

get_distribution_esi_sars <- function(report_data, epiweek = 0, test = "bio_molecular") {
  
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
  # positive_cases_sars <- positive_cases_sars[positive_cases_sars$semanaepidemiologicavegeneral == epiweek, ]
  positive_cases_sars <- positive_cases_sars[
    positive_cases_sars$eventovegeneral == "esi_irag_centinela_345", ]
  positive_cases_sars <- positive_cases_sars[
    positive_cases_sars$clasificacionvegeneral == "ambulatorio", ]
  
  if (test == "bio_molecular") {
    positive_cases_sars <- dplyr::filter(positive_cases_sars , 
                                         .data$fluorescenciavegeneral == "la_prueba_no_se_realiza" | 
                                           is.na(.data$fluorescenciavegeneral))
  } else if (test == "fluorescencia") {
    positive_cases_sars <- dplyr::filter(positive_cases_sars , 
                                         .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" & 
                                           !is.na(.data$fluorescenciavegeneral))
  }
  
  positive_cases_sars <- positive_cases_sars[
    positive_cases_sars$resultadonuevocoronavirussarscov2vegeneral 
    == "positivo_para_nuevo_coronavirus_sars_cov_2", ]
  positive_cases_sars <- positive_cases_sars[which(
    !is.na(
      positive_cases_sars$fechainiciodesintomasvegeneral)), ]
  
  viruses_age_group <- generate_age_groups_sars(positive_cases_sars)
  
  return(viruses_age_group)
  
}

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
      #report_data_sars <- report_data_sars[
      # report_data_sars$eventovegeneral == "covid_19_346", ]
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
  
  # 346 / clasificacionvegeneral
  # Validar mortalidad / casos / No se procesa
  # Contar covid en los otros eventos - siempre se hace COVID
  if (epiweek > 0) {
    report_data_esi <- report_data_esi[
      report_data_esi$semanaepidemiologicavegeneral == epiweek, ]
  }
  
  # Es valido
  report_data_esi <- report_data_esi[which(
    !is.na(
      report_data_esi$fechainiciodesintomasvegeneral)), ]
  
  if (!is.null(test)) {
    if (test == "bio_molecular") {
      positive_cases_sars <- dplyr::filter(report_data_esi ,
                                           .data$fluorescenciavegeneral == "la_prueba_no_se_realiza" || 
                                             is.na(.data$fluorescenciavegeneral))
      # Averiguar con SDS
    } else if (test == "fluorescencia") {
      report_data_esi <- dplyr::filter(report_data_esi ,
                                       .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
                                         .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
                                         .data$fluorescenciavegeneral != "muestra_escasa_de_células" &&
                                         .data$fluorescenciavegeneral != "muestra_insuficiente_no_se_procesa" &&
                                         .data$fluorescenciavegeneral 
                                       != "no_se_procesa_ifi_tiempo_de_toma_de_muestra_superior_a_7_dias_nota_2" &&
                                         !is.na(.data$fluorescenciavegeneral))
    }
  }
  
  config_path <- "C:/Users/geral/Documents/TRACE/labrep/inst/extdata/config.yml"
  column_names <- config::get(file = config_path, "respiratory_virus_detected")
  names <- config::get(file = config_path, "respiratory_virus_detected_names")
  
  i <- 1
  for (column in column_names) {
    positive_cases <- report_data_esi[which(stringr::str_detect(
      report_data_esi$virusdetectadosvegeneral, column)), ]
    positive_cases_age_group <- generate_age_groups_sars(positive_cases, 
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
    positive_cases_age_group <- generate_age_groups_sars(positive_cases, 
                                                         event_name = "sars", 
                                                         wt_percentage = TRUE, 
                                                         total_cases = nrow(positive_cases), 
                                                         event_label = "SARS CoV 2")
    viruses_age_group <- rbind(viruses_age_group, positive_cases_age_group)
  }
  
  return(viruses_age_group)
}


get_distribution_surveillance_2 <- function(report_data,
                                          epiweek = 0,
                                          include_sars = FALSE,
                                          surveillance_type = "esi",
                                          test = NULL) {
  viruses_age_group <- data.frame()
  report_data_esi <- clean_data_other_viruses(report_data)
  
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
  
  if (epiweek > 0) {
    report_data_esi <- report_data_esi[
      report_data_esi$semanaepidemiologicavegeneral == epiweek, ]
  }
  
  # Agregar filmarray FCI-Centinela
  
  report_data_esi <- report_data_esi[which(
    !is.na(
      report_data_esi$fechainiciodesintomasvegeneral)), ]
  
  if (!is.null(test)) {
    if (test == "bio_molecular") {
      positive_cases_sars <- dplyr::filter(report_data_esi ,
                                           .data$fluorescenciavegeneral == "la_prueba_no_se_realiza" || 
                                             is.na(.data$fluorescenciavegeneral))
    } else if (test == "fluorescencia") {
      report_data_esi <- dplyr::filter(report_data_esi ,
                                       .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
                                         .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
                                         .data$fluorescenciavegeneral != "muestra_insuficiente_no_se_procesa" &&
                                         .data$fluorescenciavegeneral 
                                       != "no_se_procesa_ifi_tiempo_de_toma_de_muestra_superior_a_7_dias_nota_2" &&
                                         !is.na(.data$fluorescenciavegeneral))
    }
  }
  
  config_path <- "C:/Users/geral/Documents/TRACE/tmp/sivirep/inst/extdata/config.yml"
  column_names <- config::get(file = config_path, "respiratory_virus_detected")
  names <- config::get(file = config_path, "respiratory_virus_detected_names")
  
  i <- 1
  for (column in column_names) {
    positive_cases <- report_data_esi[which(stringr::str_detect(
      report_data_esi$virusdetectadosvegeneral, column)), ]
    positive_cases_age_group <- generate_age_groups_sars(positive_cases, 
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
    positive_cases_age_group <- generate_age_groups_sars(positive_cases, 
                                                         event_name = "sars", 
                                                         wt_percentage = TRUE, 
                                                         total_cases = nrow(positive_cases), 
                                                         event_label = "SARS CoV 2")
    viruses_age_group <- rbind(viruses_age_group, positive_cases_age_group)
  }
  return(viruses_age_group)
}

get_distribution_test <- function(report_data,
                                  epiweek = 0,
                                  include_sars = FALSE,
                                  test = NULL) {
  
  viruses_epiweek_group <- data.frame()
  report_data_test <- data.frame()
  
  report_data_irag_grave <- report_data[
    report_data_test$eventovegeneral == "esi_irag_centinela_345", ]
  report_data_irag_grave <- report_data_irag_grave[
    which(stringr::str_detect(
      report_data_irag_grave$clasificacionvegeneral, "hospitalizado")), ]
  report_data_irag <- report_data[
    report_data$eventovegeneral == "irag_inusitado_348", ]
  
  report_data_test <- rbind(report_data_test, report_data_irag_grave)
  report_data_test <- rbind(report_data_test, report_data_irag)
  
  if (epiweek > 0) {
    report_data_test <- report_data_test[
      report_data_test$semanaepidemiologicavegeneral == epiweek, ]
  }
  
  # config_path <- "C:/Users/geral/Documents/TRACE/labrep/inst/extdata/config.yml"
  # exceptions <- config::get(file = config_path, "respiratory_virus_test_exceptions")
  # 
  # for (ex in exceptions) {
  #   if (length(which(report_data_test$fluorescenciavegeneral == ex 
  #                    && report_data_test$virusdetectadosvegeneral == ex)) > 0) {
  #     report_data_test <- report_data_test[
  #       -which(report_data_test$fluorescenciavegeneral == ex), ]
  #   }
  # }
  
  config_path <- "C:/Users/geral/Documents/TRACE/labrep/inst/extdata/config.yml"
  column_names <- config::get(file = config_path, "respiratory_virus_detected")
  names <- config::get(file = config_path, "respiratory_virus_detected_names")
  
  i <- 1
  for (column in column_names) {
    positive_cases <- report_data_test[which(stringr::str_detect(
      report_data_test$virusdetectadosvegeneral, column)), ]
    positive_cases_age_group <- group_columns_total(positive_cases, 
                                                    event_name = column,
                                                    col_names = "semanaepidemiologicavegeneral",
                                                    wt_percentage = TRUE, 
                                                    total_cases = nrow(positive_cases), 
                                                    event_label = names[i])
    viruses_epiweek_group <- rbind(viruses_epiweek_group, positive_cases_age_group)
    i <- i + 1
  }
  
  if (include_sars) {
    positive_cases <- report_data_test[
      report_data_test$resultadonuevocoronavirussarscov2vegeneral 
      == "positivo_para_nuevo_coronavirus_sars_cov_2", ]
    positive_cases_age_group <- group_columns_total(positive_cases, 
                                                    event_name = "sars",
                                                    col_names = "semanaepidemiologicavegeneral",
                                                    wt_percentage = TRUE, 
                                                    total_cases = nrow(positive_cases), 
                                                    event_label = "SARS CoV 2")
    viruses_epiweek_group <- rbind(viruses_epiweek_group, positive_cases_age_group)
  }
  if (length(which(is.na(viruses_epiweek_group$semanaepidemiologicavegeneral))) > 0) {
    viruses_epiweek_group <- viruses_epiweek_group[-which(is.na(viruses_epiweek_group$semanaepidemiologicavegeneral)), ]
  }
  return(viruses_epiweek_group)
}


get_cases_epiweeks <- function(diseases_epiweek, table = FALSE) {
  
  total_positive_cases <- group_columns_total(diseases_epiweek,
                                              event_name = "positivos",
                                              col_names = "semanaepidemiologicavegeneral",
                                              wt_percentage = TRUE, 
                                              total_cases = sum(diseases_epiweek$casos), 
                                              event_label = "Positivos",
                                              sum_cases = TRUE)
  if (table) {
    total_positive_cases <- data.frame(Semana = total_positive_cases$semanaepidemiologicavegeneral,
                                       Positivos = total_positive_cases$porcentaje)
  }
  
  return(total_positive_cases)
}

get_viruses_cumulative_proportion <- function(fci_data, other_virs_data) {
  
  viruses_cumulative_proportion  <- data.frame()
  
  fci_data <- fci_data[, -1]
  fci_data_proportion <- group_columns_total(fci_data,
                                             event_name = "",
                                             col_names = c("evento",
                                                           "etiqueta"),
                                             wt_percentage = TRUE,
                                             total_cases = sum(fci_data$casos), 
                                             event_label = "",
                                             sum_cases = TRUE)
  
  other_virs_data_proportion <- group_columns_total(other_virs_data,
                                                    event_name = "",
                                                    col_names = c("evento", "etiqueta"),
                                                    wt_percentage = TRUE,
                                                    total_cases = sum(other_virs_data$casos), 
                                                    event_label = "",
                                                    sum_cases = TRUE)
  viruses_cumulative_proportion <- rbind(fci_data_proportion,
                                         other_virs_data_proportion)
  viruses_cumulative_proportion <- viruses_cumulative_proportion[, -1]
  viruses_cumulative_proportion <- group_columns_total(viruses_cumulative_proportion,
                                                    event_name = "",
                                                    col_names = c("etiqueta"),
                                                    wt_percentage = TRUE,
                                                    total_cases = sum(viruses_cumulative_proportion$casos), 
                                                    event_label = "",
                                                    sum_cases = TRUE)
  
  return(viruses_cumulative_proportion)
}

get_distribution_influenza <- function(fci_data, other_virs_data) {
  
  influeza_fci_data <- fci_data
  influenza_other_virs_data <- other_virs_data
  distribution_influenza <- data.frame()
  config_path <- "C:/Users/geral/Documents/TRACE/labrep/inst/extdata/config.yml"
  
  if (nrow(influeza_fci_data) > 0) {
    column_names <- config::get(file = config_path, "influeza_types")
    names <- config::get(file = config_path, "influeza_columns_names")
    
    i <- 1
    for (column in column_names) {
      positive_cases <- influeza_fci_data[which(stringr::str_detect(
        influeza_fci_data$virusdetectadosvegeneral, column)), ]
      positive_cases_fci <- group_columns_total(positive_cases, 
                                                event_name = column,
                                                col_names = "semanaepidemiologicavegeneral",
                                                wt_percentage = TRUE, 
                                                total_cases = nrow(positive_cases), 
                                                event_label = names[i])
      distribution_influenza <- rbind(distribution_influenza,
                                      positive_cases_fci)
      i <- i + 1
    }
  }
  
  if (nrow(influenza_other_virs_data) > 0) {
    column_names <- config::get(file = config_path, "influeza_columns")
    names <- config::get(file = config_path, "influeza_columns_names")
    positive_value <- "DETECTADO"
    influenza_other_virs_data <- dplyr::rename(influenza_other_virs_data,
                                          "semanaepidemiologicavegeneral" = se)
    i <- 1
    for (column in column_names) {
      positive_cases <- influenza_other_virs_data[eval(
        parse(text = paste0("influenza_other_virs_data$",
                            column, " == ", '"', positive_value, '"'))), ]
      positive_cases_other <- group_columns_total(positive_cases,
                                                  col_names = "semanaepidemiologicavegeneral",
                                                  event_name = column,
                                                  wt_percentage = TRUE,
                                                  total_cases = nrow(positive_cases),
                                                  event_label = names[i])
      distribution_influenza <- rbind(distribution_influenza, positive_cases_other)
      i <- i + 1 
    }
  }
  
  distribution_influenza <- group_columns_total(distribution_influenza,
                                                event_name = "",
                                                col_names = c("semanaepidemiologicavegeneral",
                                                              "etiqueta"),
                                                wt_percentage = TRUE,
                                                total_cases = sum(distribution_influenza$casos), 
                                                event_label = "",
                                                sum_cases = TRUE)
  
  return(distribution_influenza)
}

get_percentage_influenza <- function(fci_data, other_virs_data) {
  config_path <- "C:/Users/geral/Documents/TRACE/labrep/inst/extdata/config.yml"
  influeza_fci_data <- fci_data
  influenza_other_virs_data <- other_virs_data
  percentage_influenza  <- data.frame()
  
  if (nrow(influeza_fci_data) > 0) {
    influeza_fci_data <- dplyr::rename(influeza_fci_data,
                                     "semanaepidemiologicavegeneral" = se)
    percentage_influenza <- group_columns_total(influeza_fci_data,
                                 col_names = "semanaepidemiologicavegeneral",
                                 event_name = "",
                                 wt_percentage = TRUE,
                                 total_cases = nrow(influeza_fci_data),
                                 event_label = "")
  }
  
  if (nrow(other_virs_data) > 0) {
    cases  <- group_columns_total(other_virs_data,
                                  col_names = "semanaepidemiologicavegeneral",
                                  event_name = "",
                                  wt_percentage = TRUE,
                                  total_cases = nrow(other_virs_data),
                                  event_label = "")
    percentage_influenza  <- rbind(percentage_influenza, cases)
  }
}

get_cases_tosferina <- function(report_data, result = "positivo", column = "semana_epidemiologica") {
  report_data$interpretacion_del_resultado <- epitrix::clean_labels(report_data$interpretacion_del_resultado)
  
  cases <- report_data[-which(is.na(report_data$interpretacion_del_resultado)), ]
  cases <- cases[-grep("muestra", cases$interpretacion_del_resultado), ]
  
  cases_grouped <- cases %>% dplyr::group_by(dplyr::across(dplyr::all_of(column))) %>% dplyr::summarise(total_casos = dplyr::n(), .groups = "drop")
  
  positive_cases <- cases[grep(result, cases$interpretacion_del_resultado), ]
  
  positive_grouped <- positive_cases %>% dplyr::group_by(dplyr::across(dplyr::all_of(column))) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  
  data_grouped <- merge(x = positive_grouped,y = cases_grouped, 
                        by = column, all.x = TRUE)
  
  data_grouped <- data_grouped %>% dplyr::mutate(porcentaje = round((data_grouped$casos/data_grouped$total_casos)*100, 1))
  
  if (column == "semana_epidemiologica") {
    for (week in setdiff(1:52, data_grouped$semana_epidemiologica)) {
      new_row <- data.frame(semana_epidemiologica = week, casos = 0, porcentaje  = 0, total_casos = 0)
      data_grouped <- rbind(data_grouped, new_row)
    }
  }
  
  if (column == "grupo_edad") {
    config_path <- "C:/Users/geral/Documents/TRACE/tmp/sivirep/inst/extdata/config.yml"
    categorie_labels <- config::get(file = config_path, "age_categorie_labels_tosferina")
    for (label in categorie_labels) {
      if (!any(data_grouped == label) || is.na(any(data_grouped == label))) {
        new_row <- data.frame(grupo_edad = label, casos = 0, porcentaje  = 0, total_casos = 0)
        data_grouped <- rbind(data_grouped, new_row)
      }
    }
    #data_grouped <- data_grouped[-grep("SD", data_grouped$grupo_edad), ]
  }
  
  data_grouped <- data_grouped %>% dplyr::mutate(evento = "tosferina", etiqueta = "tosferina", interpretacion_del_resultado = "Positivo para Bordetella")
  
  return(data_grouped)
}

get_results_tosferina <- function(report_data, results = "positivo", columns =  c("semana_epidemiologica", "interpretacion_del_resultado")) {
  report_data$interpretacion_del_resultado <- epitrix::clean_labels(report_data$interpretacion_del_resultado)
  report_data$interpretacion_del_resultado[grep("positivo", report_data$interpretacion_del_resultado)] <- "Positivo para Bordetella"
  report_data$interpretacion_del_resultado[grep("negativo", report_data$interpretacion_del_resultado)] <- "Negativo para Bordetella"
  
  cases <- report_data[grep("Positivo", report_data$interpretacion_del_resultado) 
                       || grep("Negativo", report_data$interpretacion_del_resultado), ]
  
  cases <- cases[-which(is.na(cases$interpretacion_del_resultado)), ]
  cases <- cases[-grep("muestra", cases$interpretacion_del_resultado), ]
  
  total_cases <- nrow(cases)
  
  if ("grupo_edad" %in% columns) {
    cases <- cases[-grep("SD", cases$grupo_edad), ]
  }
  
  
  data_grouped <- cases %>% dplyr::group_by(dplyr::across(dplyr::all_of(columns))) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  data_grouped <- data_grouped %>% dplyr::mutate(porcentaje = round((data_grouped$casos/total_cases)*100, 1))
  
  if ("genero" %in% columns) {
    new_row <- data.frame(genero = "Femenino", casos = 0, porcentaje = 0, interpretacion_del_resultado = "Positivo para Bordetella")
    data_grouped <- rbind(data_grouped, new_row)
  }
  
  data_grouped <- data_grouped %>% dplyr::mutate(evento = "tosferina", etiqueta = "tosferina")
  
  return(data_grouped)
}

get_table_epiweek_tosferina <- function(report_data) {
  report_data <- report_data[order(report_data$semana_epidemiologica), ]
  table_data <- data.frame(SE = report_data$semana_epidemiologica, positivos = report_data$porcentaje)
  return(table_data)
}