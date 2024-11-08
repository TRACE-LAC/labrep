#' @title Graficar la distribución de casos por grupos de edad
#' @export
plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  colors <- c("Adenovirus" = "#AC6DAD",
              "Rinovirus" = "#FCB814",
              "Bocavirus" = "#D49392",
              "Parainfluenza" = "#64439B",
              "Influenza b" = "#B94846",
              "Metapneumovirus" = "#87C762",
              "VSR" = "#2274BB",
              "H1N1" = "#F4802D",
              "H1N1 2009" = "#9DB2D0",
              "H3N2" = "#145765",
              "A no subtipificado" = "#EEEA3D",
              "Otros Virus" = "#19AFE5")
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  if (include_sars) {
    colors <- c(colors, "SARS CoV 2" = "#145765",
                "H3N1" = "#19AFE5")
  } else {
    colors <- c(colors, "H3N1" = "#145765")
  }
  plot <- ggplot2::ggplot(report_data,
                          ggplot2::aes_string(x =
                                                factor(report_data[[var_x]],
                                                       levels =
                                                      category_labels),
                                              y = var_y,
                                              fill = var_fill)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format())
      }
    } +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios")
  return(plot)
}

#' @title Graficar la distribución de casos de la vigilancia
#' @export
plot_distribution_surveillance <- function(report_data,
                                           var_x = "casos",
                                           var_y = "porcentaje",
                                           var_fill = "etiqueta") {
  colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(9)
  plot <- ggplot2::ggplot(report_data, ggplot2::aes_string(x = var_x,
                                                           y = var_y,
                                                           fill = var_fill)) + 
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                labels = scales::percent_format()) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_manual(values = colors, name = "Virus respiratorios")
  return(plot)
}

#' @title Graficar la distribución de casos por semana epidemiológica
#' @export
plot_distribution_epiweek <- function(report_data,
                                      var_x = "semanaepidemiologicavegeneral",
                                      var_y = "casos",
                                      var_fill = "etiqueta",
                                      influenza = FALSE,
                                      positives = NULL) {
  if (!influenza) {
    colors <- c("Adenovirus" = "#AC6DAD",
                "Rinovirus" = "#FCB814",
                "Bocavirus" = "#D49392",
                "Parainfluenza" = "#64439B",
                "Influenza b" = "#B94846",
                "H3N2" = "#19AFE5",
                "Metapneumovirus" = "#87C762",
                "VSR" = "#2274BB",
                "H1N1" = "#F4802D",
                "H1N1 2009" = "#9DB2D0")
  } else {
    colors <- c("H3N2" = "#145765",
                "H1N1 2009" = "#F4802D",
                "A no subtipificado" = "#EEEA3D",
                "H1N1" = "#19AFE5",
                "Influenza B" = "#B94846")
  }
  plot_epiweek <- ggplot2::ggplot(report_data) +
    ggplot2::geom_col(ggplot2::aes_string(x = var_x,
                                          y = var_y,
                                          fill = var_fill), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("\nSemana epidemiologica\n") +
    ggplot2::ylab("Numero de casos\n") +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, 2)) +
    ggplot2::scale_fill_manual(values = colors, name = "Virus respiratorios")
  if (!is.null(positives)) {
    plot_epiweek <- plot_epiweek +
      ggplot2::geom_line(data = positives,
                         ggplot2::aes_string(x = var_x,
                                             y = "porcentaje"),
                         stat = "identity",
                         linetype = "dashed",
                         color = "black",
                         size = 0.8,
                         group = 1) +
      ggplot2::scale_y_continuous(sec.axis =
                                    ggplot2::sec_axis(~. * 0.0055,
                                                      labels =
                                                    scales::percent_format(
                                                        ),
                                                      breaks = seq(0,
                                                                   1,
                                                                   0.1))) +
      ggplot2::theme(text = ggplot2::element_text(size = 14,
                                                  family = "Montserrat"))
  }
  return(plot_epiweek)
}

#' @title Graficar la distribución acumulada de los casos
#' @export
plot_cumulative_proportion <- function(data_proportion) {
  cases <- data_proportion$casos
  labels <- paste(data_proportion$etiqueta,
                  paste0(data_proportion$porcentaje, "%"))
  colors <- c("Adenovirus" = "#AC6DAD",
              "Rinovirus" = "#FCB814",
              "Bocavirus" = "#D49392",
              "Parainfluenza" = "#64439B",
              "Influenza b" = "#B94846",
              "H3N1" = "#145765",
              "Metapneumovirus" = "#87C762",
              "VSR" = "#2274BB",
              "H1N1" = "#F4802D",
              "H1N1 2009" = "#9DB2D0")
  if (!any(cases == 0)) {
    index <- which(cases == 0)
    cases <- cases[-index]
    labels <- labels[-index]
  }
  par(family = "Montserrat")
  plotrix::pie3D(cases,
                 mar = rep(1.75, 4),
                 col = colors,
                 labels = labels,
                 explode = 0.1,
                 border = "white",
                 labelcex = 1.4,
                 radius = 0.6,
                 start = 0.9)
}

#' @title Graficar la distribución de casos de Tosferina
#' @export
plot_results_tosferina <- function(report_data,
                                   column = "semana_epidemiologica",
                                   label_x = "Semana epidemiologica",
                                   positives = NULL,
                                   show_values = FALSE) {
  colors <- c("Positivo para Bordetella" = "#F4802D",
              "Negativo para Bordetella" = "#145765")
  plot <- ggplot2::ggplot(report_data) +
    ggplot2::geom_bar(ggplot2::aes_string(x = column,
                                          y = "casos",
                                          fill =
                                            "interpretacion_del_resultado"),
                      alpha = 0.9,
                      width = 0.5,
                      stat = "identity") +
    ggplot2::theme_classic() +
    ggplot2::xlab(label_x) +
    ggplot2::ylab("Numero de muestras analizadas\n") +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Interpretación del resultado")
  if (!is.null(positives)) {
    positives[[column]]  <- toupper(positives[[column]])
    plot <- plot +
      ggplot2::geom_line(data = positives,
                         ggplot2::aes_string(x = column,
                                             y = "porcentaje"),
                         stat = "identity",
                         color = "#F99D00",
                         size = 0.8,
                         group = 1) +
      ggplot2::scale_y_continuous(sec.axis =
                                    ggplot2::sec_axis(~. * 0.0055,
                                                      labels =
                                                      scales::percent_format(
                                                        ),
                                                      breaks = seq(0, 1,
                                                                   0.1))) +
      ggplot2::theme(text = ggplot2::element_text(size = 14,
                                                  family = "Montserrat"))
  }
  return(plot)
}

#' @title Graficar la tabla con la distribución de casos por semana
#' epidemiológica
#' @export
plot_table_vrs_epiweek <- function(data_epiweek) {
  table_epiweek <-
    knitr::kable(data_epiweek,
                 col.names = c("Semana Epidemiologica", "% Positivos"),
                 align = "c",
                 longtable = TRUE,
                 caption = "Positividad de virus respiratorios por semana 
                 epidemiológica, Bogotá 2024 \n ") %>%
    kableExtra::row_spec(0, bold = TRUE,
                         color = "white", background = "#145765") %>%
    kableExtra::row_spec(seq(2, nrow(data_epiweek), by = 2),
                         background = "#D4EFFB") %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(2, border_right = TRUE) %>%
    kableExtra::kable_styling()
  return(table_epiweek)
}

#' @title Graficar la tabla con la distribución de casos de Tosferna por semana
#' epidemiológica
#' @export
plot_table_epiweek_tosferina <- function(data_epiweek, distribution_epiweek) {
  table_epiweek <-
    knitr::kable(data_epiweek,
                 longtable = TRUE,
                 col.names = c("SE", "% Positivos"),
                 align = "c",
                 caption = "Positividad de tosferina por semana epidemiológica,
                 Bogotá 2022") %>%
    kableExtra::row_spec(0, bold = TRUE,
                         color = "white", background = "#145765") %>%
    kableExtra::row_spec(seq(2, nrow(distribution_epiweek), by = 2),
                         background = "#D4EFFB") %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(2, border_right = TRUE) %>%
    kableExtra::kable_styling()
  return(table_epiweek)
}
