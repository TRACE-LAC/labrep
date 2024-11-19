#' @title Graficar la distribución de casos por grupos de edad
#' @export
plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
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
  if (any(cases == 0)) {
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
                 Bogotá 2024") %>%
    kableExtra::row_spec(0, bold = TRUE,
                         color = "white", background = "#145765") %>%
    kableExtra::row_spec(seq(2, nrow(distribution_epiweek), by = 2),
                         background = "#D4EFFB") %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(2, border_right = TRUE) %>%
    kableExtra::kable_styling()
  return(table_epiweek)
}

#' @title Graficar el tiempo epidemiológico historico
#' @export
plot_historic_epi_time <- function(stacked_data, tabla,
                                   line_data) {
  scaling_factor <- 700 / 70
  # Plot the figure
  ggplot2::ggplot() +
    # Stacked bar chart
    ggplot2::geom_bar(data = stacked_data, 
             ggplot2::aes(x = YearWeek, y = Cases, fill = Virus_Type), 
             stat = "identity",
             width = 0.4) +
    
    # Line chart for % positivity with scaling applied
    ggplot2::geom_line(data = line_data, 
                       ggplot2::aes(x = YearWeek, 
                  y = Percent_Positivity * scaling_factor, 
                  color = "Positivity Rate", 
                  group = 1),
              color = "#E97132",
              linewidth = 0.7) +
    
    # Scale and labels with specified y-axis breaks
    ggplot2::scale_y_continuous(name = "NÚMERO DE CASOS POSITIVOS",
                       limits = c(-500, 700), breaks = seq(0, 700, by = 100),
                       sec.axis = ggplot2::sec_axis(~ . / scaling_factor, 
                                           breaks = seq(0, 70, by = 10), 
                                           labels = function(x) sprintf("%.1f", x))
    ) +
    ggplot2::scale_x_discrete(labels = tabla$periodo_epidemiologico)+
    ggplot2::scale_fill_manual(values = c(
      "a_h1n1_pdm09" = "#8064A2",       # Light blue for A(H1N1)pdm09
      "a_no_subtipificado" = "#4BACC6", # Purple for A no subtipificado
      "a_h3" = "#F79646",              # Green for A(H3)
      "influenza_B" = "#2C4D75",       # Dark gray for Influenza B
      "adenovirus" = "#4D3B62",         # Dark teal for Adenovirus
      "metapneumovirus" = "#2C4D75",    # Dark purple for Metapneumovirus
      "rinovirus" = "#B65708",          # Dark green for Rinovirus
      "bocavirus" = "#729ACA",          # Blue for Bocavirus
      "otros_virus" = "#4F81BD",        # Blue for Otros Virus
      "parainfluenza" = "#772C2A",      # Brown for Parainfluenza
      "vsr" = "#5F7530",                # Dark green for VSR
      "nueva_columna" = "black"         # Black for nueva_columna (appears as line in legend)
    )) +
    ggplot2::labs(x = "PERÍODO EPIDEMIOLOGICO", fill = NULL, color = NULL) +
    
    # Customize the grid lines
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, size = 7,
                                          margin = ggplot2::margin(t = -255, b=-5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, b=-10),
                                           size = 8, face = "bold",
                                           color = "#595959"),
      axis.title.y = ggplot2::element_text(hjust = 0.75, size = 8,
                                           face = "bold", color = "#595959"),
      panel.grid.major.x = ggplot2::element_blank(),         # Remove vertical major grid lines
      panel.grid.minor.x = ggplot2::element_blank(),         # Remove vertical minor grid lines
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key.height = ggplot2::unit(0.02, "lines"),
      legend.text = ggplot2::element_text(size = 7)
    ) +
    # Agregar líneas verticales con altura ajustable usando geom_segment
    ggplot2::geom_segment(ggplot2::aes(x = 13.5, xend = 13.5, y = -25, yend = 700),
                          color = "black", linewidth = 0.65) +
    ggplot2::geom_segment(ggplot2::aes(x = 26.5, xend = 26.5, y = -25, yend = 700),
                          color = "black", linewidth = 0.65) +
    ggplot2::annotate("text", x = c(6, 19, 31), y = -35, label = c("2022",
                                                                   "2023",
                                                                   "2024"),
                      size = 2.4, fontface = "bold") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 3,
        byrow = TRUE
      ),
      color = "none") + 
    ggplot2::annotate("segment", x = 21, xend = 22.2, y = -150, yend = -150,
                      color = "#E97132", linewidth = 0.7) +
    ggplot2::annotate("text", x = 22.5, y = -150, label = "% DE POSITIVIDAD",
                      hjust = 0, color = "black", size= 2) 
}

#' @title Graficar la tabla de la leyenda
#' @export
plot_table_legend <- function(report_data,
                              include_sars = FALSE) {
  report_data$cs <- ""
  report_data <- report_data %>%
    dplyr::arrange(.data$etiqueta) %>%
    dplyr::relocate(.data$cs, .after = .data$etiqueta)
  colors <- get_colors_age_groups(order = TRUE,
                                  hex_cods = TRUE,
                                  include_sars = include_sars)
  col_names <- names(report_data)
  table <- knitr::kable(report_data,
                        col.names = NULL,
                        align = "c",
                        longtable = TRUE) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      latex_options = c("bordered", "hold_position"),
      font_size = 9
    )  %>%
    kableExtra::column_spec(2, background = colors) %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(length(col_names), border_right = TRUE) %>%
    kableExtra::column_spec(column = seq(3, length(col_names)),
                            width = "1.6cm") %>%
    kableExtra::column_spec(length(col_names), border_right = TRUE,
                            width = "1.7cm")
  return(table)
}

