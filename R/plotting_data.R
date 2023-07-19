plot_distribution_age_group <- function(report_data, 
                                       var_x = "grupo_edad", 
                                       var_y = "porcentaje", 
                                       var_fill = "etiqueta",
                                       stacked_percentage = TRUE,
                                       include_sars = FALSE) {
  # colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(9)
  # colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(2, "Set2"))(10)
  colors <- c("Adenovirus" = "#AC6DAD",
              "Rinovirus" = "#FCB814",
              "Bocavirus" = "#D49392",
              "Parainfluenza" = "#64439B",
              "Influenza b" = "#B94846",
              "H3N1" = "#19AFE5",
              "Metapneumovirus" = "#87C762",
              "VSR" = "#2274BB",
              "H1N1" = "#F4802D",
              "H1N1 2009" = "#9DB2D0"
              
  )
  
  if (include_sars) {
    colors <- c(colors, "SARS CoV 2" = "#145765")
  }
  
  plot <- ggplot2::ggplot(report_data, ggplot2::aes_string(x = var_x , y = var_y, fill = var_fill)) + 
    ggplot2::geom_bar(position = "fill", stat = "identity") + 
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent_format()) 
    }
    } +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::scale_fill_manual(values = colors, name = "Virus respiratorios")
  return(plot)
}

plot_distribution_surveillance <- function(report_data,
                                           var_x = "casos",
                                           var_y = "porcentaje",
                                           var_fill = "etiqueta") {
  colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(9)
  plot <- ggplot2::ggplot(report_data, ggplot2::aes_string(x = var_x , y = var_y, fill = var_fill)) + 
    ggplot2::geom_bar(position = "fill", stat = "identity") + 
    ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent_format()) + 
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::scale_fill_manual(values = colors, name = "Virus respiratorios")
  
  return(plot)
}

plot_distribution_epiweek <- function(report_data,
                                      var_x = "semanaepidemiologicavegeneral",
                                      var_y = "casos",
                                      var_fill = "etiqueta",
                                      influenza = FALSE) {
  
  if (!influenza) {
    colors <- c("Adenovirus" = "#AC6DAD",
                "Rinovirus" = "#FCB814",
                "Bocavirus" = "#D49392",
                "Parainfluenza" = "#64439B",
                "Influenza b" = "#B94846",
                "H3N1" = "#19AFE5",
                "Metapneumovirus" = "#87C762",
                "VSR" = "#2274BB",
                "H1N1" = "#F4802D",
                "H1N1 2009" = "#9DB2D0"
                
    )
  }
  else {
    colors <- c("H3N1" = "#145765",
                "H1N1 2009" = "#F4802D",
                "A no subtipificado" = "#EEEA3D"
                
    )
  }
  
  
  plot <- ggplot2::ggplot(report_data) +
    ggplot2::geom_col(ggplot2::aes_string(x = var_x,
                                          y = var_y,
                                          fill = var_fill), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("\nSemana epidemiologica\n") +
    ggplot2::ylab("Numero de casos\n") +
    ggplot2::scale_fill_discrete(name = "") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, 1)) +
    ggplot2::scale_fill_manual(values = colors, name = "Virus respiratorios")
  return(plot)
}

plot_cumulative_proportion <- function(data_proportion) {
  cases <- data_proportion$casos
  labels <- paste(data_proportion$etiqueta,
                  paste(data_proportion$porcentaje,
                       "%", sep = ""))
  colors <- c("Adenovirus" = "#AC6DAD",
              "Rinovirus" = "#FCB814",
              "Bocavirus" = "#D49392",
              "Parainfluenza" = "#64439B",
              "Influenza b" = "#B94846",
              "H3N1" = "#19AFE5",
              "Metapneumovirus" = "#87C762",
              "VSR" = "#2274BB",
              "H1N1" = "#F4802D",
              "H1N1 2009" = "#9DB2D0"
              
  )
  
  if (length(which(cases == 0)) > 0) {
    index <- which(cases == 0)
    cases <- cases[-index]
    labels <- labels[-index]
  }
  
  # pie(cases,
  #     labels = labels,
  #     main = "Gráfico de Torta", border = "white", col = colors,
  #     cex = 0.6, mgp = c(2.9, 0.9, 0))
  plotrix::pie3D(cases,
                 mar = rep(1.75, 4),
                 col = colors,
                 labels = labels,
                 explode = 0.2,
                 border = "white",
                 labelcex = 1)
}

plot_results_tosferina <- function(report_data, 
                                   positive_cases = NULL, 
                                   column = "semana_epidemiologica", 
                                   label_x = "Semana epidemiologica",
                                   stack_line = TRUE,
                                   show_values = FALSE) {
  colors <- c("Positivo para Bordetella" = "#F4802D",
              "Negativo para Bordetella" = "#145765")
  plot <- sivirep::plot_variable(report_data,
                                 var_x = column, 
                                 var_y = "casos", 
                                 var_fill = "interpretacion_del_resultado",
                                 etiqueta_x = label_x,
                                 etiqueta_y = "Numero de muestras analizadas",
                                 pos_leyenda = "bottom",
                                 most_val = show_values,
                                 ancho_barra = 0.5,
                                 fuente_data = "Fuente: Sistema de información Laboratorio de Salud Pública (SILASP), corte SE52 de 2022") +  
    ggplot2::scale_fill_manual(values = colors)
  
  if (stack_line) {
    plot <- plot + ggplot2::geom_line(data = positive_cases, ggplot2::aes_string(x = column, y = "porcentaje"), stat = "identity",color = "#F99D00", size = 1.5) +
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~.*.01, labels = scales::percent_format(), breaks = seq(0, 1, .1)))
  }
  
  return(plot)
}