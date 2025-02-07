#' @title Obtener los colores de los grupos de edad
#' @export
get_colors_age_groups <- function(order = FALSE,
                                  hex_cods = FALSE,
                                  include_sars = FALSE) {
  colors <- c("Adenovirus" = "#9E4B9F",
              "Rinovirus" = "#145765",
              "Bocavirus" = "#D49392",
              "Parainfluenza" = "#64439B",
              "Influenza B" = "#B94846",
              "Metapneumovirus" = "#87C762",
              "VSR" = "#2274BB",
              "H1N1" = "#7451c0",
              "H1N1 2009" = "#9DB2D0",
              "H3N2" = "#7dcea0",
              "A no subtipificado" = "#F4802D",
              "Otros Virus" = "#4E82BE")
  if (include_sars) {
    colors <- c(colors, "SARS CoV 2" = "#e05f55")
  }
  if (order) {
    colors <- colors[order(names(colors))]
  }
  if (hex_cods) {
    color <- unname(colors)
  }
  return(colors)
}



#' @title Obtener configuración de colores para el periodo epidemiológico
#'
#' @description
#' Devuelve una lista con los colores asignados a diferentes elementos del gráfico, 
#' incluyendo líneas, títulos de ejes, líneas verticales y tipos de virus.
#'
#' @return Lista con los colores definidos
#' @export
get_color_periodo_epidemiologico <- function() {
  return(list(
    color_linea = "#E97132",
    color_axis_titles = "#595959",
    color_vertical_lines = "black",
    color_a_h1n1_pdm09 = "#8064A2",
    color_a_no_subtipificado = "#4BACC6",
    color_a_h3 = "#F79646",
    color_influenza_b = "#2C4D75",
    color_parainfluenza = "#772C2A",
    color_vsr = "#5F7530",
    color_adenovirus = "#4D3B62",
    color_metapneumovirus = "#2C4D75",
    color_rinovirus = "#B65708",
    color_bocavirus = "#729ACA",
    color_otros_virus = "#4F81BD"
  ))
}


#' @title Obtener configuración de los ejes para el periodo epidemiológico
#'
#' @description
#' Devuelve una lista con los valores máximos de los ejes Y, el factor de escala 
#' y los parámetros de ancho de barra y línea.
#'
#' @return Lista con la configuración de los ejes:
#' - `y_axis1_max_value`: Valor máximo del primer eje Y.
#' - `y_axis2_max_value`: Valor máximo del segundo eje Y.
#' - `scaling_factor`: Factor de escala entre los dos ejes.
#' - `bar_width`: Ancho de las barras.
#' - `line_width`: Ancho de la línea.
#' @export
get_axis_config_periodo_epidemiologico <- function() {
  return(list(
    y_axis1_max_value = 700,
    y_axis2_max_value = 70,
    scaling_factor = 700 / 70,
    bar_width = 0.4,
    line_width = 0.7
  ))
}



#' @title Generar etiquetas de texto para los periodos epidemiológicos
#'
#' @description
#' Extrae los años únicos del dataset y genera etiquetas en formato "AÑO <año>".
#'
#' @param dataset_epiTime Dataset con una columna `ano` que contiene los años.
#' @return Vector de texto con etiquetas para cada año.
#' @export
get_text_labels_periodo_epidemiologico <- function(dataset_epiTime) {
  # Extract unique years from the dataset
  unique_years <- sort(unique(dataset_epiTime$ano))
  
  # Generate the annotation text dynamically
  annotation_text <- paste("AÑO", unique_years)
  
  return(annotation_text)
}

