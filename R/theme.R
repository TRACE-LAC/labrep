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



# PLOT 1 PERIODOS EPIDEMIOLOGICO 2022-2024

# Función para obtener los colores
get_color_periodo_epidemiologico<- function() {
  return(list(
    COLOR_LINEA = "#E97132",
    COLOR_AXIS_TITLES = "#595959",
    COLOR_VERTICAL_LINES = "black",
    COLOR_a_h1n1_pdm09 = "#8064A2",
    COLOR_a_no_subtipificado = "#4BACC6",
    COLOR_a_h3 = "#F79646",
    COLOR_influenza_b = "#2C4D75",
    COLOR_parainfluenza = "#772C2A",
    COLOR_vsr = "#5F7530",
    COLOR_adenovirus = "#4D3B62",
    COLOR_metapneumovirus = "#2C4D75",
    COLOR_rinovirus = "#B65708",
    COLOR_bocavirus = "#729ACA",
    COLOR_otros_virus = "#4F81BD"
  ))
}

# Función para obtener configuración de los ejes
get_axis_config_periodo_epidemiologico <- function(periodo_epi) {
  return(list(
    Y_AXIS1_VALOR_MAX = 700,
    Y_AXIS2_VALOR_MAX = 70,
    scaling_factor = 700 / 70,
    ANCHO_BARRAS = 0.4,
    ANCHO_LINEA = 0.7
  ))
}

get_text_labels_periodo_epidemiologico <- function() {
  return(list(
    Y_AXIS1_NAME = "NÚMERO DE CASOS POSITIVOS",
    X_AXIS_NAME = "PERÍODO EPIDEMIOLÓGICO",
    ANNOTATION_TEXT = c("AÑO 2022", "AÑO 2023", "AÑO 2024")
  ))
}

