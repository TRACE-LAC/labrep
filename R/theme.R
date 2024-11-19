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
