cleansing_viral_circulation_data <- function(viral_circulation_data) {
  names(viral_circulation_data) <- epitrix::clean_labels(names(viral_circulation_data))
  return(viral_circulation_data)
}

clean_data_other_viruses <- function(report_data) {
  
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
  report_data$rangodeedadvegeneral <- epitrix::clean_labels(
    report_data$rangodeedadvegeneral)
  return(report_data)
}