
#' Hent produksjonsdata fra NVE
#'
#' Funksjon for å hente vannkraftsproduksjon for alle kraftverk i Norge
#'
#' @return data.frame med oversikt over produksjon fra alle kraftverk i Norge.
#' @export
#'
#' @examples
#' # hent_produksjonsdata

hent_produksjonsdata <- function() {
  jsonlite::fromJSON("https://www.nve.no/umbraco/api/Powerplant/GetHydroPowerPlantsInOperation")
}

#' Hent kraftproduksjon for ett selskap
#'
#' @param df data.frame som må inneholde minst disse kolonnene: Navn og Mid_81_10, der
#' Navn er navnet på kraftverket, og Mid_81_10 er kraftproduksjonen.
#' @param kraftverk Navn på kraftverket man ønsker produksjon til. Kan være en vektor
#' med flere navn.
#'
#' @return Kraftproduksjon for det eller de angitte kraftverkene
#' @export
#'
#' @examples
#' hent_produksjon_for_kraftverk(df, kraftverk = "Odberg)
hent_produksjon_for_kraftverk <- function(df, kraftverk = "Aall-Ulefoss") {
  if(!(kraftverk %in% df$Navn)) {
    stop("Kraftverket finnes ikke i dataen. Sjekk at du har skrevet riktig. ")
  }
  df %>%
    dplyr::filter(Navn %in% kraftverk) %>%
    dplyr::select(MidProd_81_10) %>%
    dplyr::pull()
}
