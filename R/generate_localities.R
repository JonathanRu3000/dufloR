#' Generiert synthetische Lokalitaetsdaten
#'
#' Diese Funktion erstellt eine Lookup-Tabelle mit synthetischen Lokalitaetsdaten
#' fuer Tests, Demos oder Entwicklungen. Die Daten enthalten Namen,
#' Geometrietypen, Flaechen, Hoehenangaben und Koordinaten (inkl. WKT-Geometrien).
#'
#' Die Zahl der Lokalitaeten ist auf maximal 20 beschraenkt.
#'
#' @param n_lokalitaeten Integer. Anzahl der zu generierenden Lokalitaeten (max. 20). Default ist 20.
#' @param seed Integer. Seed fuer den Zufallszahlengenerator. Default ist 123.
#'
#' @return Ein tibble mit den generierten Lokalitaetsdaten.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_pad
#' @importFrom tibble tibble
#' @export
generate_localities <- function(n_lokalitaeten = 20, seed = 123) {
  # --------------------------------------------
  # ✅ Check: maximal 20 Lokalitaeten zulassen
  # --------------------------------------------
  if (n_lokalitaeten > 20) {
    stop("Fehler: Es koennen maximal 20 Lokalitaeten auf einmal erzeugt werden.")
  }

  set.seed(seed)

  # --------------------------------------------
  # 📌 Pool an Ortsnamen
  # --------------------------------------------
  locality_vector <- c(
    "Berliner Stadtwald", "Lueneburger Heide", "Muenchner Isarauen", "Rheinaue Bonn",
    "Teutoburger Wald", "Schwarzwald Nord", "Hainich Nationalpark", "Bayerischer Wald",
    "Elbtalauen", "Oberrheinische Tiefebene", "Vogelsberg", "Fraenkische Alb",
    "Allgaeuer Alpen", "Eifel Hochflaeche", "Hunsrueck", "Taunus Suedhang",
    "Spreewald", "Kyffhaeuser", "Uckermark Seen", "Westerwald"
  )

  # Zufaellig auswaehlen
  locality_vector <- sample(locality_vector, n_lokalitaeten, replace = FALSE)

  # Hoehen generiern
  heights <- generate_height_pair(n_lokalitaeten)

  # --------------------------------------------
  # 📌 Lookup-Tabelle erzeugen
  # --------------------------------------------
  lookup_lokalitaet <- tibble::tibble(
    lokalitaetID = paste0("ID_Lokalitaet", stringr::str_pad(1:n_lokalitaeten, 3, pad = "0")),
    lokalitaetName = locality_vector
  ) %>%
    dplyr::mutate(
      lokalitaetTyp = sample(
        c("Polygonflaeche", "Rasterfeld", "Transektlinie", "Punkt"),
        n_lokalitaeten, replace = TRUE
      )
    ) %>%
    dplyr::mutate(
      lokalitaetFlaeche = dplyr::case_when(
        lokalitaetTyp %in% c("Polygonflaeche", "Rasterfeld") ~ generate_area_vector(n_lokalitaeten),
        TRUE ~ NA_real_
      ),
      lokalitaetLaenge = dplyr::case_when(
        lokalitaetTyp == "Transektlinie" ~ generate_length_vector(n_lokalitaeten),
        TRUE ~ NA_real_
      ),
      lokalitaetHoeheMin = heights$lokalitaetHoeheMin,
      lokalitaetHoeheMax = heights$lokalitaetHoeheMax,
      lokalitaetHoeheBezugssystem = NA_character_,
      lokalitaetKoordinaten = dplyr::case_when(
        lokalitaetTyp == "Punkt" ~ generate_wkt_point_vector(n_lokalitaeten),
        lokalitaetTyp == "Transektlinie" ~ generate_wkt_linestring_vector(n_lokalitaeten),
        lokalitaetTyp == "Rasterfeld" ~ generate_wkt_polygon_vector(n_lokalitaeten),
        lokalitaetTyp == "Polygonflaeche" ~ ifelse(
          runif(n_lokalitaeten) < 0.7,
          generate_wkt_polygon_vector(n_lokalitaeten),
          generate_wkt_multipolygon_vector(n_lokalitaeten)
        ),
        TRUE ~ NA_character_
      ),
      lokalitaetKoordinatenCRS = sample(
        c("EPSG:4326", "EPSG:25832", "EPSG:31468"),
        n_lokalitaeten, replace = TRUE
      ),
      lokalitaetKoordinatenUnschaerfe = ifelse(
        sample(c(TRUE, FALSE), n_lokalitaeten, replace = TRUE),
        NA, runif(n_lokalitaeten, 1, 20000)
      ),
      lokalitaetKoordinatenCentroid = ifelse(
        sample(c(TRUE, FALSE), n_lokalitaeten, replace = TRUE),
        NA, generate_wkt_point_vector(n_lokalitaeten)
      ),
      lokalitaetKoordinatenCentroidUnschaerfe = dplyr::case_when(
        !is.na(lokalitaetKoordinatenCentroid) ~ ifelse(
          sample(c(TRUE, FALSE), n_lokalitaeten, replace = TRUE),
          NA, runif(n_lokalitaeten, 1, 20000)
        )
      ),
      lokalitaetKoordinatenGenerierung = sample(
        c(NA, "Planzeiger", "GPS", "Satellitensystem", "Digitalkarte", "Digitalisierungstisch"),
        n_lokalitaeten, replace = TRUE
      ),
      lokalitaetAggregiert = sample(c(TRUE, FALSE), n_lokalitaeten, replace = TRUE)
    )

  return(lookup_lokalitaet)
}
