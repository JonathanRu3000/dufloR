#' Generate ISO 8601 Date Strings or Intervals
#'
#' Erzeugt eine Mischung aus einzelnen ISO-8601-Datumsangaben und Zeitintervallen.
#'
#' @param n Anzahl der Werte, die erzeugt werden sollen.
#' @param start_date Startdatum als POSIXct.
#' @param end_date Enddatum als POSIXct.
#' @param fraction_range Anteil an Werten, die als Zeitspanne (von/bis) erzeugt werden.
#' @param formats Zeichenvektor gueltiger ISO8601-Formate (z. B. Jahr, Monat, Datum, Datum+Zeit).
#'
#' @return Ein Character-Vektor mit ISO-8601-konformen Datumsangaben.
#' @export
#' 
generate_mixed_iso8601 <- function(
    n,
    start_date = as.POSIXct("2000-01-01 00:00:00", tz = "UTC"),
    end_date = as.POSIXct("2030-12-31 23:59:59", tz = "UTC"),
    fraction_range = 0.3,
    formats = c("%Y", "%Y-%m", "%Y-%m-%d", "%Y-%m-%dT%H:%M:%SZ")
) {
  random_timestamp <- function() {
    as.POSIXct(runif(1, as.numeric(start_date), as.numeric(end_date)), origin = "1970-01-01", tz = "UTC")
  }
  
  format_random <- function() {
    ts <- random_timestamp()
    fmt <- sample(formats, 1)
    format(ts, fmt)
  }
  
  replicate(n, {
    if (runif(1) < fraction_range) {
      paste0(format_random(), "/", format_random())
    } else {
      format_random()
    }
  })
}

#' Generate Height Value Pairs
#'
#' Erstellt ein Tibble mit zufaelligen Hoehenwerten (min/max), teils mit fehlenden Werten.
#'
#' @param n Anzahl der Hoehenwert-Paare.
#' @param na_fraction Anteil fehlender Werte (zwischen 0 und 1).
#' @param min_height Untere Grenze fuer Hoehenwerte.
#' @param max_height Obere Grenze fuer Hoehenwerte.
#'
#' @importFrom tibble tibble
#' @return Tibble mit Spalten `lokalitaetHoeheMin` und `lokalitaetHoeheMax`.
#' @export
generate_height_pair <- function(
    n,
    na_fraction = 0.2,
    min_height = 0,
    max_height = 2000
) {
  result <- tibble(
    lokalitaetHoeheMin = rep(NA_real_, n),
    lokalitaetHoeheMax = rep(NA_real_, n)
  )
  
  for (i in seq_len(n)) {
    roll <- runif(1)
    if (roll < na_fraction) {
      next
    } else if (roll < na_fraction + 0.2) {
      result$lokalitaetHoeheMin[i] <- runif(1, min_height, max_height)
    } else if (roll < na_fraction + 0.4) {
      result$lokalitaetHoeheMax[i] <- runif(1, min_height, max_height)
    } else {
      min_val <- runif(1, min_height, max_height - 1)
      max_val <- runif(1, min_val + 1, max_height)
      result$lokalitaetHoeheMin[i] <- min_val
      result$lokalitaetHoeheMax[i] <- max_val
    }
  }
  
  return(result)
}


#' Generate Random Area Values
#'
#' Erzeugt einen numerischen Vektor mit zufaelligen Flaechenwerten.
#'
#' @param n Anzahl der Werte.
#' @param min_area Minimale Flaeche.
#' @param max_area Maximale Flaeche.
#' @param na_prob Wahrscheinlichkeit fuer fehlende Werte.
#'
#' @return Numeric vector with area values.
#' @export
generate_area_vector <- function(n, min_area = 100, max_area = 10000, na_prob = 0.2) {
  values <- runif(n, min_area, max_area)
  is_na <- runif(n) < na_prob
  values[is_na] <- NA_real_
  return(values)
}



#' Generate Random Length Values
#'
#' Erzeugt einen numerischen Vektor mit zufaelligen Laengenwerten.
#'
#' @param n Anzahl der Werte.
#' @param min_length Minimale Laenge.
#' @param max_length Maximale Laenge.
#' @param na_prob Wahrscheinlichkeit fuer fehlende Werte.
#'
#' @return Numeric vector with length values.
#' @export
generate_length_vector <- function(n, min_length = 100, max_length = 10000, na_prob = 0.2) {
  values <- runif(n, min_length, max_length)
  is_na <- runif(n) < na_prob
  values[is_na] <- NA_real_
  return(values)
}

#' Generate WKT Point Geometries
#'
#' Erzeugt zufaellige WKT-Punkte im Bereich Mitteleuropas.
#'
#' @param n Anzahl der Punkte.
#' @return Character-Vektor mit WKT-POINT-Strings.
#' @export
#' 
generate_wkt_point_vector <- function(n) {
  paste0("POINT (", runif(n, 7, 15), " ", runif(n, 47, 55), ")")
}

#' Generate WKT LineStrings
#'
#' Erzeugt zufaellige WKT-Linien (LINESTRING).
#'
#' @param n Anzahl der Linien.
#' @return Character-Vektor mit LINESTRING-Geometrien.
#' @export
#' 
generate_wkt_linestring_vector <- function(n) {
  replicate(n, {
    n_points <- sample(2:5, 1)
    coords <- paste(
      paste0(runif(n_points, 7, 15), " ", runif(n_points, 47, 55)),
      collapse = ", "
    )
    paste0("LINESTRING (", coords, ")")
  })
}

#' Generate WKT Polygons
#'
#' Erzeugt einfache WKT-Polygon-Geometrien.
#'
#' @param n Anzahl der Polygone.
#' @return Character-Vektor mit POLYGON-Geometrien.
#' @export
#' 
generate_wkt_polygon_vector <- function(n) {
  replicate(n, {
    x1 <- runif(1, 7, 15)
    y1 <- runif(1, 47, 55)
    w <- runif(1, 0.1, 1)
    h <- runif(1, 0.1, 1)
    paste0(
      "POLYGON ((",
      x1, " ", y1, ", ",
      x1 + w, " ", y1, ", ",
      x1 + w, " ", y1 + h, ", ",
      x1, " ", y1 + h, ", ",
      x1, " ", y1, "))"
    )
  })
}

#' Generate WKT Multipolygons
#'
#' Erzeugt zufaellige MULTIPOLYGON-Geometrien mit zwei Teilen.
#'
#' @param n Anzahl der Multipolygone.
#' @return Character-Vektor mit MULTIPOLYGON-Geometrien.
#' @export
#' 
generate_wkt_multipolygon_vector <- function(n) {
  replicate(n, {
    x1 <- runif(1, 7, 10)
    y1 <- runif(1, 47, 49)
    x2 <- runif(1, 11, 14)
    y2 <- runif(1, 50, 52)
    w1 <- runif(1, 0.1, 0.5)
    h1 <- runif(1, 0.1, 0.5)
    w2 <- runif(1, 0.1, 0.5)
    h2 <- runif(1, 0.1, 0.5)
    paste0(
      "MULTIPOLYGON (((",
      x1, " ", y1, ", ",
      x1 + w1, " ", y1, ", ",
      x1 + w1, " ", y1 + h1, ", ",
      x1, " ", y1 + h1, ", ",
      x1, " ", y1, ")), ((",
      x2, " ", y2, ", ",
      x2 + w2, " ", y2, ", ",
      x2 + w2, " ", y2 + h2, ", ",
      x2, " ", y2 + h2, ", ",
      x2, " ", y2, ")))"
    )
  })
}

#' Generate a Single WKT Polygon or Multipolygon
#'
#' Gibt zufaellig ein einfaches Polygon oder ein Multipolygon zurueck.
#'
#' @return Ein einzelner WKT-String.
#' @export
#' 
generate_wkt_polygon <- function() {
  type <- sample(c("POLYGON", "MULTIPOLYGON"), 1, prob = c(0.7, 0.3))
  if (type == "POLYGON") {
    x1 <- runif(1, 7, 15)
    y1 <- runif(1, 47, 55)
    w <- runif(1, 0.1, 1)
    h <- runif(1, 0.1, 1)
    paste0("POLYGON ((", 
           x1, " ", y1, ", ",
           x1 + w, " ", y1, ", ",
           x1 + w, " ", y1 + h, ", ",
           x1, " ", y1 + h, ", ",
           x1, " ", y1, "))")
  } else {
    x1 <- runif(1, 7, 10)
    y1 <- runif(1, 47, 49)
    x2 <- runif(1, 11, 14)
    y2 <- runif(1, 50, 52)
    w1 <- runif(1, 0.1, 0.5)
    h1 <- runif(1, 0.1, 0.5)
    w2 <- runif(1, 0.1, 0.5)
    h2 <- runif(1, 0.1, 0.5)
    paste0("MULTIPOLYGON (((",
           x1, " ", y1, ", ",
           x1 + w1, " ", y1, ", ",
           x1 + w1, " ", y1 + h1, ", ",
           x1, " ", y1 + h1, ", ",
           x1, " ", y1, ")), ((",
           x2, " ", y2, ", ",
           x2 + w2, " ", y2, ", ",
           x2 + w2, " ", y2 + h2, ", ",
           x2, " ", y2 + h2, ", ",
           x2, " ", y2, ")))")
  }
}

#' Generate Randomized Name Fields
#'
#' Erzeugt realistische Namenseintraege (mit Abkuerzungen) fuer Testdaten.
#'
#' @param n Anzahl der Felder.
#' @param pool_first Vektor moeglicher Vornamen.
#' @param pool_last Vektor moeglicher Nachnamen.
#' @param min_names Minimale Anzahl an Namen pro Feld.
#' @param max_names Maximale Anzahl an Namen pro Feld.
#' @param abbrev_prob Wahrscheinlichkeit fuer abgekuerzte Vornamen.
#' @param separator Trennzeichen fuer mehrere Namen.
#'
#' @return Ein Character-Vektor mit Namenseintraegen.
#' @export
#' 
generate_names_field <- function(
    n = 1,
    pool_first = c("Anna", "Ben", "Clara", "David", "Eva", "Felix", "Greta", "Hans", "Ida", "Jonas"),
    pool_last = c("Mueller", "Schmidt", "Schneider", "Fischer", "Weber", "Meyer", "Wagner", "Becker", "Hoffmann", "Schaefer"),
    min_names = 1,
    max_names = 3,
    abbrev_prob = 0.3,
    separator = "; "
) {
  results <- character(n)
  for (i in seq_len(n)) {
    num_names <- sample(min_names:max_names, 1)
    single_field <- character(num_names)
    for (j in seq_len(num_names)) {
      first <- sample(pool_first, 1)
      last <- sample(pool_last, 1)
      if (runif(1) < abbrev_prob) {
        name <- paste0(substr(first, 1, 1), ". ", last)
      } else {
        name <- paste(first, last)
      }
      single_field[j] <- name
    }
    results[i] <- paste(single_field, collapse = separator)
  }
  return(results)
}

#' Replace German Umlauts
#'
#' Ersetzt deutsche Umlaute in Texten mit ASCII-kompatiblen Umschriften.
#'
#' @param text Character-Vektor mit deutschen Umlauten.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @return Character-Vektor mit ersetzten Umlauten.
#' @export
#' 
replace_umlaute <- function(text) {
  str_replace_all(text, c(
    "ae" = "ae", "oe" = "oe", "ue" = "ue",
    "Ae" = "Ae", "Oe" = "Oe", "Ue" = "Ue",
    "ss" = "ss"
  ))
}
