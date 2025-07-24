#' Generiert eine Lookup-Tabelle mit synthetischen Ereignissen
#'
#' Diese Funktion erstellt eine tibble-Tabelle mit maximal 30 Ereignisnamen
#' und ISO8601-kompatiblen Datumsangaben (einzeln oder als Bereiche),
#' nuetzlich fuer Tests, Demos oder Entwicklung.
#'
#' Die Zahl der Ereignisse ist auf maximal 30 beschraenkt.
#'
#' @param n_ereignisse Integer. Anzahl der zu generierenden Ereignisse (max. 30). Standard ist 30.
#' @param seed Integer. Seed fuer den Zufallszahlengenerator. Standard ist 123.
#'
#' @return Ein tibble mit Spalten \code{ereignisID}, \code{ereignisName} und \code{ereignisDatum}.
#'
#' @importFrom stringr str_pad
#' @importFrom tibble tibble
#' @export
generate_events <- function(n_ereignisse = 30, seed = 123) {
  # --------------------------------------------
  # ✅ Check: maximal 30 Ereignisse zulassen
  # --------------------------------------------
  if (n_ereignisse > 30) {
    stop("Fehler: Es koennen maximal 30 Ereignisse auf einmal erzeugt werden.")
  }

  set.seed(seed)

  # --------------------------------------------
  # 📌 Pool von Ereignisnamen
  # --------------------------------------------
  ereignisse_pool <- c(
    "Exkursion Siebengebirge 2003",
    "Botanische Wanderung 2011",
    "Pflanzenkartierung Schwarzwald 1998",
    "Herbar-Sammeltour Harz 2007",
    "Vegetationsaufnahme Rhoen 2012",
    "Floristische Exkursion Eifel 2009",
    "Sammelreise Oberbayern 2015",
    "Bestimmungsexkursion Lueneburger Heide 2002",
    "Kartierprojekt Saechsische Schweiz 2016",
    "Botanische Streifzuege Hunsrueck 2004",
    "Pflanzensammlung Pfalz 2010",
    "Moor-Exkursion Niedersachsen 2005",
    "Feldstudie Thueringer Wald 2013",
    "Flora-Erhebung Spessart 2008",
    "Studienreise Lausitz 2017",
    "Botanische Feldarbeit Franken 2001",
    "Artensuche Holsteinische Schweiz 2014",
    "Sommerexkursion Westerwald 2006",
    "Feldkartierung Nordschwarzwald 2018",
    "Sammelaktion Mecklenburgische Seenplatte 2000",
    "Herbstwanderung Bayerischer Wald 2019",
    "Alpenflora-Exkursion Allgaeu 2003",
    "Botanische Tour Sauerland 2007",
    "Fruehlingswanderung Vogelsberg 2011",
    "Pflanzenexkursion Oberrhein 2009",
    "Kartierexkursion Teutoburger Wald 2015",
    "Flora-Aufnahme Schwaebische Alb 2004",
    "Exkursionswoche Steigerwald 2012",
    "Naturkundliche Wanderung Uckermark 2008",
    "Botanische Sammelreise Rheinhessen 2016"
  )

  # --------------------------------------------
  # ✅ Ereignisse erstellen (immer vollstaendig)
  # --------------------------------------------

  selected_namen <- sample(ereignisse_pool, n_ereignisse, replace = FALSE)

  ereignis_df <- tibble::tibble(
    ereignisID    = paste0("ID_Ereignis", stringr::str_pad(1:n_ereignisse, 3, pad = "0")),
    ereignisName  = selected_namen,
    ereignisDatum = generate_mixed_iso8601(n_ereignisse)
  )

  return(ereignis_df)
}

