#' Generiert synthetische Projektdaten
#'
#' Diese Funktion erstellt eine zufaellige Lookup-Tabelle mit synthetischen Projektdaten
#' zur Demonstration, Testung oder Entwicklung. Die Funktion simuliert Projekt-IDs,
#' -Namen, -Typen, Regionen (inkl. Geometrien), Taxa und Kontaktdaten der Projektleitung.
#' Umlaute in Namen werden automatisch in ASCII-freundliche Schreibweisen konvertiert.
#'
#' @param n_projekte Integer. Anzahl der zu erzeugenden Projekte (max. 50). Standard ist 50.
#' @param seed Integer. Startwert fuer Zufallszahlengenerator (Standard 42) fuer Reproduzierbarkeit.
#'
#' @return Ein tibble mit den erzeugten Projektdaten.
#'
#' @importFrom stringr str_pad str_replace_all
#' @import dplyr
#' @importFrom tibble tibble
#' @export
generate_projects <- function(
    n_projekte = 50,
    seed = 42
) {
  set.seed(seed)

  # --------------------------------------
  # 📌 Maximal 50 Projekte zulassen
  # --------------------------------------
  if (n_projekte > 50) {
    stop("Fehler: Es koennen maximal 50 Projekte auf einmal erzeugt werden.")
  }

  # ------------------------------------------------
  # 📌 Grunddaten-Pools
  # ------------------------------------------------
  strassen <- c("Hauptstrasse", "Bahnhofstr.", "Lindenweg", "Musterallee", "Parkweg")
  hausnummern <- 1:200
  postleitzahlen <- c("10115", "80331", "50667", "20095", "70173")
  staedte <- c("Berlin", "Muenchen", "Koeln", "Hamburg", "Stuttgart")

  vornamen <- c("Anna", "Ben", "Clara", "David", "Eva", "Felix", "Greta", "Hans", "Ida", "Jonas")
  nachnamen <- c("Mueller", "Schmidt", "Schneider", "Fischer", "Weber", "Meyer", "Wagner", "Becker", "Hoffmann", "Schaefer")

  domains <- c("firma.de", "projekt.com", "example.org", "unternehmen.net")
  vorwahlen <- c("30", "89", "221", "40", "711")

  projekt_name_pool <- c(
    "Floristische Erhebung Nord", "Kartierung Suedwald", "Biotopmonitoring Rhein",
    "Biodiversitaetsstudie Alpen", "Habitatkartierung Elbe", "Renaturierungsprojekt Spree",
    "Oeko-Korridor Main", "Heideflaechen-Management", "Pflanzenmonitoring Allgaeu",
    "Artenschutzprojekt Harz", "Naturschutzplanung Taunus", "Moorschutz Bayern",
    "Vegetationsaufnahme Weserbergland", "Biotopverbund Schwarzwald",
    "Flora-Fauna-Inventar Eifel", "Trockenrasenmonitoring Rhoen", "Feuchtgebietsanalyse Donau",
    "Auenrenaturierung Neckar", "Erfassung invasive Arten Sachsen", "Oekologisches Monitoring Hessen",
    "Biotoptypenkartierung Brandenburg", "Naturschutzgrossprojekt Saarland",
    "Vegetationsanalyse Thueringen", "Artenerfassung Mecklenburg", "Landschaftspflegeprojekt NRW",
    "Oekosystemstudie Schleswig", "Renaturierung Ruhrgebiet", "Pflanzenoekologie Saar",
    "Naturraeume Unstrut", "Artenvielfalt Uckermark", "Lebensraumkartierung Oderbruch",
    "Biodiversitaet Vogtland", "Naturschutzanalyse Lausitz", "Artenmonitoring Mainfranken",
    "Kartierung Wesermarsch", "Naturschutzprogramm Spessart", "Projekt Isartal",
    "Renaturierung Leinetal", "Biotopmanagement Nordsee", "Florauntersuchung Lueneburg",
    "Naturschutzinitiative Dresden", "Oekoprojekt Muensterland",
    "Floristische Kartierung Frankfurt", "Habitatverbund Kieler Bucht",
    "Oekosystemanalyse Stuttgart", "Artenschutzprojekt Hamburg",
    "Biotopvernetzung Leipzig", "Floristische Studie Berlin",
    "Monitoringprojekt Nuernberg", "Umweltanalyse Hannover"
  )

  regionen_pool <- c(
    "Alpen", "Schwarzwald", "Norddeutsche Tiefebene", "Eifel", "Harz",
    "Bayerischer Wald", "Spessart", "Rhoen", "Thueringer Wald", "Weserbergland",
    "Bayern", "Nordrhein-Westfalen", "Brandenburg", "Sachsen", "Hessen",
    "Schleswig-Holstein", "Baden-Wuerttemberg", "Rheinland-Pfalz", "Saarland", "Mecklenburg-Vorpommern"
  )


  # ------------------------------------------------
  # 📌 Lookup-Tabelle mit Projektdetails generieren
  # ------------------------------------------------
  lookup.projekt <- tibble(
    projektID = paste0("ID_Projekt", str_pad(1:n_projekte, 3, pad = "0")),
    projektName = sample(projekt_name_pool, n_projekte),
  ) %>%
    rowwise() %>%
    mutate(
      projektTyp = sample(
        c("Unsystematisch", "Floristische Kartierung", "Stichprobe", "Pflanzensoziologische Aufnahme", NA),
        1, replace = TRUE
      ),
      projektRegion = sample(regionen_pool, 1),
      projektRegionUmriss = sample(c(NA,generate_wkt_polygon()), 1),
      projektRegionUmrissCRS = case_when(
        !is.na(projektRegionUmriss)~"EPSG:4326"
      ),
      projektTaxon = sample(c("Plantae", "Tracheophyta",NA), 1),
      projektUrl = sample(c(NA,paste0("www.", sample(domains, 1))),1),


      # Leitung
      Vorname = sample(vornamen, 1),
      Nachname = sample(nachnamen, 1),
      projektLeitung = paste(Vorname, Nachname),

      projektLeitungKontakt = sample(c(NA,{
        strasse <- sample(strassen, 1)
        hausnr <- sample(hausnummern, 1)
        plz <- sample(postleitzahlen, 1)
        stadt <- sample(staedte, 1)
        format <- sample(1:4, 1)
        if (format == 1) {
          paste(strasse, hausnr, ",", plz, stadt)
        } else if (format == 2) {
          paste(plz, stadt, ",", strasse, hausnr)
        } else if (format == 3) {
          paste(strasse, hausnr, "\n", plz, stadt)
        } else {
          paste(hausnr, strasse, ",", stadt, plz)
        }
      }),1),

      projektLeitungEmailAdresse = sample(c(NA, {
        domain <- sample(domains, 1)
        vorname_clean <- tolower(replace_umlaute(Vorname))
        nachname_clean <- tolower(replace_umlaute(Nachname))
        paste0(vorname_clean, ".", nachname_clean, "@", domain)
      }), 1),

      projektLeitungTelefonNummer = sample(c(NA, {
        vorwahl <- sample(vorwahlen, 1)
        nummer1 <- sample(100:999, 1)
        nummer2 <- sample(1000:9999, 1)
        format <- sample(1:4, 1)
        if (format == 1) {
          paste0("+49 ", vorwahl, " ", nummer1, " ", nummer2)
        } else if (format == 2) {
          paste0("0", vorwahl, "-", nummer1, "-", nummer2)
        } else if (format == 3) {
          paste0("+49 (0", vorwahl, ") ", nummer1, "/", nummer2)
        } else {
          paste0(vorwahl, "/", nummer1, "-", nummer2)
        }
      }), 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Vorname, -Nachname)


  return(lookup.projekt)
}




