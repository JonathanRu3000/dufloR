#' Generiert vollstaendigen floristischen Testdatensatz
#'
#' Diese Funktion erzeugt synthetische Testdaten fuer floristische Projekte,
#' Lokalitaeten, Personen, Ereignisse und Nachweise. Die Daten werden zusammengefuehrt
#' und optional als UTF-8-codierte CSV-Datei gespeichert.
#'
#' @param n_projekte Anzahl der Projekte (max. 50)
#' @param n_lokalitaeten Anzahl der Lokalitaeten (max. 20)
#' @param n_personen Anzahl der Personen (max. 20)
#' @param n_ereignisse Anzahl der Ereignisse (max. 30)
#' @param n_nachweise Anzahl der Nachweise
#' @param schema_path Pfad zum JSON-Schema (optional, derzeit nicht verwendet)
#' @param output_path Optional: Pfad zur Ausgabe als CSV-Datei. Wenn NULL, wird keine Datei geschrieben.
#' @param seed Zufalls-Seed fuer Reproduzierbarkeit
#'
#' @return Ein Data Frame mit den kombinierten Testdaten
#' @export
#' @import dplyr
#' @import uuid
#' @importFrom jsonvalidate json_validator
generate_floristic_occdata <- function(
    n_projekte = 50,
    n_lokalitaeten = 20,
    n_personen = 20,
    n_ereignisse = 30,
    n_nachweise = 1000,
    schema_path = system.file("schemas", "structure.schema.json", package = "dufloR"),
    output_path = NULL,
    seed = 42
) {
  # 1) Stammdaten erzeugen
  projekte <- generate_projects(n_projekte, seed = seed)
  lokalitaeten <- generate_localities(n_lokalitaeten, seed = seed + 1)
  personen <- generate_persons(n_personen, seed = seed + 2)
  ereignisse <- generate_events(n_ereignisse, seed = seed + 3)

  # 2) Nachweise erzeugen
  nachweise <- generate_occs(
    n = n_nachweise,
    n_lokalitaet = n_lokalitaeten,
    n_ereignis = n_ereignisse,
    n_projekt = n_projekte,
    n_person = n_personen,
    seed = seed + 4
  ) %>%
    mutate(across(where(is.character), enc2utf8))

  # 3) Zusammenfuehren
  df <- nachweise %>%
    left_join(lokalitaeten, by = "lokalitaetID") %>%
    left_join(personen, by = "personID") %>%
    left_join(projekte, by = "projektID") %>%
    left_join(ereignisse, by = "ereignisID") %>%
    mutate(across(where(is.character), enc2utf8))

  # 4) Optional: CSV schreiben
  if (!is.null(output_path)) {
    write.csv(df, file = output_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  return(df)
}
