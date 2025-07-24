#' Generiert eine Tabelle mit synthetischen Personendaten
#'
#' Diese Funktion erstellt eine tibble-Tabelle mit zufaellig generierten Personennamen
#' (Vorname Nachname) und eindeutigen IDs. Sie dient zur Demonstration, Testung oder
#' Entwicklung. Die maximale Anzahl an IDs ist auf 20 begrenzt.
#'
#' @param n_personen Integer. Anzahl der zu generierenden Personen (max. 20). Standard ist 20.
#' @param seed Integer. Seed fuer den Zufallszahlengenerator. Standard ist 123.
#'
#' @return Ein tibble mit Spalten \code{personID} und \code{personName}.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select sample_n
#' @export
generate_persons <- function(n_personen = 20, seed = 123) {
  set.seed(seed)

  # --------------------------------------------
  # ✅ Check: maximal 20 Personen zulassen
  # --------------------------------------------
  if (n_personen > 100) {
    stop("Fehler: Es koennen maximal 20 Personen auf einmal erzeugt werden.")
  }

  # --------------------------------------------
  # 📌 Pools fuer Vor- und Nachnamen
  # --------------------------------------------
  first_names <- c(
    "Anna", "Ben", "Clara", "David", "Emma",
    "Felix", "Greta", "Henry", "Isabel", "Jonas"
  )

  last_names <- c(
    "Mueller", "Schmidt", "Weber", "Fischer", "Koch",
    "Richter", "Wolf", "Neumann", "Krueger", "Hartmann"
  )

  # --------------------------------------------
  # 📌 Alle moeglichen eindeutigen Kombinationen
  # --------------------------------------------
  all_combinations <- expand.grid(
    first_names = first_names,
    last_names = last_names,
    stringsAsFactors = FALSE
  ) %>%
    mutate(personName = paste(first_names, last_names)) %>%
    select(personName)

  # Sicherstellen, dass genug Kombinationen vorhanden sind
  if (n_personen > nrow(all_combinations)) {
    stop("Nicht genug eindeutige Namenskombinationen verfuegbar!")
  }

  # --------------------------------------------
  # 📌 Zufaellige eindeutige Auswahl
  # --------------------------------------------
  sampled_names <- all_combinations %>%
    sample_n(n_personen)

  # --------------------------------------------
  # 📌 IDs zuweisen
  # --------------------------------------------
  person_table <- tibble(
    personID = sprintf("ID_Person%03d", 1:n_personen),
    personName = sampled_names$personName
  )

  return(person_table)
}
