#' Generiert Testfaelle mit manipulierten Spalten (NA oder entfernt)
#'
#' Diese Funktion erstellt Varianten eines gegebenen Dataframes fuer Testszenarien, bei denen einzelne oder mehrere Spalten
#' entweder entfernt oder auf NA gesetzt werden. Es koennen entweder zufaellige Spalten ausgewaehlt werden (Standard)
#' oder gezielt explizit angegebene Spaltennamen manipuliert werden.
#'
#' **Verhalten:**
#' - Randomisiert (Default): Es werden die angegebenen Anzahlen an Single- und Multi-Spalten-Manipulationen mit zufaellig gewaehlten Spalten erzeugt.
#' - Manuell: Wenn `drop_cols` oder `na_cols` angegeben sind und `randomize = FALSE`, werden nur diese gezielt manipuliert.
#'
#' **Typen von Testfaellen:**
#' - Single-column NA: eine einzelne Spalte wird komplett auf NA gesetzt
#' - Single-column DROP: eine einzelne Spalte wird entfernt
#' - Multi-column NA: mehrere Spalten werden gleichzeitig auf NA gesetzt
#' - Multi-column DROP: mehrere Spalten werden entfernt
#'
#' @param input_df Dataframe mit den Originaldaten
#' @param n_single_cases Anzahl der randomisierten Single-Spalten-Faelle (Default: 10)
#' @param n_multi_cases Anzahl der randomisierten Multi-Spalten-Faelle (Default: 7)
#' @param drop_cols Optional. Vektor von Spaltennamen, die gezielt entfernt werden sollen (nur wenn randomize = FALSE)
#' @param na_cols Optional. Vektor von Spaltennamen, die gezielt auf NA gesetzt werden sollen (nur wenn randomize = FALSE)
#' @param randomize Logical. Soll die Auswahl der Spalten zufaellig sein? Default = TRUE
#' @param seed Optional. Zahl fuer set.seed(), um Zufallsauswahl reproduzierbar zu machen
#' @param verbose Logical. Soll der Prozess mit Ausgaben begleitet werden? Default = FALSE
#'
#' @return Eine Liste von Dataframes mit unterschiedlichen Testfaellen (Manipulationen)
#'
#'
#' # Gezielte Spaltennamen
#' generate_test_cases(mydata, drop_cols = c("SpalteA", "SpalteB"), na_cols = "SpalteC", randomize = FALSE, verbose = TRUE)
#'
#' @importFrom dplyr select all_of
#' @export
generate_test_cases <- function(
    input_df,
    n_single_cases = 10,
    n_multi_cases = 7,
    drop_cols = NULL,
    na_cols = NULL,
    randomize = TRUE,
    seed = 42,
    verbose = FALSE
) {
  library(dplyr)

  all_cols <- names(input_df)
  test_cases <- list()

  if (!randomize) {
    # ----------------------------------------------
    # Nutzerdefinierte gezielte Manipulation
    # ----------------------------------------------
    if (!is.null(na_cols)) {
      na_cols_valid <- intersect(na_cols, all_cols)
      if (length(na_cols_valid) == 0) {
        warning("Keine gueltigen Spaltennamen in na_cols gefunden.")
      } else {
        if (verbose) message("Setze folgende Spalten auf NA: ", paste(na_cols_valid, collapse = ", "))
        df_na <- input_df
        df_na[, na_cols_valid] <- NA
        test_cases[["manual_NA"]] <- df_na
      }
    }
    if (!is.null(drop_cols)) {
      remaining_cols <- setdiff(all_cols, drop_cols)
      if (length(remaining_cols) < ncol(input_df)) {
        if (verbose) message("Entferne folgende Spalten: ", paste(drop_cols, collapse = ", "))
        df_drop <- input_df %>% select(all_of(remaining_cols))
        test_cases[["manual_DROP"]] <- df_drop
      } else {
        warning("Keine gueltigen Spalten zum Droppen angegeben – alle Spalten wuerden entfallen.")
      }
    }
    return(test_cases)
  }

  # ----------------------------------------------
  # Randomisierte Auswahl
  # ----------------------------------------------
  set.seed(seed)

  if (verbose) message("Starte randomisierte Generierung von Testfaellen...")

  # Single-column NA
  for (i in 1:(n_single_cases / 2)) {
    chosen_col <- sample(all_cols, 1)
    if (verbose) message("Single NA-Fall: Setze Spalte '", chosen_col, "' auf NA")
    df_na <- input_df
    df_na[[chosen_col]] <- NA
    test_cases[[paste0("single_NA_", i, "_", chosen_col)]] <- df_na
  }

  # Single-column DROP
  for (i in 1:(n_single_cases / 2)) {
    chosen_col <- sample(all_cols, 1)
    if (verbose) message("Single DROP-Fall: Entferne Spalte '", chosen_col, "'")
    df_drop <- input_df %>% select(-all_of(chosen_col))
    test_cases[[paste0("single_DROP_", i, "_", chosen_col)]] <- df_drop
  }

  # Multi-column NA
  for (i in 1:(n_multi_cases / 2)) {
    n_cols_to_change <- sample(2:min(8, length(all_cols)), 1)
    chosen_cols <- sample(all_cols, size = n_cols_to_change)
    if (verbose) message("Multi NA-Fall: Setze Spalten auf NA: ", paste(chosen_cols, collapse = ", "))
    df_na <- input_df
    df_na[, chosen_cols] <- NA
    test_cases[[paste0("multi_NA_", i, "_", paste(chosen_cols, collapse = "_"))]] <- df_na
  }

  # Multi-column DROP
  for (i in 1:(n_multi_cases / 2)) {
    n_cols_to_change <- sample(2:min(8, length(all_cols)), 1)
    chosen_cols <- sample(all_cols, size = n_cols_to_change)
    remaining_cols <- setdiff(all_cols, chosen_cols)
    if (verbose) message("Multi DROP-Fall: Entferne Spalten: ", paste(chosen_cols, collapse = ", "))
    df_drop <- input_df %>% select(all_of(remaining_cols))
    test_cases[[paste0("multi_DROP_", i, "_", paste(chosen_cols, collapse = "_"))]] <- df_drop
  }

  if (verbose) message("Testfaelle erfolgreich generiert: ", length(test_cases), " Varianten.")

  return(test_cases)
}
