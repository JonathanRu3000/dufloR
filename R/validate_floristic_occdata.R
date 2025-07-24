#' Validiert floristische Funddaten gemaess des BFN-Datenstandards fuer floristische Funddaten (CSV, JSON-String oder JSON-Datei)
#'
#' Diese Funktion validiert floristische Datenstrukturen in R gegen ein JSON-Schema, das dem DUFLOR-Standard entspricht.
#' Sie akzeptiert verschiedene Eingabeformate (Dataframe, Liste von Tabellen, JSON-String, Dateipfad) und prueft die Struktur und Konsistenz der enthaltenen Daten.
#'
#' **Unterstuetzte Eingaben:**
#' - Ein einzelner `data.frame` mit kombinierten Daten aller Objekttypen.
#' - Eine `list` mit benannten Tabellen (z. B. `list(Projekte = df1, Nachweise = df2)`).
#' - Ein gueltiger JSON-String.
#' - Ein Pfad zu einer JSON-Datei.
#'
#' **Validierungslogik:**
#' - Prueft, ob Pflichttabellen (z. B. Projekte, Nachweise, Lokalitaeten) vorhanden sind.
#' - Meldet fehlende Spalten.
#' - Prueft Dubletten (mehrfach identische Zeilen).
#' - Prueft, ob IDs eindeutig auf Namen abbildbar sind (und umgekehrt).
#' - Ignoriert fehlende Namen optional fuer bestimmte Objekte (z. B. `personName`).
#' - Gibt strukturierte Pruefergebnisse und JSON-Exporte zurueck.
#'
#' @param input Ein Dataframe, eine Liste von Tabellen, ein JSON-String oder der Pfad zu einer JSON-Datei mit floristischen Daten.
#' @param schema Optional. Ein JSON-Schema zur Validierung (als JSON-String oder R-Liste). Standard: `NULL`.
#' @param stop_on_error Logical. Wenn `TRUE`, wird die Ausfuehrung bei Fehlern gestoppt. Default: `TRUE`.
#' @param verbose Logical. Wenn `TRUE`, werden informative Meldungen ausgegeben. Default: `TRUE`.
#' @param prefix_nachweis_obj Character. Basisname fuer die Nachweisobjekte im Datensatz (z. B. `"nachweis"` oder `"fund"`). Dient zur Erkennung der Nachweis-Tabelle. Default: `"nachweis"`.
#' @param allow_na_if_name_contains Character-Vektor. Falls Spaltennamen diesen String enthalten (z. B. `"person"` oder `"ereignis"`), duerfen Namen `NA` sein. Default: `"person"`.
#'
#' @return Eine Liste mit:
#' \describe{
#'   \item{check_results}{Liste mit Validierungsergebnissen pro Objekttabelle (inkl. Status, Nachricht, Roh- und bereinigten Daten).}
#'   \item{validation_json}{JSON-String mit unbearbeiteten Eingabedaten (strukturkonform).}
#'   \item{export_json}{JSON-String mit bereinigten, eindeutigen Eintraegen pro Objekt.}
#'   \item{json_valid}{Logical. Ergebnis der Schema-Validierung (`TRUE`/`FALSE`) oder `NULL`, wenn kein Schema uebergeben wurde.}
#'   \item{validation_errors}{Liste der Schemafehler (nur bei `json_valid = FALSE`).}
#'   \item{missing_fields}{Liste der erwarteten, aber fehlenden Felder je Objekttabelle.}
#' }
#'
#' # Beispiel: Mit Schema-Datei pruefen
#' schema_path <- system.file("schemas", "structure.schema.json", package = "dufloR")
#' validate_floristic_data(df, schema = schema_path)
#'
#' @importFrom dplyr group_by summarise across distinct select filter mutate
#' @importFrom tidyr pivot_longer
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom jsonvalidate json_validator
#' @export

validate_floristic_occdata <- function(
    input,
    schema = NULL,
    stop_on_error = TRUE,
    verbose = TRUE,
    prefix_nachweis_obj = "nachweis",
    allow_na_if_name_contains = "person"
) {

  msg_counter <- 1

  log_message <- function(msg) {
    msg <- enc2utf8(msg)
    if (verbose) message(msg)
    msg_counter <<- msg_counter + 1
  }

  log_error <- function(msg) {
    msg <- enc2utf8(msg)
    if (stop_on_error) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
    msg_counter <<- msg_counter + 1
  }


  get_schema_json <- function(schema) {
    if (is.null(schema)) return(NULL)
    if (is.list(schema)) {
      return(jsonlite::toJSON(schema, auto_unbox = TRUE))
    }
    if (is.character(schema)) {
      return(schema)
    }
    stop("Das Argument 'schema' muss entweder ein JSON-String oder eine geparste JSON-Liste sein.")
  }

  # ------------------------------------------------------------
  # Erwartete Felder pro Tabelle
  # ------------------------------------------------------------

  schema_names <- fromJSON(system.file("schemas", "structure.schema.json", package = "dufloR"), simplifyVector = FALSE)

  expected_fields <- list()

  for(i in 1:length(schema_names$`$defs`)){
    nam <- names(schema_names$`$defs`[i])
    prop <- names(schema_names$`$defs`[[i]]$properties)
    obj <- matches <- grep(nam, names(schema_names$properties), value = TRUE)

    expected_fields[[obj]] <- prop
  }


  # ------------------------------------------------------------
  check_and_summarise <- function(
    data, id_col_name, label, listelement,
    check_id_name_mapping = TRUE,
    allow_empty = FALSE,
    require_attributes = FALSE
  ) {
    if (ncol(data) == 0) {
      if (allow_empty) {
        msg <- paste0(msg_counter, ": Hinweis: Keine ", listelement, " vorhanden – Angabe ist optional.")
        log_message(msg)
        return(list(raw = NULL, distinct = NULL, message = msg, valid = TRUE))
      } else {
        msg <- paste0(msg_counter, ": Fehler: Keine Daten fuer ", listelement, " vorhanden.")
        log_error(msg)
        return(list(raw = NULL, distinct = NULL, message = msg, valid = FALSE))
      }
    }

    if (!(id_col_name %in% colnames(data))) {
      if (allow_empty) {
        msg <- paste0(msg_counter, ": Hinweis: Spalte '", id_col_name, "' fehlt – keine Pruefung fuer ", listelement, ".")
        log_message(msg)
        return(list(raw = NULL, distinct = NULL, message = msg, valid = TRUE))
      } else {
        msg <- paste0(msg_counter, ": Fehler: Spalte '", id_col_name, "' fehlt – notwendig fuer ", listelement, ".")
        log_error(msg)
        return(list(raw = NULL, distinct = NULL, message = msg, valid = FALSE))
      }
    }

    other_cols <- setdiff(names(data), id_col_name)
    if (length(other_cols) == 0) {
      if (require_attributes) {
        msg <- paste0(msg_counter, ": Fehler: ", listelement, " enthaelt nur die ID-Spalte – es muessen weitere Attribute vorhanden sein.")
        log_error(msg)
        return(list(raw = NULL, distinct = NULL, message = msg, valid = FALSE))
      } else {
        msg <- paste0(msg_counter, ": Hinweis: ", listelement, " enthaelt nur die ID-Spalte – keine weiteren Attribute zu pruefen.")
        log_message(msg)
        return(list(raw = data, distinct = data %>% distinct(), message = msg, valid = TRUE))
      }
    }

    consistency_check <- data %>%
      group_by(.data[[id_col_name]]) %>%
      summarise(across(everything(), n_distinct), .groups = "drop") %>%
      pivot_longer(-all_of(id_col_name), names_to = "column", values_to = "n_unique") %>%
      filter(n_unique > 1)

    if (nrow(consistency_check) > 0) {
      msg <- paste0(msg_counter, ": Fehler: ", listelement, " – mindestens eine ", label, "ID hat unterschiedliche Attributwerte. Betroffene Spalten: ", paste(unique(consistency_check$column), collapse = ", "), ".")
      log_error(msg)
      return(list(raw = consistency_check, distinct = NULL, message = msg, valid = FALSE))
    }

    if (check_id_name_mapping) {
      name_col <- grep("Name", names(data), value = TRUE)
      if (length(name_col) == 1) {

        # TRUE, wenn die Namensspalte z. B. "personName" heisst
        allow_na <- any(sapply(allow_na_if_name_contains, function(pattern) {
          grepl(pattern, tolower(name_col))
        }))

        # Anzahl unterschiedlicher Namen pro ID (inkl. NA nur, wenn nicht erlaubt)
        id_to_name_viol <- data %>%
          { if (!allow_na) filter(., !is.na(.data[[name_col]])) else . } %>%
          group_by(.data[[id_col_name]]) %>%
          summarise(n_names = n_distinct(.data[[name_col]]), .groups = "drop") %>%
          filter(n_names > 1)

        if (nrow(id_to_name_viol) > 0) {
          msg <- paste0(msg_counter, ": Fehler: ", listelement, " – IDs nicht eindeutig auf Namen abbildbar.")
          log_error(msg)
          return(list(raw = id_to_name_viol, distinct = NULL, message = msg, valid = FALSE))
        }

        # Anzahl unterschiedlicher IDs pro Name (inkl. NA nur, wenn nicht erlaubt)
        name_to_id_viol <- data %>%
          { if (!allow_na) filter(., !is.na(.data[[name_col]])) else . } %>%
          group_by(.data[[name_col]]) %>%
          summarise(n_ids = n_distinct(.data[[id_col_name]]), .groups = "drop") %>%
          filter(n_ids > 1)

        if (nrow(name_to_id_viol) > 0) {
          msg <- paste0(msg_counter, ": Fehler: ", listelement, " – Namen nicht eindeutig auf IDs abbildbar.")
          log_error(msg)
          return(list(raw = name_to_id_viol, distinct = NULL, message = msg, valid = FALSE))
        }
      }
    }


    msg <- paste0(msg_counter, ": OK: ", listelement, " sind konsistent (jede ID hat eindeutige Attribute", if (check_id_name_mapping) " und eindeutigen Namen" else "", ").")
    log_message(msg)

    raw_result <- data
    distinct_result <- data %>%
      distinct() %>%
      group_by(.data[[id_col_name]]) %>%
      summarise(across(everything(), ~ unique(.x)[1]), .groups = "drop")

    return(list(raw = raw_result, distinct = distinct_result, message = msg, valid = TRUE))
  }

  if (is.character(input) && length(input) == 1) {
    if (file.exists(input)) {
      log_message(paste0(msg_counter, ": Hinweis: JSON-Datei erkannt – wird eingelesen."))
      input <- jsonlite::fromJSON(input, simplifyDataFrame = TRUE)
    } else {
      log_message(paste0(msg_counter, ": Hinweis: JSON-String erkannt – wird geparst."))
      input <- tryCatch(
        jsonlite::fromJSON(input, simplifyDataFrame = TRUE),
        error = function(e) {
          stop(paste0("Input-String konnte nicht als JSON geparst werden: ", e$message), call. = FALSE)
        }
      )
    }
  }


  validation_input_list <- if (is.list(input) && !is.data.frame(input)) {
    input
  } else {
    input_list <- list()
    for (i in seq_along(schema_names$`$defs`)) {
      nam <- names(schema_names$`$defs`)[i]
      matched_props <- grep(nam, names(schema_names$properties), value = TRUE)

      if (length(matched_props) > 0) {
        key_name <- matched_props[1]

        # Conditional column selection
        if (tolower(nam) == prefix_nachweis_obj) {
          selected_cols <- dplyr::select(input, dplyr::contains(prefix_nachweis_obj) | dplyr::contains("ID"))
        } else {
          selected_cols <- dplyr::select(input, dplyr::contains(tolower(nam)))
        }

        input_list[[key_name]] <- selected_cols
      }
    }

    input_list
  }


  # ------------------------------------------------------------
  # Pruefe fehlende Felder
  # ------------------------------------------------------------
  missing_fields <- list()
  for (table_name in names(validation_input_list)) {
    data <- validation_input_list[[table_name]]
    if (is.null(data) || ncol(data) == 0) {
      missing_fields[[table_name]] <- expected_fields[[table_name]]
    } else {
      missing_fields[[table_name]] <- setdiff(expected_fields[[table_name]], colnames(data))
    }
  }

  # ------------------------------------------------------------
  # Pruefen und Summarisieren
  # ------------------------------------------------------------
  validation_results <- list()

  for(i in seq_along(validation_input_list)) {
      nam <- names(schema_names$`$defs`)[i]
      prop <- paste0(tolower(nam), "ID")
      obj <- names(validation_input_list)[i]

      validation_results[[obj]] <- check_and_summarise(validation_input_list[[obj]], prop, nam, obj, allow_empty = FALSE, require_attributes = TRUE)
  }

  er <- validation_input_list$Ereignisse

  id_to_name_check <- er %>%
    filter(!is.na(ereignisID) & !is.na(ereignisName)) %>%
    group_by(ereignisID) %>%
    summarise(namen = n_distinct(ereignisName), .groups = "drop")

  # Pruefe: Welcher Name ist mehreren IDs zugeordnet?
  name_to_id_check <- er %>%
    filter(!is.na(ereignisID) & !is.na(ereignisName)) %>%
    group_by(ereignisName) %>%
    summarise(ids = n_distinct(ereignisID), .groups = "drop") %>%
    filter(ids > 1)
  er <- validation_input_list$Projekte
  # ------------------------------------------------------------
  # JSON-Building
  # ------------------------------------------------------------
  validation_list <- list()

  for(i in seq_along(validation_results)){
    name <- names(validation_results)[i]
    validation_list[[name]] <- validation_results[[i]]$raw
  }
  validation_list <- lapply(validation_list, function(x) if (is.null(x)) list() else x)
  validation_json <- jsonlite::toJSON(validation_list, na = "null", auto_unbox = TRUE, pretty = TRUE)

  export_list <- list()

  for(i in seq_along(validation_results)){
    name <- names(validation_results)[i]
    export_list[[name]] <- validation_results[[i]]$distinct
  }

  export_list <- lapply(export_list, function(x) if (is.null(x)) list() else x)
  export_json <- jsonlite::toJSON(export_list, na = "null", auto_unbox = TRUE, pretty = TRUE)

  # ------------------------------------------------------------
  # Schema-Validation
  # ------------------------------------------------------------
  is_valid <- NULL
  validation_errors <- NULL
  if (!is.null(schema)) {
    schema_json <- get_schema_json(schema)
    validator <- jsonvalidate::json_validator(schema_json, engine = "ajv")
    is_valid <- validator(validation_json, verbose = verbose)
    validation_errors <- attr(is_valid, "errors")
    if (!is.null(validation_errors)) {
      msg <- paste0(msg_counter, ": Warnung: JSON-Validierung hat Fehler ergeben.")
      log_message(msg)
    } else {
      msg <- paste0(msg_counter, ": OK: JSON ist gueltig laut Schema.")
      log_message(msg)
    }
  }

  # ------------------------------------------------------------
  # Return
  # ------------------------------------------------------------
  list(
    check_results = validation_results,
    validation_json = validation_json,
    export_json = export_json,
    json_valid = is_valid,
    validation_errors = validation_errors,
    missing_fields = missing_fields
  )
}
