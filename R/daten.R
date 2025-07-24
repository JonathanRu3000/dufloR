#' Beispiel-Datensatz: Synthetischer floristischer Nachweisdatensatz
#'
#' Dieser Datensatz wurde mit der Funktion \code{\link{generate_floristic_occdata}} erzeugt und dient
#' als Demonstrations- und Testdatenbasis fuer Validierungen floristischer Funddaten gemaess dem
#' DUFLOR-Datenstandard. Er enthaelt zufaellig generierte Angaben zu Taxa, Fundorten, Projekten,
#' Beobachtern, Ereignissen, Herbarbelegen und Quellen.
#'
#' Die enthaltenen Variablen decken alle im DUFLOR-Schema geforderten Felder fuer Nachweise ab.
#' Die Taxa umfassen Arten, Gattungen und Familien. Die Verknuepfungen zu Projekten, Personen,
#' Ereignissen und Lokalitaeten erfolgen ueber IDs. Teilweise sind Felder bewusst auf \code{NA}
#' gesetzt, um typische Luecken aus realen Daten zu simulieren.
#'
#' @format Ein Data Frame mit 1.000 Zeilen und ueber 40 Spalten:
#' \describe{
#'   \item{nachweisID}{Eindeutige ID des Nachweises}
#'   \item{nachweisDatum}{Funddatum im ISO-Format oder Intervall}
#'   \item{nachweisTaxonNameAkzeptiert}{Akzeptierter wissenschaftlicher Name}
#'   \item{nachweisTaxonName}{Synonym oder verwendeter Name}
#'   \item{nachweisBestimmungWoertlich}{Deutscher Name laut Quelle}
#'   \item{nachweisVorkommenStatus}{Status: "Praesenz" oder "Absenz"}
#'   \item{nachweisVorkommenAnsiedlung}{Herkunft, z. B. "ausgebracht", "verwildert"}
#'   \item{nachweisQuantitaetTyp}{Typ der Quantitaetsangabe}
#'   \item{nachweisQuantitaetWert}{Wert je nach Typ (z. B. Klasse, Deckung, Anzahl)}
#'   \item{nachweisQuantitaetKlassen}{Interpretation der KlasseHaeufigkeit}
#'   \item{nachweisBestimmungVon}{Name der bestimmenden Person}
#'   \item{nachweisBestimmungPruefStatus}{Plausibilitaet: z. B. "plausibel", "verifiziert"}
#'   \item{projektID}{Verweis auf Projekt}
#'   \item{lokalitaetID}{Verweis auf Lokalitaet}
#'   \item{personID}{Verweis auf zugeordnete Person (optional)}
#'   \item{ereignisID}{Verweis auf Ereignis (optional)}
#'   \item{nachweisTyp}{Typ des Nachweises: "Beleg", "Quelle", "Beobachtung"}
#'   \item{nachweisBelegSammlungsInstitut}{Herbarium-Code nach Index Herbariorum}
#'   \item{nachweisQuelleTyp}{Typ der Quelle (wenn vorhanden)}
#'   \item{nachweisQuelleZitat}{Zitierfaehiger Nachweis fuer Quellen}
#'   \item{nachweisLizenz}{Verwendete Lizenz (z. B. CC-BY)}
#'   \item{...}{Weitere Felder entsprechend DUFLOR-Schema}
#' }
#'
#' @usage data(test_nachweisdaten)
#' @keywords datasets
"test_nachweisdaten"
