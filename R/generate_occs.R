#' Generiert einen synthetischen Nachweisdatensatz
#'
#' Diese Funktion erzeugt einen grossen, synthetischen Datensatz mit Nachweisen,
#' inklusive Taxa, Projekten, Ereignissen, Lokalitaeten und Personen-IDs.
#' Sie kann fuer Entwicklung, Tests oder Demonstrationen genutzt werden.
#'
#' Die Funktion prueft dabei, dass die Untertabellen nicht mehr Zeilen enthalten, als
#' es fuer die Lookup-IDs (Projekte, Ereignisse, Lokalitaeten, Personen) maximal sinnvoll ist.
#'
#' Maximalwerte sind:
#' - n_lokalitaet <= 20
#' - n_ereignis <= 30
#' - n_projekt <= 50
#' - n_person <= 40
#'
#' @param n Integer. Anzahl der Nachweiszeilen im Endergebnis. Beliebig gross moeglich.
#' @param n_lokalitaet Integer. Anzahl der unterschiedlichen Lokalitaeten (max. 20). Default = 20.
#' @param n_ereignis Integer. Anzahl der unterschiedlichen Ereignisse (max. 30). Default = 30.
#' @param n_projekt Integer. Anzahl der unterschiedlichen Projekte (max. 50). Default = 50.
#' @param n_person Integer. Anzahl der unterschiedlichen Personen (max. 40). Default = 40.
#' @param seed Integer. Startwert fuer den Zufallszahlengenerator. Default = 42.
#'
#' @return Ein tibble mit n Zeilen und synthetischen Nachweisdaten
#'
#' @importFrom dplyr mutate case_when left_join rename slice_sample
#' @importFrom tibble tribble tibble
#' @importFrom stringr str_pad
#' @importFrom uuid UUIDgenerate
#' @importFrom purrr map_chr
#' @export
generate_occs <- function(
    n = 1000,
    n_lokalitaet = 20,
    n_ereignis = 30,
    n_projekt = 50,
    n_person = 40,
    seed = 42
) {
  set.seed(seed)

  # ------------------------------------------------
  # ✅ Plausibilitaetspruefungen
  # ------------------------------------------------
  if (n_lokalitaet > 20) stop("Fehler: n_lokalitaet darf maximal 20 sein.")
  if (n_ereignis > 30) stop("Fehler: n_ereignis darf maximal 30 sein.")
  if (n_projekt > 50) stop("Fehler: n_projekt darf maximal 50 sein.")
  if (n_person > 40) stop("Fehler: n_person darf maximal 40 sein.")

# ------------------------------------------------
# 🌿 Basistabelle fuer Taxa mit deutschen Namen
# ------------------------------------------------

taxon_table_raw <- tribble(
  ~nachweisTaxonNameAkzeptiert, ~nachweisTaxonName, ~nachweisBestimmungWoertlich, ~nachweisTaxonRang,

  # familia
  "Asteraceae", "Compositae", "Korbbluetler", "familia",
  "Rosaceae", "Rosaceae sensu lato", "Rosengewaechse", "familia",
  "Poaceae", "Gramineae", "Suessgraeser", "familia",
  "Fabaceae", "Leguminosae", "Huelsenfruechtler", "familia",
  "Brassicaceae", "Cruciferae", "Kreuzbluetler", "familia",

  # genus (mit spec.)
  "Quercus spec.", "Quercus sect. Quercus", "Eichen", "genus",
  "Fagus spec.", "Fagus subg. Fagus", "Buchen", "genus",
  "Betula spec.", "Betula sect. Betula", "Birken", "genus",
  "Salix spec.", "Salix sect. Vetrix", "Weiden", "genus",
  "Ulmus spec.", "Ulmus subg. Ulmus", "Ulmen", "genus",
  "Acer spec.", "Acer sect. Platanoidea", "Ahorn", "genus",
  "Alnus spec.", "Alnus subg. Alnus", "Erlen", "genus",
  "Populus spec.", "Populus sect. Populus", "Pappeln", "genus",
  "Fraxinus spec.", "Fraxinus sect. Fraxinus", "Eschen", "genus",
  "Prunus spec.", "Prunus subg. Prunus", "Pflaumenartige", "genus",

  # species
  "Quercus robur", "Quercus pedunculata", "Stieleiche", "species",
  "Fagus sylvatica", "Fagus ferruginea", "Rotbuche", "species",
  "Betula pendula", "Betula alba var. pendula", "Haengebirke", "species",
  "Acer platanoides", "Acer platanoides var. globosum", "Spitzahorn", "species",
  "Acer pseudoplatanus", "Acer pseudoplatanus subsp. macrocarpum", "Bergahorn", "species",
  "Salix caprea", "Salix caprea var. cinerea", "Salweide", "species",
  "Ulmus glabra", "Ulmus scabra", "Bergulme", "species",
  "Populus tremula", "Populus tremuloides", "Zitterpappel", "species",
  "Fraxinus excelsior", "Fraxinus oxyphylla", "Gemeine Esche", "species",
  "Tilia cordata", "Tilia parvifolia", "Winterlinde", "species",
  "Tilia platyphyllos", "Tilia grandifolia", "Sommerlinde", "species",
  "Alnus glutinosa", "Alnus vulgaris", "Schwarzerle", "species",
  "Sorbus aucuparia", "Pyrus aucuparia", "Eberesche", "species",
  "Corylus avellana", "Corylus maxima var. avellana", "Haselstrauch", "species",
  "Prunus avium", "Cerasus avium", "Vogelkirsche", "species",
  "Prunus spinosa", "Prunus domestica var. spinosa", "Schlehe", "species",

  # varietas
  "Acer platanoides var. globosum", "Acer platanoides", "Spitzahorn-Kugelvarietas", "varietas",
  "Salix caprea var. cinerea", "Salix caprea", "Grauweidenvarietas", "varietas",
  "Ulmus glabra var. montana", "Ulmus glabra", "Bergulmenvarietas", "varietas",
  "Betula pendula var. carelica", "Betula pendula", "Karelische Birke", "varietas",
  "Fagus sylvatica var. tortuosa", "Fagus sylvatica", "Schlangenbuche", "varietas",
  "Quercus robur var. fastigiata", "Quercus robur", "Saeuleneiche", "varietas"
)


taxon_table <- taxon_table_raw %>%
  mutate(
    nachweisTaxonNameID = paste0("NameID", str_pad(1:nrow(taxon_table_raw), 3, pad = "0")),
    nachweisTaxonNameAkzeptiertID = paste0("AkzeptiertID", str_pad(1:nrow(taxon_table_raw), 3, pad = "0"))
  )

plants <- taxon_table %>% slice_sample(n = n, replace = T)
# ------------------------------------------------
# 🏛️ Herbar-Institute mit Kontaktdaten
# ------------------------------------------------
herbarium_table <- tribble(
  ~Code, ~Stadt, ~KontaktAdresse, ~Email, ~Telefon,
  "K", "London", "Royal Botanic Gardens, Kew", "info@kew.org", "+44 20 8332 5655",
  "NY", "New York", "New York Botanical Garden", "herbarium@nybg.org", "+1 718 817 8600",
  "US", "Washington", "Smithsonian Institution", "usnh@si.edu", "+1 202 633 1000",
  "BM", "London", "Natural History Museum", "info@nhm.ac.uk", "+44 20 7942 5000",
  "P", "Paris", "Muséum national d'Histoire naturelle", "herbier@mnhn.fr", "+33 1 40 79 30 00",
  "L", "Leiden", "Naturalis Biodiversity Center", "herbarium@naturalis.nl", "+31 71 751 9600",
  "MO", "St. Louis", "Missouri Botanical Garden", "herbarium@mobot.org", "+1 314 577 5100",
  "JE", "Jena", "Botanischer Garten Jena", "herbarium@uni-jena.de", "+49 3641 9 40000",
  "B", "Berlin", "Botanischer Garten Berlin", "herbarium@bgbm.org", "+49 30 838 50 100",
  "G", "Geneva", "Conservatoire et Jardin botaniques", "herbarium@ville-ge.ch", "+41 22 418 51 00"
)

# ------------------------------------------------
# 📌 Mapping fuer Haeufigkeitsklassen
# ------------------------------------------------
haeufigkeit_klassen <- tibble(
  wert = as.character(1:5),
  klasse = c(
    "1: 1–5 Individuen",
    "2: 6–20 Individuen",
    "3: >20 Individuen",
    "4: >50 Individuen",
    "5: >100 Individuen"
  )
)

# ------------------------------------------------
# 📌 Bemerkungstexte fuer Prueferstatus
# ------------------------------------------------
pruefung_bemerkungen <- list(
  plausibel = c(
    "Plausibel weil Habitat im Arealtyp liegt",
    "Plausibel weil Vorkommen aus Nachbargebieten bekannt",
    "Plausibel aufgrund geeigneter Standortbedingungen"
  ),
  unplausibel = c(
    "Unplausibel weil Standort ausserhalb des bekannten Areals liegt",
    "Unplausibel wegen fehlender Habitatstruktur"
  ),
  falsch = c(
    "Falsch weil Bestimmung vor Ort korrigiert wurde",
    "Falsch wegen Bestimmungsfehler"
  ),
  verifiziert = c(
    "Verifiziert durch Expertenpruefung",
    "Verifiziert durch Herbarbeleg",
    "Verifiziert durch Fotobeleg"
  )
)

# ------------------------------------------------
# 📜 Lizenzen
# ------------------------------------------------
data_use_licenses <- c(
  "CC0 1.0 Universal (Public Domain Dedication)",
  "CC BY 4.0 (Attribution)",
  "CC BY-SA 4.0 (Attribution-ShareAlike)",
  "CC BY-NC 4.0 (Attribution-NonCommercial)",
  "CC BY-ND 4.0 (Attribution-NoDerivatives)",
  "CC BY-NC-SA 4.0 (Attribution-NonCommercial-ShareAlike)",
  "CC BY-NC-ND 4.0 (Attribution-NoDerivatives)",
  NA
)

# ------------------------------------------------
# 🌳 Daten erzeugen
# ------------------------------------------------
ereignis_pool <- paste0("ID_Ereignis", str_pad(1:n_ereignis, 3, pad = "0"))
projekt_pool <- paste0("ID_Projekt", str_pad(1:n_projekt, 3, pad = "0"))
lokalitaet_pool <- paste0("ID_Lokalitaet", str_pad(1:n_lokalitaet, 3, pad = "0"))
person_pool <- paste0("ID_Person", str_pad(1:n_person, 3, pad = "0"))

daten <-  plants %>%
  mutate(
    ereignisID = ifelse(sample(c(TRUE, FALSE), n, replace=T), NA, ereignis_pool),
    projektID = sample(projekt_pool, n, replace = T),
    lokalitaetID = sample(lokalitaet_pool, n, replace=T),
    personID = ifelse(sample(c(TRUE, FALSE), n, replace=T), NA, person_pool),
    nachweisID = paste0("ID_Nachweis", str_pad(1:n, 3, pad = "0")),
    nachweisDatum = generate_mixed_iso8601(n = n, fraction_range = 0.4),
    nachweisVorkommenStatus = sample(c("Praesenz", "Absenz"), n, replace = TRUE),
    nachweisVorkommenAnsiedlung = sample(c("verwildert", "ausgebracht", "spontan", "kultiviert", NA), n, replace = TRUE),
    nachweisQuantitaetTyp = sample(
      c("KlasseHaeufigkeit", "ProzentDeckung", "DeckungBraunBlanquet", "Zaehleinheit", "DeckungLondo", NA),
      n, replace = TRUE
    )
  ) %>%
  mutate(
    nachweisQuantitaetWert = case_when(
      is.na(nachweisQuantitaetTyp) ~ NA_character_,
      nachweisQuantitaetTyp == "KlasseHaeufigkeit" ~ as.character(sample(1:5, n, replace = TRUE)),
      nachweisQuantitaetTyp == "Zaehleinheit" ~ as.character(sample(1:100, n, replace = TRUE)),
      nachweisQuantitaetTyp == "DeckungBraunBlanquet" ~ sample(c("r", "+", "1", "2m", "2a", "2b", "3", "4", "5"), n, replace = TRUE),
      nachweisQuantitaetTyp == "ProzentDeckung" ~ as.character(sample(0:100, n, replace = TRUE)),
      nachweisQuantitaetTyp == "DeckungLondo" ~ sample(c("+", "1", "2", "3", "4", "5", "6", "7"), n, replace = TRUE),
      TRUE ~ NA_character_
    )
  ) %>%
  # Join mit festen Klassen fuer KlasseHaeufigkeit
  left_join(haeufigkeit_klassen, by = c("nachweisQuantitaetWert" = "wert")) %>%
  mutate(
    nachweisQuantitaetKlassen = case_when(
      is.na(nachweisQuantitaetTyp) ~ NA_character_,
      nachweisQuantitaetTyp == "KlasseHaeufigkeit" ~ klasse,
      TRUE ~ NA_character_
    ),
    klasse = NULL
  ) %>%
  mutate(
    nachweisQuantitaetZaehlEinheit = case_when(
      is.na(nachweisQuantitaetTyp) ~ NA_character_,
      nachweisQuantitaetTyp == "Zaehleinheit" ~ sample(c("Individuen", "Sprosse", "Horste", "Flaeche"), n, replace = TRUE),
      TRUE ~ NA_character_
    ),
    nachweisQuantitaetMethode = case_when(
      is.na(nachweisQuantitaetTyp) ~ NA_character_,
      TRUE ~ sample(c("Zaehlung", "Berechnung", "Schaetzung", NA), n, replace = TRUE)
    ),
    nachweisBestimmungVon = sample(c(generate_names_field(n), rep(NA, n)), n, replace = T),
    nachweisBestimmungDatum = sample(c(generate_mixed_iso8601(n),rep(NA, n)), n, replace = T),
    nachweisBestimmungRevidiertVon = sample(c(generate_names_field(n), rep(NA, n)), n, replace = T),
    nachweisBestimmungRevidiertDatum = sample(c(generate_mixed_iso8601(n),rep(NA, n)), n, replace = T),
    nachweisBestimmungPruefStatus = sample(
      c("plausibel", "unplausibel", "falsch", "verifiziert", "ungeprueft", NA),
      size = n,
      replace = TRUE,
      prob = c(0.2, 0.1, 0.2, 0.1, 0.3, 0.1)
    ),
    nachweisBestimmungPruefStatusInfo = case_when(
      is.na(nachweisBestimmungPruefStatus) ~ NA_character_,
      nachweisBestimmungPruefStatus == "ungeprueft" ~ NA_character_,
      nachweisBestimmungPruefStatus == "plausibel" ~ sample(pruefung_bemerkungen$plausibel, n(), replace = TRUE),
      nachweisBestimmungPruefStatus == "unplausibel" ~ sample(pruefung_bemerkungen$unplausibel, n(), replace = TRUE),
      nachweisBestimmungPruefStatus == "falsch" ~ sample(pruefung_bemerkungen$falsch, n(), replace = TRUE),
      nachweisBestimmungPruefStatus == "verifiziert" ~ sample(pruefung_bemerkungen$verifiziert, n(), replace = TRUE),
      TRUE ~ NA_character_
    ),
    nachweisTaxonFloristischerStatus = sample(c("einheimsch", "Normalstatus", "synanthrop", "eingebuergert", NA), n , replace = T),
    nachweisTaxonFloristischerStatusBezugsRaum = case_when(
      is.na(nachweisTaxonFloristischerStatus) ~ NA,
      TRUE ~ sample(c("Bayern", "Sachsen", "NRW", "Mittelrheintal"), n, replace = TRUE)
    ),
    nachweisTaxonKonzept = "Mueller et al. 2021",
    nachweisTaxonKonzeptListe = "Mueller et al. 2021 > Schmeil und Fitschen 2011 > Oberdorfer 2001",
    nachweisTaxonRefDatenbank = "Recorder D ver. 6.25.1.284",
    nachweisTyp = sample(c("Beobachtung", "Beleg", "Quelle"), n , replace = T),
    nachweisDauerhaftigkeit = sample(c("Dauerhaft", "Unbestaendig", NA), n, replace = T),
    nachweisBelegID = case_when(
      nachweisTyp == "Beleg" ~ UUIDgenerate()
    ),
    nachweisBelegSammlungsInstitut = case_when(
      nachweisTyp == "Beleg" ~ sample(herbarium_table$Code, n, replace =T)
    )) %>%
  left_join(herbarium_table, by= c("nachweisBelegSammlungsInstitut" = "Code")) %>%
  rename(nachweisBelegSammlungsInstitutOrt = Stadt,
         nachweisBelegSammlungsInstitutKontakt = KontaktAdresse,
         nachweisBelegSammlungsInstitutEmailAdresse = Email,
         nachweisBelegSammlungsInstitutTelefonNummer = Telefon) %>%
  mutate(
    nachweisBelegSammlungsName = case_when(
      nachweisTyp == "Beleg" ~ sample(c("Samenpflanzen Deu-tschlands",
                                        "Weimar 77",
                                        "Sammlung Robert Becker",
                                        "Botanische Exkursion Nettetal-2000", NA), n, replace = T)
    ),
    nachweisQuelleTyp = case_when(
      nachweisTyp == "Quelle" ~ sample(c("Feldbuch", "Buch", "Zeitschriftenartikel", "Kartei/Archiv", "Onlinequelle"), n, replace = T)
    ),
    nachweisQuellePubliziert = case_when(
      nachweisTyp == "Quelle" ~ sample(c(TRUE, FALSE), n, replace = T)

    ),
    nachweisQuelleZitat = case_when(
      nachweisTyp != "Quelle" ~ NA_character_,
      nachweisQuelleTyp == "Zeitschriftenartikel" ~ "Mueller, A. (2020). Vorkommen seltener Farnarten im Hunsrueck. Botanische Zeitschrift, 128(2), 45-58. https://doi.org/10.1234/bz.2020.04558 ",
      nachweisQuelleTyp == "Buch" ~ "Schneider, L. (2020). Flora des Oberrheins (2. Aufl.). Botanischer Verlag Heidelberg. https://doi.org/10.1234/flora.2020.56789 ",
      nachweisQuelleTyp == "Onlinequelle" ~ "Krause, T. (2020, 15. Juni). Verbreitungsdaten von Saxifraga-Arten in Bayern. FloraData Online. https://www.floradata.de/verbreitung/saxifraga",
      nachweisQuelleTyp %in% c("Kartei/Archiv", "Feldbuch") ~ "Meier, F. (1992). Fundkartei Gefaesspflanzen Landkreis Marburg-Biedenkopf [Unveroeffentlichte Kartei]. Archiv des Botanischen Vereins Mittelhessen, Marburg",
      TRUE ~ NA_character_
    ),
    nachweisLizenz = sample(data_use_licenses, n, replace = T),
    nachweisVersion = generate_mixed_iso8601(n)


  )%>%
  mutate(across(where(is.character), enc2utf8))

return(daten)
}

