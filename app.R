# Soziometrische Befragung - Umfrage-Bereich (Schulklasse 3b)
# Standalone Shiny App (ohne Quarto, ohne Google Sheets)
#
# Datenhaltung:
#   - klassen/klasse3b.xlsx ......... Schüler:innen, Passwörter, Frage (Spalten: name, passwort, frage)
#                                     (statischer Input, liegt im Repo/Image)
#   - <ANTWORT_DIR>/antworten_<name>.xlsx ... eine Excel-Datei pro Schüler:in
#                                     (dynamischer Output, liegt in Produktion auf dem Volume)
#
# Deployment (Sliplane):
#   - Volume mit Mount-Pfad /data an den Service anhängen
#   - Umgebungsvariable ANTWORT_DIR=/data setzen
#   Lokal (ohne Umgebungsvariable) landen die Antworten in antworten_lokal/
#   -> antworten_lokal/ in .gitignore aufnehmen

library(shiny)
library(bslib)
library(tidyverse)
library(readxl)   # Excel lesen
library(writexl)  # Excel schreiben

# ---------------------------------------------------------------------------
# Pfade und statische Daten
# ---------------------------------------------------------------------------

KLASSEN_DATEI <- "klassen/klasse3b.xlsx"

DATA <- read_xlsx(KLASSEN_DATEI)

### Ordner für Antwortdateien: Volume in Produktion, lokaler Ordner beim Entwickeln
ANTWORT_DIR <- Sys.getenv("ANTWORT_DIR", "antworten_lokal")
dir.create(ANTWORT_DIR, showWarnings = FALSE, recursive = TRUE)

### Dateiname für das Antwortmuster einer Schüler:in
antwort_datei <- function(name) {
  file.path(ANTWORT_DIR, paste0("antworten_", str_replace_all(name, "[^A-Za-z0-9_\\-]", "_"), ".xlsx"))
}

### Wer hat bereits geantwortet? (= für wen existiert schon eine Antwortdatei?)
### Als Funktion, damit der Stand bei jedem Aufruf frisch vom Dateisystem kommt
get_submitted <- function() {
  DATA$name[map_lgl(DATA$name, \(x) file.exists(antwort_datei(x)))]
}

# ---------------------------------------------------------------------------
# User Interface
# ---------------------------------------------------------------------------

ui <- page_fluid(
  theme = bs_theme(preset = "minty"),
  lang = "de",
  title = "Soziometrische Befragung",
  
  ## Title-Block-Banner (wie in Quarto)
  div(
    class = "bg-primary text-white p-4 mb-4",
    h1("Soziometrische Befragung", class = "mb-1"),
    p("Umfrage-Bereich (Schulklasse 3b)", class = "lead mb-2"),
    p(
      class = "mb-0 small",
      a(
        "Pawel R. Kulawiak - Universität zu Köln",
        href = "https://pawelkulawiak.github.io/",
        target = "_blank", class = "text-white"
      ),
      " | ",
      a(
        "Sergej Wüthrich - PH Bern",
        href = "https://www.phbern.ch/ueber-die-phbern/personen/sergej-wuethrich",
        target = "_blank", class = "text-white"
      )
    )
  ),
  
  uiOutput("login_ui"),
  uiOutput("survey_ui")
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  ### Pro Session frisch ermitteln, wer noch nicht geantwortet hat
  available <- reactiveVal(setdiff(DATA$name, get_submitted()))
  
  logged_in <- reactiveVal(FALSE)
  
  output$login_ui <- renderUI({
    if (logged_in()) return(NULL)
    tagList(
      layout_columns(
        selectInput("student_name", "Wer bist du?", choices = available()),
        passwordInput("student_password", "Passwort")
      ),
      actionButton("login", "Anmelden")
    )
  })
  
  observeEvent(input$login, {
    req(!logged_in())
    selected <- DATA |> filter(name == input$student_name)
    if (nrow(selected) == 1 && selected$passwort == input$student_password) {
      logged_in(TRUE)
    } else {
      showNotification("Falsches Passwort.", type = "error")
    }
  })
  
  output$survey_ui <- renderUI({
    req(logged_in())
    peers <- DATA$name[DATA$name != input$student_name]
    tagList(
      h2(unique(DATA$frage)),
      checkboxGroupInput("nominations", "Wähle eine oder mehrere Personen:", choices = peers),
      h2("Magst du lieber Singen oder Tanzen?"),
      radioButtons("hobby", "Wähle eine Option:", choices = c("Singen", "Tanzen"), selected = character(0)),
      br(),
      actionButton("submit", "Antwort abschicken")
    )
  })
  
  observeEvent(input$submit, {
    req(logged_in())
    if (is.null(input$nominations)) {
      showNotification("Bitte mindestens eine Person auswählen.", type = "error", duration = 30)
      return()
    }
    if (is.null(input$hobby)) {
      showNotification("Bitte Singen oder Tanzen auswählen.", type = "error", duration = 30)
      return()
    }
    
    ### Schutz vor Doppelabgabe (z. B. zwei offene Browser-Tabs derselben Person)
    if (file.exists(antwort_datei(input$student_name))) {
      showNotification("Für dich wurde bereits eine Antwort gespeichert.", type = "warning", duration = NULL)
      logged_in(FALSE)
      available(setdiff(DATA$name, get_submitted()))
      return()
    }
    
    new_rows <- tibble(
      Sender    = input$student_name,
      Empfänger = input$nominations,
      Frage     = unique(DATA$frage),
      Hobby     = input$hobby
    )
    
    ### Antwortmuster speichern: eine Excel-Datei pro Schüler:in
    write_xlsx(new_rows, antwort_datei(input$student_name))
    
    available(setdiff(DATA$name, get_submitted()))
    showNotification("Antwort gespeichert. Danke!", type = "message", duration = NULL)
    logged_in(FALSE)
  })
}

shinyApp(ui, server)
