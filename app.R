# ------------------------------------------------------------------
# Mentaler Rotationstest
# Aufgabe: Ist die rechte Figur eine gedrehte oder eine gespiegelte
# Version der linken Figur?
# Start: shiny::runApp("app.R")
# ------------------------------------------------------------------

library(shiny)
library(ggplot2)

# ---- Figur-Generierung ----

norm_cells <- function(m) {
  m <- cbind(m[, 1] - min(m[, 1]), m[, 2] - min(m[, 2]))
  m[order(m[, 1], m[, 2]), , drop = FALSE]
}

rot90_cells   <- function(m) cbind(-m[, 2], m[, 1])
mirror_cells  <- function(m) cbind(-m[, 1], m[, 2])
same_cells    <- function(a, b) nrow(a) == nrow(b) && all(a == b)

# Figur darf unter keiner 90-Grad-Drehung mit ihrem Spiegelbild
# identisch sein, sonst wäre die Aufgabe nicht lösbar
is_mirror_symmetric <- function(m) {
  orig <- norm_cells(m)
  mm   <- mirror_cells(m)
  for (k in 0:3) {
    if (same_cells(norm_cells(mm), orig)) return(TRUE)
    mm <- rot90_cells(mm)
  }
  FALSE
}

gen_shape <- function(n_cells = 8) {
  repeat {
    cells <- matrix(c(0, 0), ncol = 2)
    while (nrow(cells) < n_cells) {
      base <- cells[sample(nrow(cells), 1), ]
      step <- rbind(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))[sample(4, 1), ]
      cand <- base + step
      if (!any(cells[, 1] == cand[1] & cells[, 2] == cand[2])) {
        cells <- rbind(cells, cand)
      }
    }
    if (!is_mirror_symmetric(cells)) return(cells)
  }
}

# Zellen -> Polygon-Koordinaten (zentriert, optional gespiegelt + gedreht)
shape_df <- function(cells, angle = 0, mirrored = FALSE) {
  cx <- mean(cells[, 1])
  cy <- mean(cells[, 2])
  corners <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    x <- cells[i, 1]; y <- cells[i, 2]
    data.frame(
      x  = c(x - .5, x + .5, x + .5, x - .5) - cx,
      y  = c(y - .5, y - .5, y + .5, y + .5) - cy,
      id = i
    )
  }))
  if (mirrored) corners$x <- -corners$x
  th <- angle * pi / 180
  data.frame(
    x  = corners$x * cos(th) - corners$y * sin(th),
    y  = corners$x * sin(th) + corners$y * cos(th),
    id = corners$id
  )
}

plot_shape <- function(df, fill = "#85B7EB") {
  lim <- max(abs(c(df$x, df$y))) + 0.6
  ggplot(df, aes(x, y, group = id)) +
    geom_polygon(fill = fill, colour = "white", linewidth = 0.6) +
    coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    theme_void()
}

# ---- UI ----

ui <- fluidPage(
  div(style = "text-align: center;",
      h4("Programmiert von Pawel R. Kulawiak (Universität zu Köln)"),
      img(src = "logo.svg.webp", width = "100px"),
      hr(),
      h2("Mentaler Rotationstest"),
      h4("Ist die rechte Figur eine ", strong("gedrehte"), " oder eine ",
        strong("gespiegelte"), " Version der linken Figur?"), hr()
  ),
  fluidRow(
    column(2),
    column(4, h4("Original", align = "center"),
           plotOutput("plot_target", height = "300px")),
    column(4, h4("Vergleichsfigur", align = "center"),
           plotOutput("plot_probe", height = "300px")),
    column(2)
  ),
  fluidRow(
    column(12, align = "center",
           br(),
           actionButton("btn_same",   "Gleiche Figur (gedreht)",
                        class = "btn-success btn-lg"),
           br(), br(),
           actionButton("btn_mirror", "Gespiegelte Figur",
                        class = "btn-danger btn-lg"),
           br(), br(),
           textOutput("feedback"),
           br(),
           hr(),
           br(),
           h2("Learning Analytics Dashboard"),
           br(),
           tableOutput("stats"),
           plotOutput("histo", height = "150px"),
           plotOutput("kuchen", height = "300px")
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    cells    = NULL,
    angle    = NULL,
    mirrored = NULL,
    t0       = NULL,
    feedback = "",
    results  = data.frame(korrekt = logical(0), rt = numeric(0))
  )
  
  new_trial <- function() {
    rv$cells    <- gen_shape(8)
    rv$angle    <- sample(c(45, 90, 135, 180, 225, 270, 315), 1)
    rv$mirrored <- sample(c(TRUE, FALSE), 1)
    rv$t0       <- Sys.time()
  }
  
  answer <- function(said_mirror) {
    rt      <- as.numeric(difftime(Sys.time(), rv$t0, units = "secs"))
    korrekt <- (said_mirror == rv$mirrored)
    rv$results  <- rbind(rv$results, data.frame(korrekt = korrekt, rt = rt))
    rv$feedback <- if (korrekt) {
      sprintf("Richtig! (Reaktionszeit %.1f s)", rt)
    } else {
      sprintf("Leider falsch. Die Figur war %s. (%.1f s)",
              ifelse(rv$mirrored, "gespiegelt", "nur gedreht"), rt)
    }
    new_trial()
  }
  
  new_trial()  # erster Durchgang
  
  observeEvent(input$btn_same,   answer(FALSE))
  observeEvent(input$btn_mirror, answer(TRUE))
  
  output$plot_target <- renderPlot({
    plot_shape(shape_df(rv$cells, angle = 0, mirrored = FALSE))
  })
  
  output$plot_probe <- renderPlot({
    plot_shape(shape_df(rv$cells, angle = rv$angle, mirrored = rv$mirrored))
  })
  
  output$feedback <- renderText(rv$feedback)
  
  output$stats <- renderTable({
    if (nrow(rv$results) == 0) return(NULL)
    data.frame(
      `Durchgänge`        = nrow(rv$results),
      `Korrekt`           = sum(rv$results$korrekt),
      `Trefferquote (%)`  = round(100 * mean(rv$results$korrekt), 1),
      `Mittlere Reaktionszeit (s)`   = round(mean(rv$results$rt), 2),
      `Min`   = round(min(rv$results$rt), 2),
      `Max`   = round(max(rv$results$rt), 2),
      `SD`   = round(sd(rv$results$rt), 2),
      check.names = FALSE
    )
  }, align = "c")
  
  output$histo <- renderPlot({
    if (nrow(rv$results) == 0) return(NULL)
    rv$results |>
      ggplot(aes(x = rt, color = factor(korrekt, levels = c(FALSE, TRUE)))) +
      geom_rug(linewidth = 1.5, length = unit(0.5, "npc")) +
      scale_color_manual(values = c("gray65", "#85B7EB"), drop = FALSE) +
      scale_x_continuous(limits = c(0, ceiling(max(rv$results$rt))), breaks = 0:ceiling(max(rv$results$rt))) +
      labs(x = "Reaktionszeiten (s)", y = NULL, color = "Korrekt") +
      theme_classic(base_size = 13) +
      theme(legend.position = "none")
  })
  
  output$kuchen <- renderPlot({
    if (nrow(rv$results) == 0) return(NULL)
    rv$results$korrekt |>
      factor(levels = c(FALSE, TRUE), labels = c("Falsch", "Richtig")) |>
      table() |>
      pie(col = c("#85B7EB", "gray65") |> rev())
  })
}

shinyApp(ui, server)