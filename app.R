# SAFMEDS vocabulary practice (Say All Fast a Minute Each Day Shuffled)
# Run with: shiny::runApp("safmeds")
library(shiny)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f6f7f9; }
    .card-box { background:#fff; border:2px solid #2b3f55; border-radius:14px;
                min-height:220px; padding:30px; text-align:center;
                display:flex; align-items:center; justify-content:center;
                font-size:30px; box-shadow:0 2px 8px rgba(0,0,0,.08); }
    .card-back { font-size:24px; color:#1f6f43; margin-top:14px; }
    .timer { font-size:54px; font-weight:700; text-align:center; color:#2b3f55; }
    .timer.low { color:#c0392b; }
    .btn-row .btn { margin:6px 4px; font-size:18px; padding:10px 22px; }
    .stat { font-size:20px; text-align:center; }
  "))),
  titlePanel("SAFMEDS Vocabulary Practice"),
  tabsetPanel(id = "tabs",
    tabPanel("1. Build your deck",
      br(),
      fluidRow(
        column(5,
          h4("Add a card"),
          textInput("word", "Vocabulary word", width = "100%"),
          textAreaInput("def", "Definition", rows = 3, width = "100%"),
          actionButton("add", "Add card", class = "btn-primary"),
          hr(),
          h4("Or paste many at once"),
          helpText("One card per line: word, then a Tab or | or comma, then the definition.
                   (Only the first separator on each line is used.)"),
          textAreaInput("bulk", NULL, rows = 6, width = "100%",
                        placeholder = "reinforcement | a consequence that increases future behavior"),
          actionButton("addbulk", "Add pasted cards")
        ),
        column(7,
          h4(textOutput("deck_count", inline = TRUE)),
          tableOutput("deck_table"),
          selectInput("rm_idx", "Remove a card", choices = NULL, width = "100%"),
          actionButton("remove", "Remove selected"),
          actionButton("clear", "Clear deck")
        )
      )
    ),
    tabPanel("2. Practice",
      br(),
      fluidRow(
        column(3,
          numericInput("secs", "Timing length (seconds)", value = 60, min = 5, max = 600, step = 5),
          radioButtons("front", "Show first",
                       c("Word" = "word", "Definition" = "def"), inline = TRUE),
          checkboxInput("shuffle", "Shuffle cards", TRUE),
          actionButton("start", "Start timing", class = "btn-success btn-lg"),
          actionButton("stop", "End early", class = "btn-warning")
        ),
        column(9,
          uiOutput("timer_ui"),
          uiOutput("card_ui"),
          div(class = "btn-row", style = "text-align:center;",
              uiOutput("buttons_ui")),
          uiOutput("live_stats")
        )
      )
    ),
    tabPanel("3. Results",
      br(),
      uiOutput("last_result"),
      h4("Words in this timing"),
      fluidRow(
        column(4, h5("Correct"), uiOutput("words_correct")),
        column(4, h5("Incorrect \u2014 review these"), uiOutput("words_incorrect")),
        column(4, h5("Skipped"), uiOutput("words_skipped"))
      ),
      br(),
      tableOutput("items_table"),
      hr(),
      plotOutput("progress_plot", height = "320px"),
      tableOutput("log_table"),
      downloadButton("dl", "Download summary (CSV)"),
      downloadButton("dl_items", "Download word-by-word results (CSV)")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    deck = data.frame(word = character(), def = character(), stringsAsFactors = FALSE),
    order = integer(), pos = 0L,
    running = FALSE, end_time = NULL, revealed = FALSE,
    correct = 0L, incorrect = 0L, skipped = 0L, seen = 0L,
    log = data.frame(timing = integer(), time = character(), seconds = numeric(),
                     correct = integer(), incorrect = integer(), skipped = integer(),
                     correct_pm = numeric(), incorrect_pm = numeric(),
                     stringsAsFactors = FALSE),
    last = NULL,
    items = data.frame(timing = integer(), word = character(), definition = character(),
                       outcome = character(), stringsAsFactors = FALSE)
  )

  # ---- deck building ----
  add_cards <- function(words, defs) {
    words <- trimws(words); defs <- trimws(defs)
    ok <- nzchar(words) & nzchar(defs)
    if (any(ok)) rv$deck <- rbind(rv$deck,
      data.frame(word = words[ok], def = defs[ok], stringsAsFactors = FALSE))
    sum(ok)
  }
  observeEvent(input$add, {
    n <- add_cards(input$word, input$def)
    if (n == 0) showNotification("Enter both a word and a definition.", type = "warning")
    else { updateTextInput(session, "word", value = ""); updateTextAreaInput(session, "def", value = "") }
  })
  observeEvent(input$addbulk, {
    lines <- trimws(strsplit(input$bulk, "\n", fixed = TRUE)[[1]])
    lines <- lines[nzchar(lines)]
    w <- character(); d <- character()
    for (ln in lines) {
      m <- regexpr("\t|\\||,", ln)
      if (m > 0) { w <- c(w, substr(ln, 1, m - 1)); d <- c(d, substr(ln, m + 1, nchar(ln))) }
    }
    n <- add_cards(w, d)
    showNotification(sprintf("Added %d card(s); %d line(s) skipped.", n, length(lines) - n),
                     type = if (n > 0) "message" else "warning")
    if (n > 0) updateTextAreaInput(session, "bulk", value = "")
  })
  observeEvent(input$remove, {
    i <- suppressWarnings(as.integer(input$rm_idx))
    if (!is.na(i) && i >= 1 && i <= nrow(rv$deck)) rv$deck <- rv$deck[-i, , drop = FALSE]
  })
  observeEvent(input$clear, rv$deck <- rv$deck[0, , drop = FALSE])
  observe({
    n <- nrow(rv$deck)
    ch <- if (n) setNames(seq_len(n), paste(seq_len(n), rv$deck$word)) else character()
    updateSelectInput(session, "rm_idx", choices = ch)
  })
  output$deck_count <- renderText(sprintf("Deck: %d card(s)", nrow(rv$deck)))
  output$deck_table <- renderTable({
    if (!nrow(rv$deck)) return(NULL)
    data.frame(`#` = seq_len(nrow(rv$deck)), Word = rv$deck$word,
               Definition = rv$deck$def, check.names = FALSE)
  }, striped = TRUE, width = "100%")

  # ---- practice ----
  new_order <- function() {
    n <- nrow(rv$deck)
    rv$order <- if (isolate(input$shuffle)) sample.int(n) else seq_len(n)
    rv$pos <- 1L
  }
  record <- function(outcome) {
    cd <- rv$deck[rv$order[rv$pos], ]
    rv$items <- rbind(rv$items, data.frame(timing = nrow(rv$log) + 1L, word = cd$word,
                      definition = cd$def, outcome = outcome, stringsAsFactors = FALSE))
  }
  advance <- function() {
    rv$seen <- rv$seen + 1L
    rv$revealed <- FALSE
    if (rv$pos >= length(rv$order)) new_order() else rv$pos <- rv$pos + 1L
  }
  observeEvent(input$start, {
    if (nrow(rv$deck) < 1) {
      showNotification("Add at least one card first.", type = "warning"); return()
    }
    secs <- input$secs
    if (is.na(secs) || secs < 5) secs <- 60
    rv$correct <- 0L; rv$incorrect <- 0L; rv$skipped <- 0L; rv$seen <- 0L
    rv$revealed <- FALSE; rv$running <- TRUE
    rv$end_time <- Sys.time() + secs
    rv$secs_used <- secs
    new_order()
  })
  observeEvent(input$stop, { if (rv$running) finish(early = TRUE) })

  finish <- function(early = FALSE) {
    if (!rv$running) return()
    rv$running <- FALSE
    elapsed <- if (early) max(1, as.numeric(difftime(Sys.time(), rv$end_time - rv$secs_used, units = "secs")))
               else rv$secs_used
    k <- nrow(rv$log) + 1L
    row <- data.frame(timing = k, time = format(Sys.time(), "%Y-%m-%d %H:%M"),
                      seconds = round(elapsed, 1),
                      correct = rv$correct, incorrect = rv$incorrect, skipped = rv$skipped,
                      correct_pm = round(rv$correct * 60 / elapsed, 1),
                      incorrect_pm = round(rv$incorrect * 60 / elapsed, 1),
                      stringsAsFactors = FALSE)
    rv$log <- rbind(rv$log, row); rv$last <- row
    updateTabsetPanel(session, "tabs", selected = "3. Results")
  }

  tick <- reactiveTimer(250)
  remaining <- reactive({
    tick()
    if (!rv$running) return(NA_real_)
    max(0, as.numeric(difftime(rv$end_time, Sys.time(), units = "secs")))
  })
  observe({
    r <- remaining()
    if (isTRUE(rv$running) && !is.na(r) && r <= 0) finish()
  })

  output$timer_ui <- renderUI({
    if (!rv$running) return(div(class = "timer", "Ready"))
    r <- ceiling(remaining())
    div(class = if (r <= 10) "timer low" else "timer",
        sprintf("%d:%02d", r %/% 60, r %% 60))
  })
  cur <- reactive({
    req(rv$running, rv$pos >= 1, rv$pos <= length(rv$order))
    rv$deck[rv$order[rv$pos], ]
  })
  output$card_ui <- renderUI({
    if (!rv$running) return(div(class = "card-box", style = "color:#888;",
      "Build a deck, set the time, then press Start timing."))
    cd <- cur()
    front <- if (input$front == "word") cd$word else cd$def
    back  <- if (input$front == "word") cd$def  else cd$word
    tagList(div(class = "card-box", front),
            if (rv$revealed) div(class = "card-back", back))
  })
  output$buttons_ui <- renderUI({
    if (!rv$running) return(NULL)
    if (!rv$revealed) {
      tagList(actionButton("reveal", "Show answer", class = "btn-primary"),
              actionButton("skip", "Next card"))
    } else {
      tagList(actionButton("right", "✓ I got it right", class = "btn-success"),
              actionButton("wrong", "✗ I got it wrong", class = "btn-danger"),
              actionButton("skip", "Next card"))
    }
  })
  observeEvent(input$reveal, { if (rv$running) rv$revealed <- TRUE })
  observeEvent(input$right,  { if (rv$running) { record("Correct"); rv$correct <- rv$correct + 1L; advance() } })
  observeEvent(input$wrong,  { if (rv$running) { record("Incorrect"); rv$incorrect <- rv$incorrect + 1L; advance() } })
  observeEvent(input$skip,   { if (rv$running) { record("Skipped"); rv$skipped <- rv$skipped + 1L; advance() } })

  output$live_stats <- renderUI({
    if (!rv$running) return(NULL)
    div(class = "stat", br(),
        sprintf("Correct: %d   |   Incorrect: %d   |   Skipped: %d",
                rv$correct, rv$incorrect, rv$skipped))
  })

  # ---- results ----
  output$last_result <- renderUI({
    if (is.null(rv$last)) return(p("Complete a timing to see results here."))
    l <- rv$last
    div(class = "stat",
        h3(sprintf("Timing %d", l$timing)),
        sprintf("Correct per minute: %.1f   |   Incorrect per minute: %.1f   |   Skipped: %d   |   Length: %.0f s",
                l$correct_pm, l$incorrect_pm, l$skipped, l$seconds), br(), br())
  })
  last_items <- reactive({
    req(rv$last)
    rv$items[rv$items$timing == rv$last$timing, , drop = FALSE]
  })
  word_list <- function(outcome) {
    renderUI({
      if (is.null(rv$last)) return(NULL)
      it <- last_items(); it <- it[it$outcome == outcome, , drop = FALSE]
      if (!nrow(it)) return(p(em("None")))
      tags$ul(lapply(unique(it$word), function(w) tags$li(w)))
    })
  }
  output$words_correct   <- word_list("Correct")
  output$words_incorrect <- word_list("Incorrect")
  output$words_skipped   <- word_list("Skipped")
  output$items_table <- renderTable({
    if (is.null(rv$last)) return(NULL)
    it <- last_items()
    if (!nrow(it)) return(NULL)
    data.frame(`#` = seq_len(nrow(it)), Word = it$word, Definition = it$definition,
               Outcome = it$outcome, check.names = FALSE)
  }, striped = TRUE, width = "100%")
  output$progress_plot <- renderPlot({
    lg <- rv$log
    if (!nrow(lg)) return(NULL)
    ymax <- max(5, lg$correct_pm, lg$incorrect_pm) * 1.15
    plot(lg$timing, lg$correct_pm, type = "b", pch = 16, col = "#1f6f43", lwd = 2,
         ylim = c(0, ymax), xaxt = "n", xlab = "Timing", ylab = "Count per minute",
         main = "Progress across timings")
    axis(1, at = lg$timing)
    lines(lg$timing, lg$incorrect_pm, type = "b", pch = 4, col = "#c0392b", lwd = 2)
    legend("topleft", c("Correct", "Incorrect"), col = c("#1f6f43", "#c0392b"),
           pch = c(16, 4), lwd = 2, bty = "n")
  })
  output$log_table <- renderTable(rv$log, striped = TRUE, width = "100%")
  output$dl <- downloadHandler(
    filename = function() paste0("safmeds_results_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$log, file, row.names = FALSE))
  output$dl_items <- downloadHandler(
    filename = function() paste0("safmeds_words_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$items, file, row.names = FALSE))
}

shinyApp(ui, server)
