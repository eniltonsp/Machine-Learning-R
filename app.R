library(shiny)
library(bslib)
library(ggplot2)
source("R/data.R", local = TRUE)
source("R/model.R", local = TRUE)
options(shiny.maxRequestSize = 25 * 1024^2, shiny.sanitize.errors = TRUE)

ui <- page_sidebar(
  title = "Consumer Complaint Intelligence",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    p("By Enilton Machado · Academic project, modernised portfolio edition"),
    radioButtons("source", "Data source", c("Synthetic demonstration" = "demo", "Upload CSV / TSV" = "upload")),
    conditionalPanel("input.source == 'upload'",
      fileInput("file", "Complaint data (up to 25 MB)", accept = c(".csv", ".txt", ".tsv")),
      selectInput("delimiter", "Delimiter", c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
      selectInput("encoding", "Encoding", c("UTF-8", "Windows-1252")),
      checkboxInput("backslash", "R-style backslash-escaped quotes", FALSE),
      helpText("Required: company/Empresa and status/Status. Optional: date/Data, text/Reclamacao and model fields.")),
    selectInput("company", "Company", choices = "All"),
    uiOutput("dates"),
    helpText("Uploads stay in this server session; they are not added to the repository. Do not upload personal data to a public deployment."),
    downloadButton("export", "Export filtered data")
  ),
  uiOutput("notice"),
  navset_card_tab(
    nav_panel("Overview",
      layout_columns(value_box("Complaints", textOutput("n")),
        value_box("Companies", textOutput("companies")),
        value_box("Resolved / all complaints", textOutput("resolved"))),
      plotOutput("status_plot"), DT::DTOutput("summary"),
      helpText("Evaluated resolution share uses only Resolved + Unresolved. Other statuses remain separate; counts are not market-share estimates.")),
    nav_panel("Timeline", plotOutput("timeline")),
    nav_panel("Text insights", p("Word frequency is descriptive; it is not sentiment or causal analysis."), plotOutput("words")),
    nav_panel("Data quality", DT::DTOutput("quality"),
      p("Exact duplicates are flagged, not silently removed. Unknown statuses are retained. Missing dates are excluded only from the timeline.")),
    nav_panel("Predictions", uiOutput("model_status"), actionButton("score", "Score current selection"),
      DT::DTOutput("scores"), downloadButton("export_scores", "Export predictions")),
    nav_panel("About",
      h3("Purpose"), p("Explore complaint patterns and, with a validated compatible model, estimate declared return intent after service."),
      p("Return intent is not observed repurchase. Final rating and resolution status are retrospective features, unavailable when a complaint first opens."),
      p("The bundled data are synthetic. No current performance claim is made from the 2021 report. Individual predictions are associations, not intervention recommendations."),
      a("Source and documentation", href = "https://github.com/eniltonsp/Machine-Learning-R"))
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    if (input$source == "demo") return(prepare_data(read_input("data/demo/complaints.csv")))
    req(input$file)
    tryCatch(prepare_data(read_input(input$file$datapath, input$encoding, input$delimiter, isTRUE(input$backslash))),
      error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  observeEvent(input$delimiter, {
    updateCheckboxInput(session, "backslash", value = identical(input$delimiter, "\t"))
  })
  observeEvent(dataset(), {
    choices <- sort(unique(dataset()$company))
    updateSelectInput(session, "company", choices = c("All", choices), selected = "All")
  })
  output$dates <- renderUI({
    d <- dataset()
    if ("date" %in% names(d) && any(!is.na(d$date))) {
      bounds <- range(d$date, na.rm = TRUE)
      dateRangeInput("dates", "Period", start = bounds[1], end = bounds[2], min = bounds[1], max = bounds[2])
    }
  })
  filtered <- reactive({
    d <- dataset()
    if (!is.null(input$company) && input$company != "All") d <- d[d$company == input$company, , drop = FALSE]
    if ("date" %in% names(d) && !is.null(input$dates) && length(input$dates) == 2 && !anyNA(input$dates)) {
      keep <- is.na(d$date) | (d$date >= input$dates[1] & d$date <= input$dates[2])
      d <- d[keep, , drop = FALSE]
    }
    d
  })
  output$notice <- renderUI({
    if (input$source == "demo") div(class = "alert alert-warning", "SYNTHETIC DEMONSTRATION — fictional companies and records. Not historical evidence or model validation.")
    else div(class = "alert alert-info", "Uploaded data — results describe your selected sample. Rows without dates remain in overview totals.")
  })
  output$n <- renderText(nrow(filtered()))
  output$companies <- renderText(length(unique(filtered()$company)))
  output$resolved <- renderText({
    d <- filtered()
    if (!nrow(d)) "—" else paste0(round(100 * mean(d$status == "Resolved"), 1), "%")
  })
  output$status_plot <- renderPlot({
    d <- filtered(); validate(need(nrow(d) > 0, "No records in this selection."))
    ggplot(d, aes(y = status)) + geom_bar(fill = "#147d92") + labs(x = "Complaints", y = NULL) + theme_minimal(base_size = 13)
  })
  output$summary <- DT::renderDT(company_summary(filtered()), rownames = FALSE, escape = TRUE)
  output$quality <- DT::renderDT(quality_report(dataset()), rownames = FALSE, escape = TRUE)
  output$timeline <- renderPlot({
    d <- filtered(); validate(need("date" %in% names(d), "Upload a date column to view the timeline."))
    d <- d[!is.na(d$date), , drop = FALSE]; validate(need(nrow(d) > 0, "No dated records."))
    d$month <- as.Date(format(d$date, "%Y-%m-01"))
    ggplot(d, aes(x = month, fill = status)) + geom_bar() + labs(x = NULL, y = "Complaints", fill = "Status") + theme_minimal(base_size = 13)
  })
  output$words <- renderPlot({
    d <- word_counts(filtered()); validate(need(nrow(d) > 0, "No complaint text available."))
    ggplot(d, aes(x = count, y = reorder(word, count))) + geom_col(fill = "#147d92") + labs(x = "Occurrences", y = NULL) + theme_minimal(base_size = 13)
  })
  bundle <- reactive({
    tryCatch(load_model_bundle(), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  })
  output$model_status <- renderUI({
    b <- bundle()
    if (is.null(b)) return(div(class = "alert alert-warning", "Prediction unavailable: no validated compatible model is installed. See scripts/train.R and docs/MODEL_CARD.md."))
    div(class = "alert alert-info", paste("Installed model:", b$algorithm, "| Trained:", b$trained_at,
      "| P(Yes) means declared return intent after service, not observed repurchase."))
  })
  scored <- reactiveVal(NULL)
  observeEvent(filtered(), { scored(NULL) })
  observeEvent(input$score, {
    b <- bundle()
    if (is.null(b)) { showNotification("No model installed.", type = "warning"); return() }
    if (input$source == "demo") { showNotification("Demo data are for exploration only; upload compatible data for scoring.", type = "warning"); return() }
    tryCatch({
      d <- filtered()
      if (!nrow(d)) stop("No rows selected.")
      scored(score_data(b, d))
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 10))
  })
  output$scores <- DT::renderDT({ req(scored()); scored() }, rownames = FALSE, escape = TRUE)
  output$export <- downloadHandler(filename = function() "complaints-filtered.csv", content = function(file) {
    readr::write_excel_csv(safe_export(filtered()), file)
  })
  output$export_scores <- downloadHandler(filename = function() "return-intent-predictions.csv", content = function(file) {
    req(scored()); readr::write_excel_csv(safe_export(scored()), file)
  })
}
shinyApp(ui, server)
