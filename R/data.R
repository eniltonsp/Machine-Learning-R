# Input contracts and transformations. No global workspace or network dependencies.
normalise_names <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

aliases <- c(data = "date", empresa = "company", idempresa = "company_id",
  localidade = "city", localizacao = "city", reclamacao = "text",
  titulo = "title", estado = "state", segmento = "segment",
  empresa_ramo = "segment", qtd_dias_resposta = "days_response",
  qtd_dias_conclusao = "days_resolution", nota_cliente = "rating",
  voltaria_a_fazer_negocio = "return_intent", reclamacao_id = "complaint_id")

read_input <- function(path, encoding = "UTF-8", delim = ",", escape_backslash = identical(delim, "\t")) {
  if (!file.exists(path)) stop("File not found.", call. = FALSE)
  if (!delim %in% c(",", ";", "\t")) stop("Unsupported delimiter.", call. = FALSE)
  if (file.info(path)$size > 25 * 1024^2) stop("Maximum file size: 25 MB.", call. = FALSE)
  raw <- readr::read_delim(path, delim = delim,
    escape_backslash = escape_backslash,
    locale = readr::locale(encoding = encoding), col_types = readr::cols(.default = "c"),
    trim_ws = TRUE, name_repair = "minimal", show_col_types = FALSE, progress = FALSE)
  if (nrow(readr::problems(raw))) stop("Malformed file: inconsistent columns or quotes.", call. = FALSE)
  names(raw) <- normalise_names(names(raw))
  mapped <- match(names(raw), names(aliases))
  names(raw)[!is.na(mapped)] <- unname(aliases[mapped[!is.na(mapped)]])
  if (anyDuplicated(names(raw)) || any(!nzchar(names(raw))))
    stop("Duplicate or empty column names after normalisation.", call. = FALSE)
  d <- as.data.frame(raw, stringsAsFactors = FALSE)
  for (nm in names(d)) d[[nm]][trimws(d[[nm]]) == ""] <- NA_character_
  d
}

numeric_column <- function(x) {
  suppressWarnings(as.numeric(sub(",", ".", trimws(x), fixed = TRUE)))
}

prepare_data <- function(d) {
  missing <- setdiff(c("company", "status"), names(d))
  if (length(missing)) stop(paste("Missing columns:", paste(missing, collapse = ", ")), call. = FALSE)
  if (!nrow(d)) stop("The dataset is empty.", call. = FALSE)
  if (nrow(d) > 100000) stop("Maximum supported rows: 100,000.", call. = FALSE)
  if (anyNA(d$company) || anyNA(d$status)) stop("Company and status cannot be empty.", call. = FALSE)
  status_map <- c("Resolvido" = "Resolved", "Não Resolvido" = "Unresolved",
    "Respondida" = "Answered", "Não respondida" = "Unanswered", "Em réplica" = "In discussion")
  status_match <- match(d$status, names(status_map))
  d$status[!is.na(status_match)] <- unname(status_map[status_match[!is.na(status_match)]])
  if ("date" %in% names(d)) {
    original <- d$date
    d$date <- as.Date(original, format = "%d/%m/%Y")
    iso <- !is.na(original) & grepl("^\\d{4}-\\d{2}-\\d{2}$", original)
    d$date[iso] <- as.Date(original[iso], format = "%Y-%m-%d")
    if (any(!is.na(original) & is.na(d$date))) stop("Invalid dates; use DD/MM/YYYY or YYYY-MM-DD.", call. = FALSE)
  }
  for (nm in intersect(c("rating", "days_response", "days_resolution"), names(d))) {
    old <- d[[nm]]
    d[[nm]] <- numeric_column(old)
    if (any(!is.na(old) & (is.na(d[[nm]]) | !is.finite(d[[nm]]))))
      stop(paste("Invalid numeric values:", nm), call. = FALSE)
    if (any(d[[nm]] < 0, na.rm = TRUE)) stop(paste("Negative values:", nm), call. = FALSE)
  }
  if ("rating" %in% names(d) && any(d$rating > 10, na.rm = TRUE))
    stop("Rating must be between 0 and 10.", call. = FALSE)
  if ("return_intent" %in% names(d)) {
    x <- normalise_names(d$return_intent)
    if (any(!is.na(x) & !x %in% c("sim", "nao", "yes", "no")))
      stop("Return intent must be Sim/Não or Yes/No.", call. = FALSE)
    d$return_intent <- ifelse(is.na(x), NA_character_, ifelse(x %in% c("sim", "yes"), "Yes", "No"))
  }
  rownames(d) <- NULL
  d
}

quality_report <- function(d) {
  known <- c("Resolved", "Unresolved", "Answered", "Unanswered", "In discussion")
  data.frame(check = c("Rows", "Exact duplicate rows", "Missing cells", "Unknown status rows"),
    value = c(nrow(d), sum(duplicated(d)), sum(is.na(d)), sum(!d$status %in% known)))
}

company_summary <- function(d) {
  groups <- split(seq_len(nrow(d)), d$company)
  if (!length(groups)) return(data.frame(company = character(), complaints = integer(),
    resolved = integer(), evaluated = integer(), resolved_share_all = numeric(), resolved_share_evaluated = numeric()))
  do.call(rbind, lapply(names(groups), function(nm) {
    s <- d$status[groups[[nm]]]
    evaluated <- sum(s %in% c("Resolved", "Unresolved"))
    resolved <- sum(s == "Resolved")
    data.frame(company = nm, complaints = length(s), resolved = resolved,
      evaluated = evaluated, resolved_share_all = resolved / length(s),
      resolved_share_evaluated = if (evaluated) resolved / evaluated else NA_real_)
  }))
}

word_counts <- function(d, limit = 30L) {
  if (!"text" %in% names(d)) return(data.frame(word = character(), count = integer()))
  text <- d$text[!is.na(d$text)]
  words <- unlist(strsplit(tolower(paste(text, collapse = " ")), "[^[:alpha:]]+"))
  stopwords <- c("para", "como", "mais", "pela", "pelo", "esse", "essa", "isso", "uma", "com",
    "que", "não", "nao", "por", "dos", "das", "foi", "meu", "minha", "tenho", "estou",
    "the", "and", "for", "was", "with", "this", "that")
  counts <- sort(table(words[nchar(words) >= 3 & !words %in% stopwords]), decreasing = TRUE)
  counts <- head(counts, limit)
  data.frame(word = names(counts), count = as.integer(counts))
}

# Prefix strings that spreadsheet software may interpret as formulas.
safe_export <- function(d) {
  for (nm in names(d)) if (is.character(d[[nm]])) {
    flag <- !is.na(d[[nm]]) & grepl("^[[:space:]]*[=+@-]", d[[nm]])
    d[[nm]][flag] <- paste0("'", d[[nm]][flag])
  }
  d
}
