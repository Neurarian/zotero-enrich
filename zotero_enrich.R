if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(httr2, jsonlite)

ZOTERO_API_KEY <- "PUT YOUR ZOTERO API KEY HERE"
ZOTERO_BASE <- "https://api.zotero.org/users/PUT YOUR USER ID HERE"
# ZOTERO_BASE <- "https://api.zotero.org/groups/OR PUT YOUR GROUP ID HERE"
MAILTO <- "your-email-for-better-rate-limits@uni-xyz.com"

# OA label map
oa_label <- function(status) {
  switch(status,
    "gold"   = "Gold Open Access",
    "green"  = "Green Open Access",
    "hybrid" = "Hybrid Open Access",
    "bronze" = "Bronze Open Access",
    "closed" = "Closed Access",
    NA_character_
  )
}

# Zotero: fetch all items with pagination
get_zotero_items <- function() {
  all_items <- list()
  start <- 0
  limit <- 100

  repeat {
    resp <- request(ZOTERO_BASE) |>
      req_url_path_append("items") |>
      req_url_query(format = "json", limit = limit, start = start) |>
      req_headers("Zotero-API-Key" = ZOTERO_API_KEY) |>
      req_perform()

    batch <- resp_body_json(resp, simplifyVector = FALSE)
    if (length(batch) == 0) break

    all_items <- c(all_items, batch)
    total <- as.integer(resp_header(resp, "Total-Results"))
    start <- start + limit
    if (start >= total) break
  }
  all_items
}

# OpenAlex: fetch work metadata by DOI
get_openalex_metadata <- function(doi) {
  doi <- gsub("https://doi.org/", "", doi, fixed = TRUE)
  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
  resp <- tryCatch(
    request(url) |>
      req_url_query(mailto = MAILTO) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(NULL)
  }
  resp_body_json(resp)
}

# Semantic Scholar: fetch citation metrics by DOI
get_s2_metadata <- function(doi) {
  doi <- gsub("https://doi.org/", "", doi, fixed = TRUE)
  url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi)
  resp <- tryCatch(
    request(url) |>
      req_url_query(
        fields = "citationCount,influentialCitationCount,publicationDate"
      ) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(NULL)
  }
  resp_body_json(resp)
}

# Build Extra field content
build_extra <- function(oa_data, s2_data, pmcid) {
  lines <- character(0)

  # OA status
  if (!is.null(oa_data)) {
    status <- oa_data$open_access$oa_status
    is_oa <- oa_data$open_access$is_oa

    medium <- oa_label(status)

    if (identical(medium, "Closed Access")) {
      if (!is.null(pmcid) && nzchar(pmcid)) {
        medium <- "Green Open Access"
      } else if (!is.null(is_oa) && isTRUE(is_oa)) {
        medium <- "Open Access (unclassified)"
      }
    }

    lines <- c(lines, paste0("medium: ", medium))

    # Topics (up to 3)
    topics <- oa_data$topics
    if (!is.null(topics) && length(topics) > 0) {
      topic_names <- sapply(
        topics[seq_len(min(3, length(topics)))],
        function(t) t$display_name
      )
      lines <- c(lines, paste0("topics: ", paste(topic_names, collapse = "; ")))
    }

    # OpenAlex ID
    oalex_id <- oa_data$id
    if (!is.null(oalex_id)) {
      lines <- c(lines, paste0("openalex-id: ", oalex_id))
    }
  }

  # Citation counts from Semantic Scholar
  if (!is.null(s2_data)) {
    cit <- s2_data$citationCount
    inf <- s2_data$influentialCitationCount
    if (!is.null(cit)) lines <- c(lines, paste0("citation-count: ", cit))
    if (!is.null(inf)) lines <- c(lines, paste0("influential-citations: ", inf))
  }

  paste(lines, collapse = "\n")
}

# Zotero: write Extra field via PATCH
update_extra_field <- function(item_key, item_version, new_extra) {
  body <- toJSON(list(extra = new_extra), auto_unbox = TRUE)

  request(ZOTERO_BASE) |>
    req_url_path_append("items", item_key) |>
    req_headers(
      "Zotero-API-Key"              = ZOTERO_API_KEY,
      "Content-Type"                = "application/json",
      "If-Unmodified-Since-Version" = as.character(item_version)
    ) |>
    req_body_raw(body) |>
    req_method("PATCH") |>
    req_perform()
}

# Main
message("Fetching items from Zotero...")
items <- get_zotero_items()
message("Total items fetched: ", length(items))

for (item in items) {
  doi <- item$data$DOI
  if (is.null(doi) || !nzchar(doi)) next

  key <- item$data$key
  version <- item$version
  pmcid <- item$data$PMCID %||% ""

  message("\nProcessing ", key, " — ", doi)

  # Fetch from both APIs
  oa_data <- get_openalex_metadata(doi)
  s2_data <- get_s2_metadata(doi)

  if (is.null(oa_data) && is.null(s2_data)) {
    message("  Skipping — no data returned from either API")
    next
  }

  new_extra <- build_extra(oa_data, s2_data, pmcid)
  message("  Writing:\n", paste0("    ", strsplit(new_extra, "\n")[[1]],
    collapse = "\n"
  ))

  result <- tryCatch(
    update_extra_field(key, version, new_extra),
    error = function(e) {
      message("  ERROR: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(result)) message("  OK (HTTP ", resp_status(result), ")")
  Sys.sleep(0.2) # API rate limits
}
