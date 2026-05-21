if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(httr2, jsonlite, tidyverse)

ZOTERO_API_KEY <- "PUT YOUR ZOTERO API KEY HERE"
ZOTERO_BASE <- "https://api.zotero.org/users/PUT YOUR USER ID HERE"
# ZOTERO_BASE <- "https://api.zotero.org/groups/OR PUT YOUR GROUP ID HERE"
MAILTO <- "your-email-for-better-rate-limits@uni-xyz.com"

# OA label map
oa_label <- function(status) {
  switch(status,
    "diamond" = "Diamond Open Access",
    "gold" = "Gold Open Access",
    "green" = "Green Open Access",
    "hybrid" = "Hybrid Open Access",
    "bronze" = "Bronze Open Access",
    "closed" = "Closed Access",
    NA_character_
  )
}

strip_doi_prefix <- function(doi) gsub("https://doi.org/", "", doi, fixed = TRUE)

resolve_oa_medium <- function(status, is_oa, pmcid = "") {
  has_pmcid <- nzchar(pmcid)
  raw <- oa_label(status)

  case_when(
    # Green OA: trustworthy with a PMCID; otherwise flag as unclassified
    raw == "Green Open Access" & !has_pmcid ~ "Open Access (unclassified)",
    # Closed/Bronze upgrade to Green with available PMCID
    raw %in% c("Closed Access", "Bronze Open Access") & has_pmcid ~ "Green Open Access",
    # Closed/Bronze with is_oa=TRUE but no PMCID — may be OA but unclear where
    raw %in% c("Closed Access", "Bronze Open Access") & isTRUE(is_oa) ~ "Open Access (unclassified)",
    # Others pass through as-is
    .default = raw
  )
}
# Zotero: fetch all items with pagination
get_zotero_items <- function() {
  limit <- 100

  first_resp <- request(ZOTERO_BASE) |>
    req_url_path_append("items") |>
    req_url_query(format = "json", limit = limit, start = 0) |>
    req_headers("Zotero-API-Key" = ZOTERO_API_KEY) |>
    req_perform()

  total <- as.integer(resp_header(first_resp, "Total-Results"))
  all_items <- vector("list", total)
  batch <- resp_body_json(first_resp, simplifyVector = FALSE)
  all_items[seq_along(batch)] <- batch

  starts <- if (total > limit) seq(limit, total - 1, by = limit) else integer(0)
  for (start in starts) {
    resp <- request(ZOTERO_BASE) |>
      req_url_path_append("items") |>
      req_url_query(format = "json", limit = limit, start = start) |>
      req_headers("Zotero-API-Key" = ZOTERO_API_KEY) |>
      req_perform()
    batch <- resp_body_json(resp, simplifyVector = FALSE)
    all_items[start + seq_along(batch)] <- batch
  }

  all_items
}
# OpenAlex: fetch work metadata by DOI
get_openalex_metadata <- function(doi, pmid = "") {
  doi <- strip_doi_prefix(doi)

  # Primary: DOI lookup
  resp <- tryCatch(
    request(paste0("https://api.openalex.org/works/https://doi.org/", doi)) |>
      req_url_query(mailto = MAILTO) |>
      req_perform(),
    error = function(e) NULL
  )

  if (!is.null(resp) && resp_status(resp) < 400) {
    body <- resp_body_json(resp)
    returned_doi <- strip_doi_prefix(body$doi %||% "")
    if (tolower(returned_doi) == tolower(doi)) {
      return(body)
    }
    message("  DOI mismatch in OA response, trying PMID fallback...")
  }

  # Fallback: PMID lookup
  if (nzchar(pmid)) {
    resp2 <- tryCatch(
      request(paste0("https://api.openalex.org/works/pmid:", pmid)) |>
        req_url_query(mailto = MAILTO) |>
        req_perform(),
      error = function(e) NULL
    )
    if (!is.null(resp2) && resp_status(resp2) < 400) {
      return(resp_body_json(resp2))
    }
  }

  NULL
}

# Semantic Scholar: fetch citation metrics by DOI
get_s2_metadata <- function(doi) {
  doi <- strip_doi_prefix(doi)
  resp <- tryCatch(
    request(paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi)) |>
      req_url_query(fields = "citationCount,influentialCitationCount,publicationDate") |>
      req_perform(),
    error = function(e) NULL
  )
  if (!is.null(resp)) resp_body_json(resp)
}
# Build Extra field content
build_extra <- function(oa_data, s2_data, pmcid = "") {
  parts <- list()

  if (!is.null(oa_data)) {
    medium <- resolve_oa_medium(
      status = oa_data$open_access$oa_status,
      is_oa  = oa_data$open_access$is_oa,
      pmcid  = pmcid
    )

    oa_types <- c(
      "Gold Open Access", "Green Open Access",
      "Hybrid Open Access", "Diamond Open Access"
    )

    topics <- oa_data$topics

    parts$medium <- paste0("medium: ", medium)
    parts$annote <- if (medium %in% oa_types) "annote: OA"
    parts$topics <- if (length(topics) > 0) {
      paste0("topics: ", paste(map_chr(topics[seq_len(min(3, length(topics)))], "display_name"), collapse = "; "))
    }
    parts$oalex_id <- if (!is.null(oa_data$id)) paste0("openalex-id: ", oa_data$id)
  }

  if (!is.null(s2_data)) {
    parts$citations <- if (!is.null(s2_data$citationCount)) {
      paste0("citation-count: ", s2_data$citationCount)
    }
    parts$influential <- if (!is.null(s2_data$influentialCitationCount)) {
      paste0("influential-citations: ", s2_data$influentialCitationCount)
    }
  }

  paste(Filter(Negate(is.null), parts), collapse = "\n")
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

# Helper: process and update a single item
process_item <- function(item) {
  doi <- item$data$DOI
  key <- item$data$key
  version <- item$version
  pmcid <- item$data$PMCID %||% ""
  pmid <- item$data$PMID %||% ""

  message("\nProcessing ", key, " — ", doi)

  oa_data <- get_openalex_metadata(doi, pmid)
  s2_data <- get_s2_metadata(doi)

  if (is.null(oa_data) && is.null(s2_data)) {
    message("  Skipping — no data returned from either API")
    return(invisible(NULL))
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
  Sys.sleep(0.2)
}

# Main
message("Fetching items from Zotero...")
items <- get_zotero_items()
message("Total items fetched: ", length(items))

items |>
  keep(\(item) !is.null(item$data$DOI) && nzchar(item$data$DOI)) |>
  walk(process_item)
