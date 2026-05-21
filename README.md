# zotero-metadata-enrichment

An R script that automatically enriches your Zotero library with open access
status, citation metrics, and topic classifications by querying the
[OpenAlex](https://openalex.org) and [Semantic Scholar](https://www.semanticscholar.org)
APIs. Results are written directly to each item's `Extra` field in a structured
`key: value` format, making them accessible to custom CSL citation styles.

## What It Does

For every item in your Zotero library or group that has a DOI, the script fetches
and writes the following metadata to the `Extra` field:

| Key | Source | Example |
|---|---|---|
| `medium` | OpenAlex / PubMed Central | `Gold Open Access` |
| `topics` | OpenAlex | `Parkinson's disease; PINK1; Mitophagy` |
| `openalex-id` | OpenAlex | `https://openalex.org/W12345678` |
| `citation-count` | Semantic Scholar | `312` |
| `influential-citations` | Semantic Scholar | `18` |

Any pre-existing content in the `Extra` field is **overwritten** on each run,
ensuring citation counts and OA status stay up to date.

## Open Access Classification

OA status is determined using the following priority order:

1. **OpenAlex `oa_status`** - diamond, gold, and hybrid are used directly
2. **PubMed Central fallback** - if OpenAlex returns `closed`,`green`, or `bronze` and a PMCID is
   stored in Zotero, the article is reclassified as `Green Open Access`
3. **Inconsistencies and no PMCID** - In case of inconsistencies or a missing PMCID, entries are labelled defensively as `Open Access (unclassified)` and won't qualify as OA by default.
4. **`Closed Access`** - written when no free version is detected via OpenAlex & PubMed

> **Note:** OA classification, particularly for `bronze` and `closed`, is not
> always accurate. Bronze articles are free to read without a formal license and
> may become paywalled without notice, which is the reason for this strict classification.
> See the [OpenAlex documentation](https://developers.openalex.org/) for details
> on their methodology.

## Requirements

- R ≥ 4.4.0
- [`pacman`](https://cran.r-project.org/package=pacman) for dependency management

Generate a Zotero API key on the Zotero website. Add it and your user or group id to the respective constants in the script.
