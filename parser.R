#more efficient code?
library(tidyverse)
library(rentrez)
library(xml2)
library(fs)

#api key for pubmed, stolen from something else
set_entrez_key("609b78b66eec9191564222514a6b2cad4108")

#set limits on the batch sizes, because chunks bigger than this were causing major issues when the actual function to pull information was run. Anyuthing larger than 200 calls was resulting in a lot of errors and NAs to be returned, so this seems to be as big as i can go without crashing the whole thing. results_dir saves the results in a specific folder in the working directly titled "pubmed_results" which is insanely helpful. 
chunk_size <- 10
batch_size <- 200
results_dir <- "pubmed_results"

#all of the possible mesh terms that i could figure out could potentially be relevant. the only thing i didn't include was related to tobacco use because the stigma isn't the same ~vibe~ as the stigma faced by folks who use alcohol or illicit/prescription substances. some of these are sub-subheadings which is maybe slowing the code down a little bit but i'd rather it be slower and grab everything instead of being efficient. "unique()" is helping to manage what pubmed sees as duplicates (which i think are occurring when a mesh term "appears" more than once because a sub-subheading appears alongside its parent category)
mesh_terms <- c(
  "Substance-Related Disorders",
  "Alcoholism",
  "Alcohol-Related Disorders",
  "Alcohol-Induced Disorders",
  "Cocaine-Related Disorders",
  "Cocaine Addiction",
  "Drug Misuse",
  "Drug Users",
  "Drug Addicts",
  "Inhalant Abuse",
  "Marijuana Abuse",
  "Narcotic-Related Disorders",
  "Narcotic Addiction",
  "Neonatal Abstinence Syndrome",
  "Passive Addiction, Neonatal",
  "Phencyclidine Abuse",
  "Psychoses, Substance-Induced",
  "Substance Abuse, Intravenous",
  "Substance Abuse, Oral",
  "Substance Withdrawal Syndrome",
  "Opioid-Related Disorders",
  "Behavior, Addictive",
  "Addictive Behavior",
  "Heroin Dependence",
  "Morphine Dependence",
  "Opiate Overdose",
  "Addiction, Opioid",
  "Opiate Addiction",
  "Opioid Addiction",
  "Opium Dependence",
  "Opium Addiction",
  "Amphetamine-Related Disorders",
  "Amphetamine Addiction",
  "Addiction Medicine",
  "Addiction Psychiatry",
  "Prescription Drug Misuse",
  "Drug Overdose",
  "Prescription Drug Overuse",
  "Alcohol Withdrawal Delirium",
  "Drug-Seeking Behavior",
  "Recreational Drug Use",
  "Drug Abusers",
  "IV Drug Users",
  "Intravenous Drug Users",
  "People Who Inject Drugs"
) |>
  unique()

#makes sure that everything is formatted correctly for pubmed api searches and attaches the correct tag to the end of it to make sure that the search is searching in the correct category of information. also essentially makes the search function work by joining all of the terms with "OR" in the search function
mesh_query <- str_c('"', mesh_terms, '"[MeSH]', collapse = " OR ")

#next order of business: getting the id-ed journals from the Q1 category on clarivate identified and loaded in. The journals coming from clarivate use canonical names (as far as i can tell?) so i'm pulling the journal titles (the file includes their Q categorization and their associated displine), making sure that extra spaces/whitespace aren't included (str_trim) and deleting duplicates (most of which i removed by hand, because several journals are cross-listed in several disciplinary categories). 
all_journals <- read_csv("all_journals.csv") |>
  pull(journal_title) |>
  str_trim() |>
  unique()

#create the search function by vector-izing the journal names and joining them by OR. 
search_chunk <- function(journals) {
  journal_query <- str_c('"', journals, '"[Journal]', collapse = " OR ")
#then make a matrix table that creates every unique combination of journal and mesh terms possible and prepares them for search in pubmed
  query <- str_c("(", mesh_query, ") AND (", journal_query, ")")
#sets up the parameters of the search--we're looking in pubmed, we're searching each journal for the appearance of the mesh terms of interest (query made above), we're first returning just whether or not there were matches, and storing the results on the pubmed servers to retrieve later. supposedly going to help this run a little smoother and more efficiently (my poor macbook can't handle much)
  entrez_search(db = "pubmed", term = query, retmax = 0, use_history = TRUE)
}

#a necessary addition because one failure in the retrieval was causing the whole thing to stop and crash. search_res establishes the parameters of the search, where the search should start, how many, that we're searching on pubmed, where to look for previous hits to return that were found during the preliminary entrez_search function stored by pubmed, returning plain text to then be parsed
fetch_batch <- function(search_res, start, batch_size = 200) {
  entrez_fetch(
    db = "pubmed",
    web_history = search_res$web_history,
    rettype = "xml",
    retstart = start,
    retmax = batch_size,
    parsed = FALSE
  ) |>
    read_xml() |>
#searches for and id's the element nodes id'd by pubmed so that we can be sure to grab abstracts, titles, etc in the parser
    xml_find_all("//PubmedArticle")
}

#failsafe i had to add in to make sure things wouldn't crash with one failure. purrr magic, automatically retries a search if an error gets thrown with increasing wait time between each search (usually for network issues or temporary bugs on pubmed's side).tries it 3 times before it gives up and returns an NA. because it's associated with fetch_batch function, it applies in subsequent search functions that involve fetch_batch
fetch_batch <- insistently(
  fetch_batch,
  rate = rate_backoff(pause_base = 2, max_times = 3)
)

#NEXT STEP: identifies pmid, journal, year of publication --things that appear once per article that could be used to identify a specific article. this function returns one entry per article with identifying information 
node_unique <- function(nodes, xpath) {
  nodes |>
    xml_find_first(xpath) |>
    xml_text()
}

#with unique article identifiers established, searches the other nodes for for information tagged as relevant to the abstract. captures the specific cases where the abstract might be formatted differently (like if it has defined "methods" or "results" sections within the abstract itself). Captures any text associated with the abstract and returns it as text. collapsed into one string for ease of analysis later. if no nodes exist related to the abstracts, then an NA is returned
node_text_all <- function(node, xpath, collapse = " ") {
  node |>
    xml_find_all(xpath) |>
    xml_text() |>
    str_flatten(collapse)
}
#takes both of the node functions (the unique IDs and the abstract text) and searches each unique article for matching mesh terms. each node is searched for mesh information, searching within the mesh node for "descriptor name" which appears around each potential mesh term of interest as <DescriptorName>Alcoholism</DescriptorName> in the returned XML.
parse_batch <- function(nodes) {
  all_mesh <- map(nodes, \(x) {
    x |>
      xml_find_all(".//MeshHeading/DescriptorName") |>
      xml_text()
  })
  
#NEXT STEP: creates a tibble of returned information from ALLL of the parsing functions outlined above. pmid, journal, year of publication, abstract text, all mesh terms, all matching mesh terms all become columns, and NAs are returned if any of these nodes are empty.
  tibble(
    pmid = node_unique(nodes, ".//MedlineCitation/PMID"),
    journal = node_unique(nodes, ".//Journal/Title"),
    year = coalesce(
    #dates are tricky, i've learned. if a clean pubdate is provided, that's grabbed first. if not, the first four numerical digits are grabbed from the <MedlineDate> node
        node_unique(nodes, ".//JournalIssue/PubDate/Year"),
      node_unique(nodes, ".//JournalIssue/PubDate/MedlineDate") |>
        str_extract("\\d{4}")
    ),
    abstract = map_chr(nodes, node_text_all, ".//Abstract/AbstractText") |>
      na_if(""),
    all_mesh_terms = map_chr(all_mesh, \(m) str_flatten(m, "; ")) |>
      na_if(""),
    matched_mesh_terms = map_chr(
      all_mesh,
      \(m) str_flatten(intersect(m, mesh_terms), "; ")
    ) |>
      na_if("")
  ) |>
    mutate(has_matching_mesh = !is.na(matched_mesh_terms))
}

#NEXT: process each of the batches. first create a folder within the working directory and references it when running the results so that batches don't get rerun. 
dir_create(results_dir)

#each chunk of 200 is assigned a batch number and is saved one at a time. kinda nifty, i wish i had known that you could automate the batch naming earlier! if there are no results for a particular batch, a file isn't written out and the chunk is essentially skipped. function won't rerun if a file for a particular chunk already exists in the folder, so it checks the folder first to see where it needs to start from.
process_chunk <- function(journals, id) {
  file <- path(results_dir, str_c("chunk_", str_pad(id, 3, pad = "0"), ".rds"))
  if (file_exists(file)) {
    return(invisible(NULL))
  }
  
#res is the combination mesh-and-journal table query built a couple lines previously
  res <- search_chunk(journals)
 
#calculates the number of hits, and skips a chunk if there are no matching results
   n_hits <- as.numeric(res$count)
  if (n_hits == 0) {
    return(invisible(NULL))
  }
  
#establishes how the chunks should be divvied up (i still don't fully understand the n_hits - 1 but it breaks without it). map iterates the pulling, parsing, and tibble-izing of returned results. they get written out in batches
  out <- seq(0, n_hits - 1, by = batch_size) |>
    map(\(start) fetch_batch(res, start, batch_size) |> parse_batch()) |>
    list_rbind()
  
  write_rds(out, file)
}

#RUN IT. chunks are created and named according to the order of the chunks that were created (the first 10 appear as chunk 1, etc). I figured out why 178 is necessary. the way that i'm creating the batches above puts all of the different queries into one long string that would overwhelm pubmed if sent at once. this effectively breaks up the query, dividing each chunk into batches of 10 journals and assigns them a group number so that the progress of the overall query can be traced.
journal_chunks <- all_journals |>
  split(ceiling(seq_along(all_journals) / chunk_size))
#implementation of the safe iteration that doesn't break the whole thing if an error gets thrown for a pubmed issue. either returns the result to returns an error
safe_process_chunk <- safely(process_chunk)
#the time consuming part, doing the searches by chunk. assigns them a name/number as it processes
results_log <- journal_chunks |>
  imap(\(journals, id) safe_process_chunk(journals, as.integer(id)))
#finds anything that didn't run properly 
failed <- results_log |>
  keep(\(x) !is.null(x$error))
#if anything failed, this reports exactly which chunks failed that actually had content that should have been returned. after the rest of the processing has occurred, this code specifically reruns anything that returned as an error (as opposed to an NA) and saves it as its own chunk in the folder
if (length(failed) > 0) {
  message("Failed chunks: ", str_c(names(failed), collapse = ", "))
  
  failed |>
    imap(\(x, id) {
      tibble(chunk = id, error_message = conditionMessage(x$error))
    }) |>
    list_rbind() |>
    write_csv(path(results_dir, "failed_chunks_log.csv"))
}

#combines all of the returned chunks into one file. reads the folder containing all of the returned chunk files formatted as "chunk_#.rds" everything is loaded back as the tibbles that were generated during the search, preserving column names, and stacks the rows with rbind (since they all share exactly the same column names). distinct keeps only the unique results. 
all_results <- dir_ls(results_dir, regexp = "chunk_\\d+\\.rds$") |>
  map(read_rds) |>
  list_rbind() |>
  distinct(pmid, .keep_all = TRUE)

#write out this massive fucking file!!
write_csv(all_results, "all_results.csv")

#pull out NAs and rerun. decided to rerun part of the parser after pulling out the specific pmids that returned NA abstracts to rerun.
all_results <- read.csv("all_results.csv")

nas <- all_results |> 
  filter(is.na(abstract)) |>
  pull(pmid)
 
#redefining a function to search by pmid for this particular case
fetch_by_pmid <- function(nas) {
  entrez_fetch(db = "pubmed", id = pmid, rettype = "xml", parsed = FALSE) |>
    read_xml() |>
    xml_find_all("//PubmedArticle")
}
fetch_by_pmid <- insistently(
  fetch_by_pmid,
  rate = rate_backoff(pause_base = 2, max_times = 3)
)

pmid_batches <- split(nas, ceiling(seq_along(nas) / 200))

safe_retry <- safely(\(x) fetch_by_pmid(x) |> 
                       parse_batch())

retry_log <- imap(pmid_batches, \(b, id) {
  Sys.sleep(0.4)
  message("Retry batch ", id, " of ", length(pmid_batches))
  safe_retry(b)
})

retry_log |> keep(\(x) !is.null(x$error)) |> names()

retry <- retry_log |>
  map("result") |>
  compact() |>
  list_rbind()
#okay so all of these abstracts are true NAs. i spot checked 15-20 and they all had no abstract. i'll be damned. 
