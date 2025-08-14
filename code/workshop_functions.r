# Functions for analysing data

# Function to get authors from specific communities in a time slice
get_authors_from_communities <- function(communities, time_slice = "all_time_communities") {
  
  authors_in_communities <- network_data %>%
    filter(node_type == "author") %>%
    filter(.data[[time_slice]] %in% communities) %>%
    pull(node_id)
  
  return(authors_in_communities)
}

# Function to get community membership for specific authors
get_author_communities <- function(authors, time_slice = "all_time_communities") {
  
  author_communities <- network_data %>%
    filter(node_type == "author", node_id %in% authors) %>%
    select(node_id, community = !!sym(time_slice)) %>%
    filter(!is.na(community))
  
  return(author_communities)
}

# # Function to determine best time slice for a given year
# get_best_time_slice <- function(year) {
#   if (is.na(year)) return("all_time_communities")
#   
#   time_slices <- list(
#     "slice_1650_1699" = c(1650, 1699),
#     "slice_1675_1724" = c(1675, 1724),
#     "slice_1700_1749" = c(1700, 1749),
#     "slice_1725_1774" = c(1725, 1774),
#     "slice_1750_1799" = c(1750, 1799),
#     "slice_1775_1824" = c(1775, 1824),
#     "slice_1800_1849" = c(1800, 1849),
#     "slice_1825_1874" = c(1825, 1874),
#     "slice_1850_1899" = c(1850, 1899),
#     "slice_1875_1924" = c(1875, 1924)
#   )
#   
#   for (slice_name in names(time_slices)) {
#     slice_range <- time_slices[[slice_name]]
#     if (year >= slice_range[1] && year <= slice_range[2]) {
#       return(slice_name)
#     }
#   }
#   
#   return("all_time_communities")
# }

# Function to subset corpus based on community membership
subset_corpus_by_communities <- function(corpus, communities, time_slice = "all_time_communities", 
                                         match_any_author = TRUE) {
  
  # Get authors from these communities
  target_authors <- get_authors_from_communities(communities, time_slice)
  
  if (length(target_authors) == 0) {
    warning("No authors found in specified communities for time slice: ", time_slice)
    return(corpus_subset(corpus, rep(FALSE, ndoc(corpus))))
  }
  
  # Find documents with these authors
  if (match_any_author) {
    # Document matches if ANY author is in target communities
    subset_docs <- map_lgl(docvars(corpus, "authors_list"), function(doc_authors) {
      any(doc_authors %in% target_authors)
    })
  } else {
    # Document matches if ALL authors are in target communities
    subset_docs <- map_lgl(docvars(corpus, "authors_list"), function(doc_authors) {
      all(doc_authors %in% target_authors)
    })
  }
  
  return(corpus_subset(corpus, subset_docs))
}

# Function to subset corpus by date range
subset_by_date <- function(corpus, start_year = NULL, end_year = NULL, 
                           decade = NULL, century = NULL) {
  
  if (!is.null(decade)) {
    # If decade specified (e.g., 1900 for 1900s)
    start_year <- decade
    end_year <- decade + 9
  }
  
  if (!is.null(century)) {
    # If century specified (e.g., 19 for 1900s)
    start_year <- century * 100
    end_year <- (century * 100) + 99
  }
  
  if (!is.null(start_year) || !is.null(end_year)) {
    if (is.null(start_year)) start_year <- min(docvars(corpus, "year"), na.rm = TRUE)
    if (is.null(end_year)) end_year <- max(docvars(corpus, "year"), na.rm = TRUE)
    
    subset_docs <- docvars(corpus, "year") >= start_year & 
      docvars(corpus, "year") <= end_year & 
      !is.na(docvars(corpus, "year"))
    
    return(corpus_subset(corpus, subset_docs))
  }
  
  return(corpus)
}

# Function to subset corpus by document type
subset_by_type <- function(corpus, doc_types) {
  if (!is.null(doc_types) && length(doc_types) > 0) {
    subset_docs <- docvars(corpus, "type") %in% doc_types
    return(corpus_subset(corpus, subset_docs))
  }
  return(corpus)
}

# Function to subset corpus by primary topic
subset_by_topic <- function(corpus, topics, topic_threshold = NULL) {
  if (!is.null(topics) && length(topics) > 0) {
    subset_docs <- docvars(corpus, "primaryTopic") %in% topics
    
    # Optional: filter by topic percentage threshold
    if (!is.null(topic_threshold)) {
      subset_docs <- subset_docs & 
        docvars(corpus, "primaryTopicPercentage") >= topic_threshold
    }
    
    return(corpus_subset(corpus, subset_docs))
  }
  return(corpus)
}

# Function to subset corpus by journal
subset_by_journal <- function(corpus, journals) {
  if (!is.null(journals) && length(journals) > 0) {
    subset_docs <- docvars(corpus, "jrnl") %in% journals
    return(corpus_subset(corpus, subset_docs))
  }
  return(corpus)
}

# Function to subset corpus by authors
subset_by_authors <- function(corpus, authors, match_type = "any") {
  if (!is.null(authors) && length(authors) > 0) {
    if (match_type == "any") {
      # Documents where any of the specified authors appear
      subset_docs <- map_lgl(docvars(corpus, "authors_list"), function(doc_authors) {
        any(authors %in% doc_authors)
      })
    } else if (match_type == "all") {
      # Documents where all specified authors appear
      subset_docs <- map_lgl(docvars(corpus, "authors_list"), function(doc_authors) {
        all(authors %in% doc_authors)
      })
    } else if (match_type == "first") {
      # Documents where the first author is one of the specified authors
      subset_docs <- docvars(corpus, "first_author") %in% authors
    }
    
    return(corpus_subset(corpus, subset_docs))
  }
  return(corpus)
}

# Function to subset corpus by author count
subset_by_author_count <- function(corpus, min_authors = NULL, max_authors = NULL, 
                                   single_author = NULL, multi_author = NULL) {
  subset_docs <- rep(TRUE, ndoc(corpus))
  
  if (!is.null(min_authors)) {
    subset_docs <- subset_docs & docvars(corpus, "author_count") >= min_authors
  }
  
  if (!is.null(max_authors)) {
    subset_docs <- subset_docs & docvars(corpus, "author_count") <= max_authors
  }
  
  if (!is.null(single_author) && single_author) {
    subset_docs <- subset_docs & docvars(corpus, "author_count") == 1
  }
  
  if (!is.null(multi_author) && multi_author) {
    subset_docs <- subset_docs & docvars(corpus, "author_count") > 1
  }
  
  return(corpus_subset(corpus, subset_docs))
}

# Function to subset corpus by community
subset_by_community <- function(corpus, communities, time_slice = "all_time_communities", 
                                match_any_author = TRUE) {
  return(subset_corpus_by_communities(corpus, communities, time_slice, match_any_author))
}

# General subsetting function that combines all criteria
subset_corpus <- function(corpus, 
                          start_year = NULL, end_year = NULL, 
                          decade = NULL, century = NULL,
                          doc_types = NULL, 
                          topics = NULL, topic_threshold = NULL,
                          journals = NULL,
                          authors = NULL, author_match_type = "any",
                          min_authors = NULL, max_authors = NULL,
                          single_author = NULL, multi_author = NULL,
                          communities = NULL, time_slice = "all_time_communities",
                          match_any_author = TRUE,
                          debug = FALSE) {
  
  result <- corpus
  if (debug) cat("Starting with", ndoc(result), "documents\n")
  
  result <- subset_by_date(result, start_year, end_year, decade, century)
  if (debug) cat("After date filtering:", ndoc(result), "documents\n")
  
  result <- subset_by_type(result, doc_types)
  if (debug) cat("After type filtering:", ndoc(result), "documents\n")
  
  result <- subset_by_topic(result, topics, topic_threshold)
  if (debug) cat("After topic filtering:", ndoc(result), "documents\n")
  
  result <- subset_by_journal(result, journals)
  if (debug) cat("After journal filtering:", ndoc(result), "documents\n")
  
  result <- subset_by_authors(result, authors, author_match_type)
  if (debug) cat("After author filtering:", ndoc(result), "documents\n")
  
  result <- subset_by_author_count(result, min_authors, max_authors, single_author, multi_author)
  if (debug) cat("After author count filtering:", ndoc(result), "documents\n")
  
  # Handle community subsetting separately using network data
  if (!is.null(communities)) {
    result <- subset_by_community(result, communities, time_slice, match_any_author)
    if (debug) cat("After community filtering:", ndoc(result), "documents\n")
  }
  
  return(result)
}