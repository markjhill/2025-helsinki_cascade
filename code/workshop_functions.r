# Universal Functions for analysing corpus and DFM data
# These functions work with both quanteda corpus and DFM objects

# Helper function to detect object type and get docvars
get_docvars <- function(obj) {
  if (inherits(obj, "corpus")) {
    return(docvars(obj))
  } else if (inherits(obj, "dfm")) {
    return(docvars(obj))
  } else {
    stop("Object must be a quanteda corpus or dfm")
  }
}

# Helper function to get document count
get_ndoc <- function(obj) {
  if (inherits(obj, "corpus")) {
    return(ndoc(obj))
  } else if (inherits(obj, "dfm")) {
    return(ndoc(obj))
  } else {
    stop("Object must be a quanteda corpus or dfm")
  }
}

# Helper function to subset objects
subset_object <- function(obj, logical_vector) {
  if (inherits(obj, "corpus")) {
    return(corpus_subset(obj, logical_vector))
  } else if (inherits(obj, "dfm")) {
    return(dfm_subset(obj, logical_vector))
  } else {
    stop("Object must be a quanteda corpus or dfm")
  }
}

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

# Function to subset corpus/dfm based on community membership
subset_corpus_by_communities <- function(obj, communities, time_slice = "all_time_communities", 
                                         match_any_author = TRUE) {
  
  # Get authors from these communities
  target_authors <- get_authors_from_communities(communities, time_slice)
  
  if (length(target_authors) == 0) {
    warning("No authors found in specified communities for time slice: ", time_slice)
    return(subset_object(obj, rep(FALSE, get_ndoc(obj))))
  }
  
  # Get document variables
  obj_docvars <- get_docvars(obj)
  
  # Find documents with these authors
  if (match_any_author) {
    # Document matches if ANY author is in target communities
    subset_docs <- map_lgl(obj_docvars$authors_list, function(doc_authors) {
      any(doc_authors %in% target_authors)
    })
  } else {
    # Document matches if ALL authors are in target communities
    subset_docs <- map_lgl(obj_docvars$authors_list, function(doc_authors) {
      all(doc_authors %in% target_authors)
    })
  }
  
  return(subset_object(obj, subset_docs))
}

# Function to subset corpus/dfm by date range
subset_by_date <- function(obj, start_year = NULL, end_year = NULL, 
                           decade = NULL, century = NULL) {
  
  obj_docvars <- get_docvars(obj)
  subset_docs <- rep(TRUE, get_ndoc(obj))
  
  # Filter by year range
  if (!is.null(start_year) || !is.null(end_year)) {
    if (is.null(start_year)) start_year <- min(obj_docvars$year, na.rm = TRUE)
    if (is.null(end_year)) end_year <- max(obj_docvars$year, na.rm = TRUE)
    
    subset_docs <- subset_docs & 
      obj_docvars$year >= start_year & 
      obj_docvars$year <= end_year & 
      !is.na(obj_docvars$year)
  }
  
  # Filter by decade
  if (!is.null(decade)) {
    subset_docs <- subset_docs & obj_docvars$decade %in% decade
  }
  
  # Filter by century using existing century docvar
  if (!is.null(century)) {
    subset_docs <- subset_docs & obj_docvars$century %in% century
  }
  
  return(subset_object(obj, subset_docs))
}

# Function to subset corpus/dfm by document type
subset_by_type <- function(obj, doc_types) {
  if (!is.null(doc_types) && length(doc_types) > 0) {
    obj_docvars <- get_docvars(obj)
    subset_docs <- obj_docvars$type %in% doc_types
    return(subset_object(obj, subset_docs))
  }
  return(obj)
}

# Function to subset corpus/dfm by primary topic
subset_by_topic <- function(obj, topics, topic_threshold = NULL) {
  if (!is.null(topics) && length(topics) > 0) {
    obj_docvars <- get_docvars(obj)
    subset_docs <- obj_docvars$primaryTopic %in% topics
    
    # Optional: filter by topic percentage threshold
    if (!is.null(topic_threshold)) {
      subset_docs <- subset_docs & 
        obj_docvars$primaryTopicPercentage >= topic_threshold
    }
    
    return(subset_object(obj, subset_docs))
  }
  return(obj)
}

# Function to subset corpus/dfm by journal
subset_by_journal <- function(obj, journals) {
  if (!is.null(journals) && length(journals) > 0) {
    obj_docvars <- get_docvars(obj)
    subset_docs <- obj_docvars$jrnl %in% journals
    return(subset_object(obj, subset_docs))
  }
  return(obj)
}

# Function to subset corpus/dfm by authors
subset_by_authors <- function(obj, authors, match_type = "any") {
  if (!is.null(authors) && length(authors) > 0) {
    obj_docvars <- get_docvars(obj)
    
    if (match_type == "any") {
      # Documents where any of the specified authors appear
      subset_docs <- map_lgl(obj_docvars$authors_list, function(doc_authors) {
        any(authors %in% doc_authors)
      })
    } else if (match_type == "all") {
      # Documents where all specified authors appear
      subset_docs <- map_lgl(obj_docvars$authors_list, function(doc_authors) {
        all(authors %in% doc_authors)
      })
    } else if (match_type == "first") {
      # Documents where the first author is one of the specified authors
      subset_docs <- obj_docvars$first_author %in% authors
    }
    
    return(subset_object(obj, subset_docs))
  }
  return(obj)
}

# Function to subset corpus/dfm by author count
subset_by_author_count <- function(obj, min_authors = NULL, max_authors = NULL, 
                                   single_author = NULL, multi_author = NULL) {
  obj_docvars <- get_docvars(obj)
  subset_docs <- rep(TRUE, get_ndoc(obj))
  
  if (!is.null(min_authors)) {
    subset_docs <- subset_docs & obj_docvars$author_count >= min_authors
  }
  
  if (!is.null(max_authors)) {
    subset_docs <- subset_docs & obj_docvars$author_count <= max_authors
  }
  
  if (!is.null(single_author) && single_author) {
    subset_docs <- subset_docs & obj_docvars$author_count == 1
  }
  
  if (!is.null(multi_author) && multi_author) {
    subset_docs <- subset_docs & obj_docvars$author_count > 1
  }
  
  return(subset_object(obj, subset_docs))
}

# Function to subset corpus/dfm by community
subset_by_community <- function(obj, communities, time_slice = "all_time_communities", 
                                match_any_author = TRUE) {
  return(subset_corpus_by_communities(obj, communities, time_slice, match_any_author))
}

# General subsetting function that combines all criteria
subset_corpus <- function(obj, 
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
  
  result <- obj
  if (debug) cat("Starting with", get_ndoc(result), "documents\n")
  
  result <- subset_by_date(result, start_year, end_year, decade, century)
  if (debug) cat("After date filtering:", get_ndoc(result), "documents\n")
  
  result <- subset_by_type(result, doc_types)
  if (debug) cat("After type filtering:", get_ndoc(result), "documents\n")
  
  result <- subset_by_topic(result, topics, topic_threshold)
  if (debug) cat("After topic filtering:", get_ndoc(result), "documents\n")
  
  result <- subset_by_journal(result, journals)
  if (debug) cat("After journal filtering:", get_ndoc(result), "documents\n")
  
  result <- subset_by_authors(result, authors, author_match_type)
  if (debug) cat("After author filtering:", get_ndoc(result), "documents\n")
  
  result <- subset_by_author_count(result, min_authors, max_authors, single_author, multi_author)
  if (debug) cat("After author count filtering:", get_ndoc(result), "documents\n")
  
  # Handle community subsetting separately using network data
  if (!is.null(communities)) {
    result <- subset_by_community(result, communities, time_slice, match_any_author)
    if (debug) cat("After community filtering:", get_ndoc(result), "documents\n")
  }
  
  return(result)
}

# Convenience wrapper functions for backward compatibility
subset_dfm <- function(dfm, ...) {
  subset_corpus(dfm, ...)
}