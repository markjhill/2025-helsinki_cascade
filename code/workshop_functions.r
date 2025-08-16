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

# Function to track term usage over time
track_term_over_time <- function(dfm, term, time_var = "decade") {
  
  # Get document-level counts for the term
  term_counts <- dfm[, term]
  
  # Extract metadata for time grouping  
  time_data <- docvars(dfm, time_var)
  
  # Create summary by time period
  usage_by_time <- data.frame(
    time_period = time_data,
    term_count = as.numeric(term_counts),
    doc_length = ntoken(dfm)
  ) %>%
    group_by(time_period) %>%
    summarise(
      total_occurrences = sum(term_count),
      documents_using_term = sum(term_count > 0),
      total_documents = n(),
      total_tokens = sum(doc_length),
      usage_rate = total_occurrences / total_tokens * 1000, # per 1000 words
      document_penetration = documents_using_term / total_documents,
      .groups = 'drop'
    ) %>%
    filter(!is.na(time_period))
  
  return(usage_by_time)
}

# Function to identify key authors for specific terms/concepts
find_key_authors_for_term <- function(dfm, term, min_usage = 2) {
  
  # Check if term exists in the DFM
  if (!term %in% colnames(dfm)) {
    cat("Term '", term, "' not found in corpus vocabulary\n", sep = "")
    return(data.frame(
      author = character(0),
      total_term_usage = numeric(0),
      total_words_written = numeric(0),
      papers_using_term = numeric(0),
      usage_intensity = numeric(0)
    ))
  }
  
  # Get term counts by document - properly handle quanteda DFM
  term_counts <- as.numeric(dfm[, term])
  
  # Get author information
  author_data <- docvars(dfm)
  
  # Create data frame with all documents
  all_docs_data <- data.frame(
    doc_id = docnames(dfm),
    term_count = term_counts,
    doc_length = ntoken(dfm),
    stringsAsFactors = FALSE
  )
  
  # Filter to documents that actually use the term
  docs_with_term <- all_docs_data[all_docs_data$term_count > 0, ]
  
  if (nrow(docs_with_term) == 0) {
    cat("No documents found using term '", term, "'\n", sep = "")
    return(data.frame(
      author = character(0),
      total_term_usage = numeric(0),
      total_words_written = numeric(0),
      papers_using_term = numeric(0),
      usage_intensity = numeric(0)
    ))
  }
  
  # Get corresponding author information for these documents
  doc_indices <- match(docs_with_term$doc_id, docnames(dfm))
  relevant_authors_list <- author_data$authors_list[doc_indices]
  
  # Expand to individual authors (handling multi-author papers)
  author_rows <- list()
  
  for (i in 1:nrow(docs_with_term)) {
    current_authors <- relevant_authors_list[[i]]
    
    if (length(current_authors) > 0 && !is.null(current_authors)) {
      for (author in current_authors) {
        if (!is.na(author) && author != "" && nchar(trimws(author)) > 0) {
          author_rows[[length(author_rows) + 1]] <- data.frame(
            author = trimws(author),
            term_count = docs_with_term$term_count[i],
            doc_length = docs_with_term$doc_length[i],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  # Combine all author rows
  if (length(author_rows) > 0) {
    all_author_data <- do.call(rbind, author_rows)
    
    # Aggregate by author
    author_term_usage <- all_author_data %>%
      group_by(author) %>%
      summarise(
        total_term_usage = sum(term_count),
        total_words_written = sum(doc_length),
        papers_using_term = n(),
        usage_intensity = total_term_usage / total_words_written * 1000,
        .groups = 'drop'
      ) %>%
      filter(total_term_usage >= min_usage) %>%
      arrange(desc(total_term_usage))
    
    return(author_term_usage)
  } else {
    cat("No valid authors found for term '", term, "'\n", sep = "")
    return(data.frame(
      author = character(0),
      total_term_usage = numeric(0),
      total_words_written = numeric(0),
      papers_using_term = numeric(0),
      usage_intensity = numeric(0)
    ))
  }
}


# Function to find terms that appeared in specific time periods
find_period_innovations <- function(dfm, early_period, late_period, time_var = "decade") {
  
  # Subset to time periods
  early_dfm <- dfm[docvars(dfm, time_var) %in% early_period, ]
  late_dfm <- dfm[docvars(dfm, time_var) %in% late_period, ]
  
  # Get vocabulary for each period
  early_vocab <- colnames(early_dfm)[colSums(early_dfm) > 0]
  late_vocab <- colnames(late_dfm)[colSums(late_dfm) > 0]
  
  # Find innovations (terms that appear in late period but not early)
  innovations <- setdiff(late_vocab, early_vocab)
  
  # Calculate usage statistics for innovations in late period
  innovation_stats <- colSums(late_dfm[, innovations, drop = FALSE])
  innovation_docs <- colSums(late_dfm[, innovations, drop = FALSE] > 0)
  
  innovation_summary <- data.frame(
    term = names(innovation_stats),
    total_usage = innovation_stats,
    docs_using = innovation_docs,
    usage_per_doc = innovation_stats / innovation_docs
  ) %>%
    arrange(desc(total_usage))
  
  return(innovation_summary)
}

# Function to track concept usage across topics/communities
track_concept_diffusion <- function(dfm, term, grouping_var) {
  
  # Get term usage and grouping data
  term_counts <- dfm[, term]
  groups <- docvars(dfm, grouping_var)
  doc_lengths <- ntoken(dfm)
  
  # Calculate usage by group
  diffusion_data <- data.frame(
    group = groups,
    term_count = as.numeric(term_counts),
    doc_length = doc_lengths
  ) %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      total_usage = sum(term_count),
      docs_with_term = sum(term_count > 0),
      total_docs = n(),
      total_words = sum(doc_length),
      usage_rate = total_usage / total_words * 1000,
      penetration_rate = docs_with_term / total_docs,
      .groups = 'drop'
    ) %>%
    arrange(desc(usage_rate))
  
  return(diffusion_data)
}

# Turn docvars list-column into long edgelist: author <-> document
make_bipartite_edges <- function(dfm_comm) {
  tibble(
    doc = docnames(dfm_comm),
    authors = docvars(dfm_comm)$authors_list
  ) %>%
    unnest(authors) %>%
    mutate(type = "author")
}

plot_bipartite <- function(edges, comm_id) {
  # Build igraph object
  g <- graph_from_data_frame(edges %>% select(authors, doc), directed = FALSE)
  
  # Identify node types: author = TRUE, doc = FALSE
  V(g)$type <- V(g)$name %in% edges$authors
  
  # Plot
  ggraph(g, layout = "bipartite") +
    geom_edge_link(alpha = 0.4) +
    geom_node_point(aes(color = as.factor(type)), size = 3) +
    geom_node_text(aes(label = name), size = 2, repel = TRUE) +
    scale_color_manual(values = c("red", "steelblue"), labels = c("Document", "Author")) +
    theme_void() +
    ggtitle(paste("Bipartite network for Community", comm_id))
}


plot_coauthors <- function(edges, comm_id) {
  g_bip <- graph_from_data_frame(edges %>% select(authors, doc), directed = FALSE)
  V(g_bip)$type <- V(g_bip)$name %in% edges$authors
  
  # Project onto author–author network
  # Get both projections and select the one with authors
  projections <- bipartite_projection(g_bip)
  
  # Check which projection contains authors (should be the one with type = TRUE)
  author_nodes <- V(g_bip)$name[V(g_bip)$type == TRUE]
  
  # Select the projection that contains author nodes
  if (all(V(projections$proj1)$name %in% author_nodes)) {
    g_proj <- projections$proj1
  } else {
    g_proj <- projections$proj2
  }
  
  # Only plot if there are edges (co-authorships exist)
  if (ecount(g_proj) > 0) {
    ggraph(g_proj, layout = "fr") +
      geom_edge_link(alpha = 0.4, aes(width = weight)) +
      geom_node_point(size = 3, color = "steelblue") +
      geom_node_text(aes(label = name), size = 2.5, repel = TRUE) +
      theme_void() +
      ggtitle(paste("Co-author network for Community", comm_id))
  } else {
    # Handle case where no co-authorships exist
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "No co-authorships in this community", size = 5) +
      theme_void() +
      ggtitle(paste("Co-author network for Community", comm_id))
  }
}

plot_coauthors_with_topics <- function(dfm_comm, edges, comm_id) {
  g_bip <- graph_from_data_frame(edges %>% select(authors, doc), directed = FALSE)
  V(g_bip)$type <- V(g_bip)$name %in% edges$authors
  
  # Project onto author–author network
  projections <- bipartite_projection(g_bip)
  author_nodes <- V(g_bip)$name[V(g_bip)$type == TRUE]
  
  if (all(V(projections$proj1)$name %in% author_nodes)) {
    g_proj <- projections$proj1
  } else {
    g_proj <- projections$proj2
  }
  
  # Only proceed if there are edges
  if (ecount(g_proj) > 0) {
    # Create author-topic mapping
    doc_topics <- tibble(
      doc = docnames(dfm_comm),
      topic = docvars(dfm_comm)$primaryTopic
    )
    
    # Join with edges to get author-topic relationships
    author_topics <- edges %>%
      left_join(doc_topics, by = "doc") %>%
      group_by(authors) %>%
      # Get most common topic for each author, or concatenate multiple topics
      summarise(
        primary_topic = names(sort(table(topic), decreasing = TRUE))[1],
        topic_count = n_distinct(topic),
        all_topics = paste(unique(topic), collapse = ", "),
        .groups = 'drop'
      )
    
    # Add topic information to vertices
    vertex_data <- tibble(name = V(g_proj)$name) %>%
      left_join(author_topics, by = c("name" = "authors"))
    
    V(g_proj)$primary_topic <- vertex_data$primary_topic
    V(g_proj)$topic_count <- vertex_data$topic_count
    V(g_proj)$all_topics <- vertex_data$all_topics
    
    # Create color palette for topics
    unique_topics <- unique(vertex_data$primary_topic)
    topic_colors <- rainbow(length(unique_topics))
    names(topic_colors) <- unique_topics
    
    ggraph(g_proj, layout = "fr") +
      geom_edge_link(alpha = 0.4, aes(width = weight)) +
      geom_node_point(aes(color = primary_topic, size = topic_count), alpha = 0.8) +
      geom_node_text(aes(label = name), size = 2.5, repel = TRUE) +
      scale_color_manual(values = topic_colors, name = "Primary Topic") +
      scale_size_continuous(name = "Topic Diversity", range = c(3, 8)) +
      theme_void() +
      ggtitle(paste("Co-author network with Topics for Community", comm_id)) +
      theme(legend.position = "right")
  } else {
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "No co-authorships in this community", size = 5) +
      theme_void() +
      ggtitle(paste("Co-author network with Topics for Community", comm_id))
  }
}