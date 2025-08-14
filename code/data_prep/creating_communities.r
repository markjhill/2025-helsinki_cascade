library(igraph)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gplots)

################################
# Load data
author_df <- read.csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_author_nodes.csv')
publication_df <- read.csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_publication_nodes.csv')
edges_df <- read.csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_edges.csv')

cat("Loaded", nrow(author_df), "author nodes\n")
cat("Loaded", nrow(publication_df), "publication nodes\n")
cat("Loaded", nrow(edges_df), "edges\n")

# Clean duplicates
author_df_clean <- author_df %>% distinct(id, .keep_all = TRUE)
publication_df_clean <- publication_df %>% distinct(id, .keep_all = TRUE)
all_nodes <- bind_rows(author_df_clean, publication_df_clean) %>% distinct(id, .keep_all = TRUE)

# Create bipartite graph
G <- graph_from_data_frame(d = edges_df, vertices = all_nodes, directed = FALSE)

################################
# Function to get stable communities for a graph
get_stable_communities <- function(graph, graph_name, n_runs = 100) {
  cat("Processing", graph_name, "...\n")
  
  # Create author-only network
  vertex_types <- V(graph)$type == "author"
  author_projection <- bipartite_projection(graph, types = vertex_types)
  author_net <- author_projection$proj2  # Authors
  
  # Remove isolates (components with 1 node)
  components_info <- components(author_net)
  large_components <- which(components_info$csize >= 2)
  nodes_to_keep <- which(components_info$membership %in% large_components)
  
  if(length(nodes_to_keep) == 0) {
    cat("No connected authors in", graph_name, "\n")
    return(list(communities = integer(0), author_names = character(0), isolated_authors = character(0)))
  }
  
  author_net_filtered <- induced_subgraph(author_net, nodes_to_keep)
  
  cat("  ", vcount(author_net_filtered), "connected authors\n")
  
  # Run Louvain multiple times
  n_authors <- vcount(author_net_filtered)
  membership_matrix <- matrix(NA, nrow = n_authors, ncol = n_runs)
  
  for(i in 1:n_runs) {
    if(i %% 50 == 0) cat("    Run", i, "\n")
    communities_temp <- cluster_louvain(author_net_filtered)
    membership_matrix[, i] <- communities_temp$membership
  }
  
  # Calculate stability matrix
  stability_matrix <- matrix(0, nrow = n_authors, ncol = n_authors)
  for(i in 1:n_authors) {
    for(j in i:n_authors) {
      same_community <- sum(membership_matrix[i, ] == membership_matrix[j, ])
      stability_matrix[i, j] <- same_community / n_runs
      stability_matrix[j, i] <- stability_matrix[i, j]
    }
  }
  
  # Find stable communities
  distance_matrix <- 1 - stability_matrix
  stable_clustering <- hclust(as.dist(distance_matrix), method = "average")
  
  # Use 50% threshold
  stable_communities <- cutree(stable_clustering, h = 0.5)
  
  cat("  Found", max(stable_communities), "communities\n")
  
  return(list(
    communities = stable_communities,
    author_names = names(V(author_net_filtered)),
    isolated_authors = setdiff(names(V(author_net)), names(V(author_net_filtered)))
  ))
}

################################
# Generate time slices
start_year <- 1650
end_year <- 1925
slice_length <- 50
step_size <- 25

time_slices <- list()
slice_names <- c()

for(year in seq(start_year, end_year - slice_length, step_size)) {
  slice_name <- paste0("slice_", year, "_", year + slice_length - 1)
  time_slices[[slice_name]] <- c(year, year + slice_length - 1)
  slice_names <- c(slice_names, slice_name)
}

cat("Created", length(time_slices), "time slices:", paste(slice_names, collapse = ", "), "\n")

################################
# Process overall graph

overall_result <- get_stable_communities(G, "Overall", n_runs = 1000)

################################
# Process time slices
time_slice_results <- list()

for(slice_name in names(time_slices)) {
  cat("\n=== TIME SLICE", slice_name, "===\n")
  
  years <- time_slices[[slice_name]]
  
  # Filter publications by year
  slice_pubs <- V(G)[V(G)$type == "publication" & 
                       V(G)$year >= years[1] & 
                       V(G)$year <= years[2]]
  
  if(length(slice_pubs) == 0) {
    cat("No publications in", slice_name, "\n")
    next
  }
  
  # Get authors connected to these publications
  slice_edges <- edges_df[edges_df$target %in% names(slice_pubs), ]
  slice_authors <- unique(slice_edges$source)
  
  # Create subgraph
  slice_nodes <- c(slice_authors, names(slice_pubs))
  slice_graph <- induced_subgraph(G, which(names(V(G)) %in% slice_nodes))
  
  cat("  ", sum(V(slice_graph)$type == "publication"), "publications,", 
      sum(V(slice_graph)$type == "author"), "authors\n")
  
  if(sum(V(slice_graph)$type == "author") > 1) {
    time_slice_results[[slice_name]] <- get_stable_communities(slice_graph, slice_name, n_runs = 1000)
  }
}

################################
# Build author results table with dynamic columns


# Get all unique authors
all_authors <- unique(c(
  overall_result$author_names,
  overall_result$isolated_authors,
  unlist(lapply(time_slice_results, function(x) c(x$author_names, x$isolated_authors)))
))

# Initialize author table with all time slice columns
author_results <- data.frame(
  node_id = all_authors,
  node_type = "author",
  all_time_communities = NA,
  stringsAsFactors = FALSE
)

# Add columns for each time slice
for(slice_name in slice_names) {
  author_results[[slice_name]] <- NA
}

# Fill in overall communities
overall_communities <- rep(0, length(overall_result$isolated_authors))  # Isolated = 0
names(overall_communities) <- overall_result$isolated_authors

connected_communities <- overall_result$communities
names(connected_communities) <- overall_result$author_names

all_overall_communities <- c(overall_communities, connected_communities)
author_results$all_time_communities <- all_overall_communities[author_results$node_id]

# Fill in time slice communities
for(slice_name in names(time_slice_results)) {
  result <- time_slice_results[[slice_name]]
  
  # Connected authors
  for(i in seq_along(result$author_names)) {
    author_name <- result$author_names[i]
    community <- result$communities[i]
    
    if(author_name %in% author_results$node_id) {
      author_results[[slice_name]][author_results$node_id == author_name] <- community
    }
  }
  
  # Isolated authors
  for(author_name in result$isolated_authors) {
    if(author_name %in% author_results$node_id) {
      author_results[[slice_name]][author_results$node_id == author_name] <- 0
    }
  }
}

################################
# Build publication results


pub_results <- data.frame(
  node_id = names(V(G)[V(G)$type == "publication"]),
  node_type = "publication",
  stringsAsFactors = FALSE
)

# Add all_time_communities and time slice columns (all NA for publications)
pub_results$all_time_communities <- NA
for(slice_name in slice_names) {
  pub_results[[slice_name]] <- NA
}

# Add publication_communities column
pub_results$publication_communities <- NA

# For each publication, collect all communities from connected authors
for(i in 1:nrow(pub_results)) {
  pub_id <- pub_results$node_id[i]
  
  # Get connected authors
  connected_authors <- neighbors(G, pub_id)
  author_names <- names(connected_authors)
  
  # Collect all communities these authors belong to
  all_communities <- c()
  
  for(author_name in author_names) {
    author_row <- author_results[author_results$node_id == author_name, ]
    
    if(nrow(author_row) > 0) {
      # Add communities from all time periods (overall + all time slices)
      community_columns <- c("all_time_communities", slice_names)
      
      for(col in community_columns) {
        community_val <- author_row[[col]]
        if(!is.na(community_val)) {
          all_communities <- c(all_communities, community_val)
        }
      }
    }
  }
  
  # Remove duplicates and store as comma-separated string
  unique_communities <- unique(all_communities)
  if(length(unique_communities) > 0) {
    pub_results$publication_communities[i] <- paste(unique_communities, collapse = ",")
  }
  
  if(i %% 1000 == 0) cat("  Processed", i, "publications\n")
}

# Count non-NAs per column for authors
author_non_na_counts <- sapply(author_results, function(x) sum(!is.na(x)))

# Create histogram
barplot(author_non_na_counts, 
        main = "Non-NA Count per Column (Authors)",
        xlab = "Column", 
        ylab = "Count of Non-NAs",
        las = 2,  # Rotate x-axis labels
        cex.names = 0.7)  # Smaller text for readability

# Print the counts
print(author_non_na_counts)

################################
# Combine results

# Add pub column
author_results$publication_communities <- NA


final_results <- rbind(
  author_results,
  pub_results
)

# Save results
write.csv(final_results, "C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\multi_temporal_communities.csv", row.names = FALSE)

cat("\nSaved", nrow(final_results), "nodes to multi_temporal_communities.csv\n")
cat("Authors:", sum(final_results$node_type == "author"), "\n")
cat("Publications:", sum(final_results$node_type == "publication"), "\n")
cat("Columns:", paste(colnames(final_results), collapse = ", "), "\n")
