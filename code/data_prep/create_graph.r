library(readr)
library(dplyr)
library(stringr)
library(igraph)
library(purrr)
library(ggplot2)
library(RColorBrewer)

# Create graph of authors
df <- read.csv("C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\Royal_Society_Corpus_open_v6.0_meta.tsv", sep = "\t")

# Check author data
df %>% 
  filter(!is.na(author)) %>% 
  slice_head(n = 10) %>% 
  select(author)

# Optional: Filter by century
# df <- df %>% filter(century == 1700)

# Split author strings into lists
df <- df %>% 
  mutate(author_list = str_split(author, "\\|"))

cat("Papers:", nrow(df), "\n")
cat("Papers with authors:", sum(!is.na(df$author_list)), "\n")

# Look at first few author lists
df$author_list[1:5]

# Flatten all author lists into one big list
all_authors <- df %>% 
  filter(!is.na(author_list)) %>% 
  pull(author_list) %>% 
  unlist() %>%
  str_trim()  # Add this line to remove leading/trailing whitespace


# Get unique authors
unique_authors <- unique(all_authors)
cat("Total unique authors:", length(unique_authors), "\n")

# Create author nodes
author_nodes <- tibble(
  id = unique_authors,
  type = "author"
)

# Create publication nodes
pub_cols <- c('id', 'title', 'year', 'decade', 'volume', 'pages', 'sentences',
              'tokens', 'journal', 'type', 'primaryTopic', 'primaryTopicPercentage',
              'secondaryTopic', 'secondaryTopicPercentage')

publication_nodes <- df %>% 
  select(all_of(pub_cols)) %>% 
  rename(doc_type = type) %>%  # Rename 'type' to avoid confusion with node type
  mutate(type = "publication")

cat("Author nodes:", nrow(author_nodes), "\n")
cat("Sample author node:\n")
print(author_nodes[1, ])
cat("\n")
cat("Publication nodes:", nrow(publication_nodes), "\n")
cat("Sample publication node:\n")
print(publication_nodes[1, ])

# Create edge list
edges <- df %>% 
  filter(!is.na(author_list)) %>% 
  select(id, author_list) %>% 
  unnest(author_list) %>% 
  mutate(author_list = str_trim(author_list)) %>%
  rename(source = author_list, target = id) %>% 
  mutate(type = "authored")

cat("Total edges:", nrow(edges), "\n")
cat("Sample edges:\n")
print(edges[1:3, ])

################################
# Save data
cat("Author nodes:", nrow(author_nodes), "\n")
cat("Publication nodes:", nrow(publication_nodes), "\n")
cat("Edges:", nrow(edges), "\n")

write_csv(author_nodes, 'C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_author_nodes.csv')
write_csv(publication_nodes, 'C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_publication_nodes.csv')
write_csv(edges, 'C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_edges.csv')

################################
# Optional: Create igraph object for analysis
################################

# Combine all nodes
all_nodes <- bind_rows(author_nodes, publication_nodes)

# Create igraph object
g <- graph_from_data_frame(d = edges, vertices = all_nodes, directed = FALSE)

cat("Graph summary:\n")
print(g)
cat("Number of vertices:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")

# Basic network statistics
cat("Network density:", edge_density(g), "\n")
cat("Is connected:", is_connected(g), "\n")
cat("Number of components:", components(g)$no, "\n")

# Author-specific statistics
author_vertices <- V(g)[V(g)$type == "author"]
pub_vertices <- V(g)[V(g)$type == "publication"]

cat("Author vertices:", length(author_vertices), "\n")
cat("Publication vertices:", length(pub_vertices), "\n")

# Degree distribution for authors (number of papers they've authored)
author_degrees <- degree(g, v = author_vertices)
cat("Author degree statistics:\n")
print(summary(author_degrees))

# Most prolific authors
top_authors <- sort(author_degrees, decreasing = TRUE)[1:10]
cat("Top 10 most prolific authors:\n")
print(top_authors)

# Publication degree distribution (number of authors per paper)
pub_degrees <- degree(g, v = pub_vertices)
cat("Publication degree statistics (authors per paper):\n")
print(summary(pub_degrees))

################################
# Additional network analysis functions
################################

# Function to get author collaboration network (bipartite projection)
create_author_collaboration_network <- function(bipartite_graph) {
  # Project to author-author network
  author_net <- bipartite_projection(bipartite_graph, which = "false")$proj1
  return(author_net)
}

# Function to analyze temporal patterns
analyze_temporal_patterns <- function(graph, time_var = "year") {
  # Get publication nodes with time information
  pub_data <- as_data_frame(graph, what = "vertices") %>% 
    filter(type == "publication") %>% 
    select(name, all_of(time_var))
  
  # Get edges and add time information
  edge_data <- as_data_frame(graph, what = "edges") %>% 
    left_join(pub_data, by = c("to" = "name"))
  
  return(edge_data)
}

# Example usage:
# author_collab_net <- create_author_collaboration_network(g)
# temporal_edges <- analyze_temporal_patterns(g, "decade")

cat("\nNetwork creation complete! Files saved and igraph object 'g' created.\n")
cat("Use the graph object 'g' for further network analysis with igraph functions.\n")
