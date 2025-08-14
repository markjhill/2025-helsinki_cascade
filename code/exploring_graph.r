library(igraph)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gplots)


################################
# Load data
author_df <- read_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_author_nodes.csv')
publication_df <- read_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_publication_nodes.csv')
edges_df <- read_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_edges.csv')

cat("Converted", nrow(author_df), "author nodes\n")
cat("Converted", nrow(publication_df), "publication nodes\n")
cat("Converted", nrow(edges_df), "edges\n")

################################
# Load the igraph object
# First, let's check for duplicate IDs
cat("Checking for duplicates...\n")
cat("Author nodes with duplicates:", sum(duplicated(author_df$id)), "\n")
cat("Publication nodes with duplicates:", sum(duplicated(publication_df$id)), "\n")

# Check for overlapping IDs between authors and publications
overlap <- intersect(author_df$id, publication_df$id)
cat("Overlapping IDs between authors and publications:", length(overlap), "\n")
if(length(overlap) > 0) {
  cat("First few overlapping IDs:", head(overlap), "\n")
}

# Remove duplicates within each dataset
author_df_clean <- author_df %>% distinct(id, .keep_all = TRUE)
publication_df_clean <- publication_df %>% distinct(id, .keep_all = TRUE)

cat("After removing duplicates:\n")
cat("Unique author nodes:", nrow(author_df_clean), "\n")
cat("Unique publication nodes:", nrow(publication_df_clean), "\n")

# Combine all nodes (this should now work without duplicates)
all_nodes <- bind_rows(
  author_df_clean,
  publication_df_clean
)

# Double-check for any remaining duplicates
remaining_dups <- sum(duplicated(all_nodes$id))
cat("Remaining duplicate IDs in combined dataset:", remaining_dups, "\n")

if(remaining_dups > 0) {
  # If there are still duplicates, remove them
  cat("Removing remaining duplicates...\n")
  all_nodes <- all_nodes %>% distinct(id, .keep_all = TRUE)
  cat("Final unique nodes:", nrow(all_nodes), "\n")
}

# Create bipartite graph
G <- graph_from_data_frame(d = edges_df, vertices = all_nodes, directed = FALSE)

################################
# Explore
cat("Total nodes:", vcount(G), "\n")
cat("Total edges:", ecount(G), "\n")
cat("Is connected:", is_connected(G), "\n")
cat("Number of components:", components(G)$no, "\n")

# Check node types
author_nodes_in_graph <- V(G)[V(G)$type == "author"]
pub_nodes_in_graph <- V(G)[V(G)$type == "publication"]

cat("Author nodes:", length(author_nodes_in_graph), "\n")
cat("Publication nodes:", length(pub_nodes_in_graph), "\n")

# Get document types
doc_types <- V(G)$doc_type[V(G)$type == "publication"]
doc_type_counts <- table(doc_types)
cat("\nDocument types in network:\n")
print(doc_type_counts)

# Basic degree statistics
degrees <- degree(G)
cat("\nDegree statistics:\n")
cat("Average degree:", round(mean(degrees), 2), "\n")
cat("Max degree:", max(degrees), "\n")
cat("Min degree:", min(degrees), "\n")

# Explore connections
# Get degree for author nodes only
author_degrees <- degree(G, v = author_nodes_in_graph)
author_degrees_sorted <- sort(author_degrees, decreasing = TRUE)

cat("\nMost connected authors (top 10):\n")
for(i in 1:min(10, length(author_degrees_sorted))) {
  author_name <- names(author_degrees_sorted)[i]
  degree_val <- author_degrees_sorted[i]
  cat(author_name, ":", degree_val, "connections\n")
}

# Check what document types the most connected author publishes
top_author <- names(author_degrees_sorted)[1]
connected_pubs <- neighbors(G, top_author)
connected_doc_types <- V(G)$doc_type[connected_pubs]

cat("\n", top_author, "publishes:\n")
print(table(connected_doc_types))

################################
# Detect
################################

# Run Louvain community detection on the bipartite graph
communities <- cluster_louvain(author_net_communities)

################################
# 1. FULL BIPARTITE GRAPH - AUTHORS AND PUBLICATIONS COLORED DIFFERENTLY
################################

# Set up colors for node types
V(G)$color <- ifelse(V(G)$type == "author", "lightblue", "lightcoral")

# Plot 1: Full bipartite graph
plot(G, 
     vertex.label = NA,           # No labels
     vertex.size = 2,             # Small nodes
     vertex.color = V(G)$color,
     edge.color = "gray80",
     edge.width = 0.5,
     layout = layout_with_fr(G),
     main = "Full Bipartite Graph: Authors (blue) and Publications (red)")

# Add legend
legend("topright", 
       legend = c("Authors", "Publications"), 
       col = c("lightblue", "lightcoral"), 
       pch = 19, 
       cex = 0.8)

# Note: this is a typical exmaple of why graph visualisations are often not very useful.
# It's much too busy, and lacks any real structure or detail as this scale. 
# It's often more helpful to simplify. In this case we will do this in a few ways.

################################
# 2. AUTHOR COLLABORATION NETWORK - NO PUBLICATIONS
################################

# Create bipartite projection to get author-only network
vertex_types <- V(G)$type == "author"
author_projection <- bipartite_projection(G, types = vertex_types)
author_net <- author_projection$proj1

# Plot flattened author collaboration network
plot(author_net,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = "lightblue",
     edge.color = "gray70",
     edge.width = 0.8,
     layout = layout_with_fr(author_net),
     main = "Author Collaboration Network (flattened)")

################################
# 3. AUTHOR COLLABORATION NETWORK - NO ISOLATES
################################

# Remove isolated nodes (degree = 0)
isolated_nodes <- which(degree(author_net) == 0)
author_net_no_isolates <- delete_vertices(author_net, isolated_nodes)

# Plot 3: Author network without isolates
plot(author_net_no_isolates,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = V(author_net_no_isolates)$topic_color,
     edge.color = "gray70",
     edge.width = 0.8,
     layout = layout_with_fr(author_net_no_isolates),
     main = "Author Collaboration Network (no isolates)")

cat("Original author network:", vcount(author_net), "nodes,", ecount(author_net), "edges\n")
cat("Without isolates:", vcount(author_net_no_isolates), "nodes,", ecount(author_net_no_isolates), "edges\n")
cat("Removed", length(isolated_nodes), "isolated authors\n")

################################
# 4. COMPONENTS
################################

# Get component information
comp <- components(author_net)

# Create table of component sizes
component_table <- table(comp$csize)
component_df <- data.frame(
  component_size = as.numeric(names(component_table)),
  number_of_components = as.numeric(component_table)
)

# Sort by component size
component_df <- component_df[order(component_df$component_size, decreasing = TRUE), ]

print(component_df)

# Summary statistics
cat("\nComponent Summary:\n")
cat("Total components:", comp$no, "\n")
cat("Largest component size:", max(comp$csize), "\n")
cat("Number of isolates (size 1):", sum(comp$csize == 1), "\n")
cat("Nodes in largest component:", round(max(comp$csize)/vcount(author_net)*100, 1), "% of total\n")

# Calculate nodes in largest component as percentage of non-isolated nodes
non_isolated_nodes <- vcount(author_net) - sum(comp$csize == 1)
largest_comp_pct_non_isolated <- round(max(comp$csize)/non_isolated_nodes*100, 1)

cat("Nodes in largest component (excluding isolates):", largest_comp_pct_non_isolated, "% of non-isolated nodes\n")

# Get second largest component size
sorted_sizes <- sort(comp$csize, decreasing = TRUE)
second_largest_size <- sorted_sizes[2]

# Calculate percentages for second largest component
second_comp_pct_total <- round(second_largest_size/vcount(author_net)*100, 1)
second_comp_pct_non_isolated <- round(second_largest_size/non_isolated_nodes*100, 1)

cat("Second largest component size:", second_largest_size, "\n")
cat("Second largest component:", second_comp_pct_total, "% of total nodes\n")
cat("Second largest component (excluding isolates):", second_comp_pct_non_isolated, "% of non-isolated nodes\n")


# Get decades for publications by authors in main component
# Get largest component from the ORIGINAL bipartite graph G
comp_G <- components(G)
largest_comp_nodes_G <- which(comp_G$membership == which.max(comp_G$csize))
largest_comp_subgraph_G <- induced_subgraph(G, largest_comp_nodes_G)

# Get just the publication nodes from the main component
main_comp_pubs <- V(largest_comp_subgraph_G)[V(largest_comp_subgraph_G)$type == "publication"]
main_comp_pub_decades <- V(largest_comp_subgraph_G)$decade[main_comp_pubs]

# Count by decade
total_by_decade <- table(V(G)$decade[V(G)$type == "publication"])
main_comp_by_decade <- table(main_comp_pub_decades)

# Calculate proportions
decades <- sort(unique(V(G)$decade[V(G)$type == "publication" & !is.na(V(G)$decade)]))
proportions <- main_comp_by_decade[as.character(decades)] / total_by_decade[as.character(decades)]
proportions[is.na(proportions)] <- 0

# Plot
barplot(proportions, 
        names.arg = decades,
        main = "Proportion of Publications in Main Component",
        xlab = "Decade", 
        ylab = "Proportion",
        las = 2,
        cex.names = 0.7)


##########
# This can be a useful strategy depending on your data. Why may it not be ideal in this case?
# Here we are missing some specific eras. Perhaps by component is not the best approach in this case.
# Because the components may represent specific disciplinary groups 
# where mathematicians and biologists fail to publish together.

################################
# CHECK PROPORTION OF TOPICS IN MAIN COMPONENT
################################

# Get publication IDs in main component
main_comp_pub_ids <- names(V(largest_comp_subgraph_G))[V(largest_comp_subgraph_G)$type == "publication"]

# Calculate topic proportions
topic_summary <- publication_df %>%
  filter(!is.na(primaryTopic)) %>%
  group_by(primaryTopic) %>%
  summarise(
    total_pubs = n(),
    in_main_comp = sum(id %in% main_comp_pub_ids),
    proportion_in_main = in_main_comp / total_pubs,
    proportion_lost = 1 - proportion_in_main,
    .groups = "drop"
  ) %>%
  arrange(desc(proportion_lost))

print(topic_summary)

# Plot the proportion lost by topic
barplot(topic_summary$proportion_lost,
        names.arg = topic_summary$primaryTopic,
        main = "Proportion of Publications Lost by Topic (when keeping only main component)",
        xlab = "Topic",
        ylab = "Proportion Lost",
        las = 2,
        cex.names = 0.6)

# So we would be losing a LOT of subject/topic specific data if we dropped the all but the main component.

################################
# SUBSET BY TIME
################################

total_G <- G
G <- total_G

# Filter for 18th century publications (1700-1799)
start_year <- 1725
finish_year <- 1799

eighteenth_century_pubs <- V(G)[V(G)$type == "publication" & V(G)$year >= start_year & V(G)$year <= finish_year]
eighteenth_century_pub_ids <- names(eighteenth_century_pubs)

# Get authors connected to these publications
eighteenth_century_edges <- edges_df[edges_df$target %in% eighteenth_century_pub_ids, ]
eighteenth_century_authors <- unique(eighteenth_century_edges$source)

# Get all nodes for 18th century subgraph
eighteenth_century_nodes <- c(eighteenth_century_authors, eighteenth_century_pub_ids)

# Create subgraph
G_18th <- induced_subgraph(G, which(names(V(G)) %in% eighteenth_century_nodes))

cat("18th century subgraph:\n")
cat("Total nodes:", vcount(G_18th), "\n")
cat("Authors:", sum(V(G_18th)$type == "author"), "\n") 
cat("Publications:", sum(V(G_18th)$type == "publication"), "\n")
cat("Edges:", ecount(G_18th), "\n")

G <- G_18th

################################
# 1. FULL BIPARTITE GRAPH - AUTHORS AND PUBLICATIONS COLORED DIFFERENTLY
################################

# Set up colors for node types
V(G)$color <- ifelse(V(G)$type == "author", "lightblue", "lightcoral")

# Plot 1: Full bipartite graph
plot(G, 
     vertex.label = NA,           # No labels
     vertex.size = 2,             # Small nodes
     vertex.color = V(G)$color,
     edge.color = "gray80",
     edge.width = 0.5,
     layout = layout_with_fr(G),
     main = "Full Bipartite Graph: Authors (blue) and Publications (red)")

# Add legend
legend("topright", 
       legend = c("Authors", "Publications"), 
       col = c("lightblue", "lightcoral"), 
       pch = 19, 
       cex = 0.8)

################################
# 2. AUTHOR COLLABORATION NETWORK - NO PUBLICATIONS
################################

# Create bipartite projection to get author-only network
vertex_types <- V(G)$type == "author"
author_projection <- bipartite_projection(G, types = vertex_types)
author_net <- author_projection$proj1

# Plot flattened author collaboration network
plot(author_net,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = "lightblue",
     edge.color = "gray70",
     edge.width = 0.8,
     layout = layout_with_fr(author_net),
     main = "Author Collaboration Network (flattened)")

################################
# 3. AUTHOR COLLABORATION NETWORK - NO ISOLATES
################################

# Remove isolated nodes (degree = 0)
isolated_nodes <- which(degree(author_net) == 0)
author_net_no_isolates <- delete_vertices(author_net, isolated_nodes)

# Plot 3: Author network without isolates
plot(author_net_no_isolates,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = V(author_net_no_isolates)$topic_color,
     edge.color = "gray70",
     edge.width = 0.8,
     layout = layout_with_fr(author_net_no_isolates),
     main = "Author Collaboration Network (no isolates)")

cat("Original author network:", vcount(author_net), "nodes,", ecount(author_net), "edges\n")
cat("Without isolates:", vcount(author_net_no_isolates), "nodes,", ecount(author_net_no_isolates), "edges\n")
cat("Removed", length(isolated_nodes), "isolated authors\n")

################################
# 4. COMPONENTS
################################

# Get component information
comp <- components(author_net)

# Create table of component sizes
component_table <- table(comp$csize)
component_df <- data.frame(
  component_size = as.numeric(names(component_table)),
  number_of_components = as.numeric(component_table)
)

# Sort by component size
component_df <- component_df[order(component_df$component_size, decreasing = TRUE), ]

print(component_df)

# Summary statistics
cat("\nComponent Summary:\n")
cat("Total components:", comp$no, "\n")
cat("Largest component size:", max(comp$csize), "\n")
cat("Number of isolates (size 1):", sum(comp$csize == 1), "\n")
cat("Nodes in largest component:", round(max(comp$csize)/vcount(author_net)*100, 1), "% of total\n")

# Calculate nodes in largest component as percentage of non-isolated nodes
non_isolated_nodes <- vcount(author_net) - sum(comp$csize == 1)
largest_comp_pct_non_isolated <- round(max(comp$csize)/non_isolated_nodes*100, 1)

cat("Nodes in largest component (excluding isolates):", largest_comp_pct_non_isolated, "% of non-isolated nodes\n")

# Get second largest component size
sorted_sizes <- sort(comp$csize, decreasing = TRUE)
second_largest_size <- sorted_sizes[2]

# Calculate percentages for second largest component
second_comp_pct_total <- round(second_largest_size/vcount(author_net)*100, 1)
second_comp_pct_non_isolated <- round(second_largest_size/non_isolated_nodes*100, 1)

cat("Second largest component size:", second_largest_size, "\n")
cat("Second largest component:", second_comp_pct_total, "% of total nodes\n")
cat("Second largest component (excluding isolates):", second_comp_pct_non_isolated, "% of non-isolated nodes\n")


# Get decades for publications by authors in main component
# Get largest component from the ORIGINAL bipartite graph G
comp_G <- components(G)
largest_comp_nodes_G <- which(comp_G$membership == which.max(comp_G$csize))
largest_comp_subgraph_G <- induced_subgraph(G, largest_comp_nodes_G)

# Get just the publication nodes from the main component
main_comp_pubs <- V(largest_comp_subgraph_G)[V(largest_comp_subgraph_G)$type == "publication"]
main_comp_pub_decades <- V(largest_comp_subgraph_G)$decade[main_comp_pubs]

# Count by decade
total_by_decade <- table(V(G)$decade[V(G)$type == "publication"])
main_comp_by_decade <- table(main_comp_pub_decades)

# Calculate proportions
decades <- sort(unique(V(G)$decade[V(G)$type == "publication" & !is.na(V(G)$decade)]))
proportions <- main_comp_by_decade[as.character(decades)] / total_by_decade[as.character(decades)]
proportions[is.na(proportions)] <- 0

# Plot
barplot(proportions, 
        names.arg = decades,
        main = "Proportion of Publications in Main Component",
        xlab = "Decade", 
        ylab = "Proportion",
        las = 2,
        cex.names = 0.7)


################################
# CHECK PROPORTION OF TOPICS IN MAIN COMPONENT
################################

# Get publication IDs in main component
main_comp_pub_ids <- names(V(largest_comp_subgraph_G))[V(largest_comp_subgraph_G)$type == "publication"]

# Calculate topic proportions
topic_summary <- publication_df %>%
  filter(year >= start_year) %>%
  filter(year < finish_year) %>%
  filter(!is.na(primaryTopic)) %>%
  group_by(primaryTopic) %>%
  summarise(
    total_pubs = n(),
    in_main_comp = sum(id %in% main_comp_pub_ids),
    proportion_in_main = in_main_comp / total_pubs,
    proportion_lost = 1 - proportion_in_main,
    .groups = "drop"
  ) %>%
  arrange(desc(proportion_lost))

print(topic_summary)

# Plot the proportion lost by topic
barplot(topic_summary$proportion_lost,
        names.arg = topic_summary$primaryTopic,
        main = "Proportion of Publications Lost by Topic (when keeping only main component)",
        xlab = "Topic",
        ylab = "Proportion Lost",
        las = 2,
        cex.names = 0.6)

# Not ideal, but it's the data we have. Let's now look at the make up of this component.

# Get the main component from the original bipartite graph G
comp_G <- components(G)
largest_comp_nodes_G <- which(comp_G$membership == which.max(comp_G$csize))
G_main <- induced_subgraph(G, largest_comp_nodes_G)

cat("Main component graph:\n")
cat("Total nodes:", vcount(G_main), "\n")
cat("Authors:", sum(V(G_main)$type == "author"), "\n")
cat("Publications:", sum(V(G_main)$type == "publication"), "\n")
cat("Edges:", ecount(G_main), "\n")

G <- G_main

################################
# SIMPLIFY TOPICS
################################

# First, assign colors to publications based on their primary topics
pub_vertices <- V(G)[V(G)$type == "publication"]
pub_topics <- V(G)$primaryTopic[pub_vertices]

# Create simplified topic categories
V(G)$simplified_topic <- V(G)$primaryTopic

# Combine anatomy topics
V(G)$simplified_topic[grepl("Anatomy", V(G)$primaryTopic, ignore.case = TRUE)] <- "Anatomy"

# Combine biology topics  
V(G)$simplified_topic[grepl("Biology", V(G)$primaryTopic, ignore.case = TRUE)] <- "Biology"

# Combine botany topics
V(G)$simplified_topic[grepl("Botany", V(G)$primaryTopic, ignore.case = TRUE)] <- "Botany"

# Combine chemistry topics
V(G)$simplified_topic[grepl("Chemistry", V(G)$primaryTopic, ignore.case = TRUE)] <- "Chemistry"

# Group administrative/meta content
admin_topics <- c("Headmatter", "Tables", "Reporting", "Formulae")
V(G)$simplified_topic[V(G)$primaryTopic %in% admin_topics] <- "Administrative"

# Get unique simplified topics for publications
pub_simplified_topics <- V(G)$simplified_topic[V(G)$type == "publication"]
unique_simplified_topics <- unique(pub_simplified_topics[!is.na(pub_simplified_topics)])

cat("Simplified topics:", length(unique_simplified_topics), "\n")
print(sort(unique_simplified_topics))

# Get unique publication topics and assign colors to simplified topics
n_simplified_topics <- length(unique_simplified_topics)
simplified_topic_colors <- rainbow(n_simplified_topics)
names(simplified_topic_colors) <- unique_simplified_topics

# Assign colors to publication vertices using simplified topics
V(G)$pub_topic_color <- ifelse(V(G)$type == "publication" & !is.na(V(G)$simplified_topic), 
                               simplified_topic_colors[V(G)$simplified_topic], 
                               "gray")

# Plot 2a: Bipartite graph with publications colored by topic
plot(G, 
     vertex.label = NA,
     vertex.size = 2,
     vertex.color = ifelse(V(G)$type == "author", "lightblue", V(G)$pub_topic_color),
     edge.color = "gray80",
     edge.width = 0.5,
     layout = layout_with_lgl(G),
     #layout = layout_with_graphopt(G, charge = 0.02),
     main = "Bipartite Graph: Authors (blue) and Publications (colored by topic)")

# Add legend for publication topics
legend("topright", 
       legend = c("Authors", unique_simplified_topics), 
       col = c("lightblue", simplified_topic_colors), 
       pch = 19, 
       cex = 0.6)


# Create publication-only network by projecting the bipartite graph
vertex_types <- V(G)$type == "publication"
pub_projection <- bipartite_projection(G, types = vertex_types)
pub_net <- pub_projection$proj2  # proj2 is the publication projection

# Transfer publication attributes
V(pub_net)$simplified_topic <- V(G)$simplified_topic[match(names(V(pub_net)), names(V(G)))]
V(pub_net)$pub_topic_color <- simplified_topic_colors[V(pub_net)$simplified_topic]

# Plot flattened publication network
plot(pub_net,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = V(pub_net)$pub_topic_color,
     edge.color = "gray70",
     edge.width = 0.8,
     #layout = layout_with_fr(pub_net),
     layout = layout_with_lgl(pub_net),
     main = "Publication Network (flattened - colored by topic)")

# Add legend
legend("left", 
       legend = unique_simplified_topics, 
       col = simplified_topic_colors, 
       pch = 19, 
       cex = 0.6)


# We can now see that we're getting clusters of subjects/topics based only on co-authorship
# This is before we've even looked at the language of the texts

###########################
# Community detection
###########################

# Run Louvain community detection on the bipartite graph
communities <- cluster_louvain(G)

# Get community colors
n_communities <- max(communities$membership)
community_colors <- rainbow(n_communities)

# Assign colors to vertices based on community membership
V(G)$community_color <- community_colors[communities$membership]

# Try layout that better preserves community structure
layout_coords <- layout_with_lgl(G)

# Plot graph with community colors using LGL layout
plot(G,
     vertex.label = NA,
     vertex.size = 2,
     vertex.color = V(G)$community_color,
     edge.color = "gray80",
     edge.width = 0.5,
     layout = layout_with_lgl,
     main = "Bipartite Graph with Louvain Communities (LGL layout)")


# Print community summary
cat("Number of communities found:", n_communities, "\n")
cat("Community sizes:\n")
print(sort(table(communities$membership), decreasing = TRUE))

# Create author-only network by dropping publications
vertex_types <- V(G)$type == "author"
author_projection <- bipartite_projection(G, types = vertex_types)
author_net_communities <- author_projection$proj1

# Transfer community membership to author network
V(author_net_communities)$community <- communities$membership[match(names(V(author_net_communities)), names(V(G)))]
V(author_net_communities)$community_color <- community_colors[V(author_net_communities)$community]

# Plot flattened author network with communities
plot(author_net_communities,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = V(author_net_communities)$community_color,
     edge.color = "gray70",
     edge.width = 0.8,
     layout = layout_with_lgl(author_net_communities),
     main = "Author Collaboration Network with Communities (flattened)")


###########################
# Same as above, but on author only netwrok

# Run Louvain community detection on the bipartite graph
communities <- cluster_louvain(author_net_communities)

# Get community colors
n_communities <- max(communities$membership)
community_colors <- rainbow(n_communities)

# Assign colors to vertices based on community membership
V(author_net_communities)$community_color <- community_colors[communities$membership]

# Try layout that better preserves community structure
layout_coords <- layout_with_lgl(author_net_communities)

# Plot graph with community colors using LGL layout
plot(author_net_communities,
     vertex.label = NA,
     vertex.size = 2,
     vertex.color = V(author_net_communities)$community_color,
     edge.color = "gray80",
     edge.width = 0.5,
     layout = layout_coords,
     main = "Bipartite Graph with Louvain Communities (LGL layout)")

# Print community summary
cat("Number of communities found:", n_communities, "\n")
cat("Community sizes:\n")
print(sort(table(communities$membership), decreasing = TRUE))

#################
# What do communities represent?
#################

# Fix community indexing issue

# Get publications and their vertex indices in the full graph
pubs <- V(G)[V(G)$type == "publication"]
pub_indices <- which(V(G)$type == "publication")

# Get topics and communities using the correct indices
topics <- V(G)$simplified_topic[pub_indices]
communities_pub <- communities$membership[pub_indices]

# Check what we have
print(paste("Total pubs:", length(pubs)))
print(paste("Topics with NAs:", sum(is.na(topics))))
print(paste("Communities with NAs:", sum(is.na(communities_pub))))

# Remove NAs
valid <- !is.na(topics) & !is.na(communities_pub)
topics_clean <- topics[valid]
communities_clean <- communities_pub[valid]

print(paste("Valid pairs:", length(topics_clean)))

if(length(topics_clean) > 1) {
  # Convert to numeric
  topic_numeric <- as.numeric(as.factor(topics_clean))
  community_numeric <- as.numeric(communities_clean)
  
  # Calculate correlation
  correlation <- cor(topic_numeric, community_numeric, use = "complete.obs")
  print(paste("Correlation between topic and community:", round(correlation, 3)))
}

###

# Create heatmap of communities vs topics
# Create heatmap of communities vs topics

# Get publications and their vertex indices in the full graph
pubs <- V(G)[V(G)$type == "publication"]
pub_indices <- which(V(G)$type == "publication")

# Get topics and communities using the correct indices
topics <- V(G)$simplified_topic[pub_indices]
communities_pub <- communities$membership[pub_indices]

# Remove NAs
valid <- !is.na(topics) & !is.na(communities_pub)
topics_clean <- topics[valid]
communities_clean <- communities_pub[valid]

# Create contingency table
topic_community_table <- table(topics_clean, communities_clean)

# Create heatmap with proportions
library(gplots)
topic_community_prop <- prop.table(topic_community_table, margin = 2)  # Proportions by community

heatmap.2(as.matrix(topic_community_prop),
          main = "Topic Proportions by Community",
          xlab = "Community",
          ylab = "Topic",
          col = heat.colors(20),
          scale = "none",
          key = TRUE,
          trace = "none",
          keysize = 1.5,
          key.title = NA,
          key.xlab = "Proportion")

print("Contingency table:")
print(topic_community_table)

########

# Create heatmap of communities vs topics

# Get publications and their vertex indices in the full graph
pubs <- V(G)[V(G)$type == "publication"]
pub_indices <- which(V(G)$type == "publication")

# Get topics and communities using the correct indices
topics <- V(G)$simplified_topic[pub_indices]
communities_pub <- communities$membership[pub_indices]

# Remove NAs
valid <- !is.na(topics) & !is.na(communities_pub)
topics_clean <- topics[valid]
communities_clean <- communities_pub[valid]

# Create contingency table
topic_community_table <- table(topics_clean, communities_clean)

# Filter out topics with fewer than 20 publications
topic_counts <- rowSums(topic_community_table)
keep_topics <- names(topic_counts)[topic_counts >= 10]

# Filter out communities with fewer than 20 publications
community_counts <- colSums(topic_community_table)
keep_communities <- names(community_counts)[community_counts >= 10]

# Filter the table for both topics and communities
topic_community_table_filtered <- topic_community_table[keep_topics, keep_communities]

print(paste("Kept", length(keep_topics), "topics out of", nrow(topic_community_table)))
print(paste("Kept", length(keep_communities), "communities out of", ncol(topic_community_table)))
print("Filtered table:")
print(topic_community_table_filtered)

# Create heatmap with proportions
library(gplots)
topic_community_prop <- prop.table(topic_community_table_filtered, margin = 2)  # Proportions by community

heatmap.2(as.matrix(topic_community_prop),
          main = "Topic Proportions by Community",
          xlab = "Community",
          ylab = "Topic",
          col = heat.colors(100),
          scale = "none",
          key = TRUE,
          keysize = 1,
          key.title = NA,
          trace = "none",
          key.xlab = "Proportion")

print("Contingency table:")
print(topic_community_table)
