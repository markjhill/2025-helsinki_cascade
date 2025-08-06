import pandas as pd
import networkx as nx
from collections import Counter

################################
# Load data

author_df = pd.read_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_author_nodes.csv')
publication_df  = pd.read_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_publication_nodes.csv')
edges_df = pd.read_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_edges.csv')

# Convert DataFrames to lists of dictionaries
author_nodes = author_df.to_dict('records')
publication_nodes = publication_df.to_dict('records')
edges = edges_df.to_dict('records')

print(f"Converted {len(author_nodes)} author nodes")
print(f"Converted {len(publication_nodes)} publication nodes") 
print(f"Converted {len(edges)} edges")

################################
# Load the networkx graph

# Create bipartite graph
G = nx.Graph()

# Add nodes with attributes
for node in author_nodes:
    G.add_node(node['id'], **{k:v for k,v in node.items() if k != 'id'})

for node in publication_nodes:
    G.add_node(node['id'], **{k:v for k,v in node.items() if k != 'id'})

# Add edges
for edge in edges:
    G.add_edge(edge['source'], edge['target'], **{k:v for k,v in edge.items() if k not in ['source', 'target']})

################################
# Explore

print(f"Total nodes: {G.number_of_nodes()}")
print(f"Total edges: {G.number_of_edges()}")
print(f"Is connected: {nx.is_connected(G)}")
print(f"Number of components: {nx.number_connected_components(G)}")

# Check node types
author_nodes_in_graph = [n for n, d in G.nodes(data=True) if d.get('type') == 'author']
pub_nodes_in_graph = [n for n, d in G.nodes(data=True) if d.get('type') == 'publication']

print(f"Author nodes: {len(author_nodes_in_graph)}")
print(f"Publication nodes: {len(pub_nodes_in_graph)}")

# Get document types
doc_types = [G.nodes[n]['doc_type'] for n in pub_nodes_in_graph]
doc_type_counts = Counter(doc_types)
print(f"\nDocument types in network: {doc_type_counts}")




# Basic degree statistics
degrees = [G.degree(n) for n in G.nodes()]
print(f"\nDegree statistics:")
print(f"Average degree: {sum(degrees)/len(degrees):.2f}")
print(f"Max degree: {max(degrees)}")
print(f"Min degree: {min(degrees)}")

# Explore connections

# Get degree for author nodes only
author_degrees = [(n, G.degree(n)) for n in author_nodes_in_graph]
author_degrees.sort(key=lambda x: x[1], reverse=True)

print("Most connected authors (top 10):")
for author, degree in author_degrees[:10]:
    print(f"{author}: {degree} connections")

# Check what document types the most connected author publishes
top_author = author_degrees[0][0]
connected_pubs = list(G.neighbors(top_author))
connected_doc_types = [G.nodes[pub]['doc_type'] for pub in connected_pubs]
print(f"\n{top_author} publishes:")
print(Counter(connected_doc_types))
