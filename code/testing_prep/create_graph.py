import pandas as pd

# Create graph of authors

df = pd.read_csv("C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\Royal_Society_Corpus_open_v6.0_meta.tsv", sep = '\t')

#df_orig = df

df[df['author'].notna()]['author'].head(10)

#df = df[df['century'] == 1700]

df['author_list'] = df['author'].str.split('|')

print(f"Papers: {len(df)}")
print(f"Papers with authors: {df['author_list'].notna().sum()}")
df['author_list'].head()

# Look at a few multi-author papers to see if we need to strip whitespace
multi_author = df[df['author_list'].str.len() > 1]['author_list'].head()
print(multi_author)

# Flatten all author lists into one big list
all_authors = []
for author_list in df['author_list'].dropna():
    all_authors.extend(author_list)

# Get unique authors
unique_authors = list(set(all_authors))
print(f"Total unique authors: {len(unique_authors)}")

author_nodes = []
for author in unique_authors:
    author_nodes.append({
        'id': author,
        'type': 'author'
    })

# Create pub nodes

pub_cols = ['id', 'title', 'year', 'decade', 'volume', 'pages', 'sentences', 
            'tokens', 'journal', 'type', 'primaryTopic', 'primaryTopicPercentage', 
            'secondaryTopic', 'secondaryTopicPercentage']

publication_nodes = df[pub_cols].to_dict('records')

# type is used for type of node (i.e., author or publication) for the graph, so this needs to be renamed
publication_nodes = df[pub_cols].rename(columns={'type': 'doc_type'}).to_dict('records')

# Add node type
for node in publication_nodes:
    node['type'] = 'publication'

print(f"Author nodes: {len(author_nodes)}")
print("Sample author node:", author_nodes[0])
print()
print(f"Publication nodes: {len(publication_nodes)}")
print("Sample publication node:", publication_nodes[0])

# Create edge list

edges = []
for _, row in df[df['author_list'].notna()].iterrows():
    publication_id = row['id']
    for author in row['author_list']:
        edges.append({
            'source': author,
            'target': publication_id,
            'type': 'authored'
        })

print(f"Total edges: {len(edges)}")
print("Sample edges:", edges[:3])

################################
# Save data

# Convert node lists to DataFrames
author_df = pd.DataFrame(author_nodes)
publication_df = pd.DataFrame(publication_nodes)
edges_df = pd.DataFrame(edges)

print("Author nodes:", len(author_df))
print("Publication nodes:", len(publication_df))
print("Edges:", len(edges_df))

author_df.to_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_author_nodes.csv', index=False)
publication_df.to_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_publication_nodes.csv', index=False)
edges_df.to_csv('C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\rsc_edges.csv', index=False)



