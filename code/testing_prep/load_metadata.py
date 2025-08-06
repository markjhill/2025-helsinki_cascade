import pandas as pd
import os

# Basic TSV loading
df = pd.read_csv("C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\Royal_Society_Corpus_open_v6.0_meta.tsv", sep = '\t')

df = pd.read_csv(r"C:\\Users\\markj\\onedrive_cloud\\coding\\GitHub\\2025-helsinki_cascade\\data\\rsc\\Royal_Society_Corpus_open_v6.0_meta.tsv", 
                 sep='\t', 
                 lineterminator='\n')

# Royal Society Corpus - Exploratory Data Analysis
# Simple, flexible code snippets for actual exploration

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

# ==========================================
# EXPLORATION TIPS
# ==========================================

# Always try:
# 1. Different time windows: df[df['year'].between(1850, 1950)]
# 2. Different subsets: df[df['type'] == 'article']
# 3. Different groupings: groupby(['decade', 'type']) vs groupby('year')
# 4. Different plot types: .plot(), .plot(kind='bar'), .hist(), .boxplot()
# 5. Different statistics: .mean(), .median(), .std(), .quantile([0.25, 0.75])

# Useful patterns:
# df.query("year > 1900 and pages < 20")  # Clean query syntax
# df.nlargest(10, 'tokens')  # Longest documents
# df.nsmallest(10, 'pages')   # Shortest documents
# df.sample(n=100)            # Random sample for quick checks

# ==========================================
# BASIC EXPLORATION - Start here
# ==========================================

# Quick overview
df.shape
df.columns
df.dtypes
df.describe()

# What's actually in this data?
df.head(3)
df.sample(5)  # Random sample often more revealing than head()

# ==========================================
# TEMPORAL PATTERNS - Play with different groupings
# ==========================================

# Publications by year - try different periods
df['year'].value_counts().sort_index().plot()
plt.clf()  # Clear the current figure
df['decade'].value_counts().sort_index().plot(kind='bar')
plt.clf()  # Clear the current figure
df['century'].value_counts().plot(kind='bar')
plt.clf()  # Clear the current figure

# Try different time windows
df.groupby('period')['title'].count().plot()  # 50-year periods
plt.clf()  # Clear the current figure
df[df['year'] > 1800]['year'].value_counts().sort_index().plot()  # Just modern period
plt.clf()  # Clear the current figure

# What types of documents over time?
pd.crosstab(df['decade'], df['type']).plot(kind='bar', stacked=True)
plt.clf()  # Clear the current figure
pd.crosstab(df['decade'], df['type'], normalize='index').plot(kind='bar', stacked=True)  # Proportions
plt.clf()  # Clear the current figure

# Document length over time - experiment with different metrics
df.groupby('decade')['pages'].mean().plot(marker='o', title='Average Pages by Decade')
plt.clf()  # Clear the current figure
df.groupby('decade')['tokens'].median().plot(marker='s', title='Median Tokens by Decade')
plt.clf()  # Clear the current figure
df.groupby('year')['sentences'].mean().rolling(10).mean().plot(title='Sentences (10-year rolling avg)')
plt.clf()  # Clear the current figure

# ==========================================
# CONTENT & TOPICS - Flexible topic exploration
# ==========================================

# What topics exist?
df['primaryTopic'].value_counts()
df['primaryTopic'].value_counts().head(10).plot(kind='barh')
plt.clf()  # Clear the current figure

# Try different numbers of topics
df['primaryTopic'].value_counts().tail(20)  # Rare topics
df['secondaryTopic'].value_counts().head(15)

# Topic confidence - play with thresholds
df['primaryTopicPercentage'].describe()
df['primaryTopicPercentage'].hist(bins=30)
df[df['primaryTopicPercentage'] > 80]['primaryTopic'].value_counts()  # High confidence topics
df[df['primaryTopicPercentage'] < 30]['primaryTopic'].value_counts()  # Low confidence - might be interesting!

# Topics over time - try different topics
topic = 'Optics'  # Change this!
topic_over_time = df[df['primaryTopic'] == topic]['decade'].value_counts().sort_index()
topic_over_time.plot(title=f'{topic} over time')


# Compare topics
plt.clf()  # Clear the current figure
topics_to_compare = ['Optics', 'Geography', 'Electricity']  # Modify as needed
for topic in topics_to_compare:
    df[df['primaryTopic'] == topic]['decade'].value_counts().sort_index().plot(label=topic, alpha=0.7)
plt.legend()

# ==========================================
# DOCUMENT CHARACTERISTICS - Experiment with different views
# ==========================================

# Length distributions - try different cuts
plt.clf()  # Clear the current figure
df['pages'].hist(bins=50)
plt.clf()  # Clear the current figure
df[df['pages'] < 50]['pages'].hist(bins=30)  # Remove extreme outliers
plt.clf()  # Clear the current figure
df['tokens'].hist(bins=50, alpha=0.7)

# By document type
plt.clf()  # Clear the current figure
df.boxplot('pages', by='type')
plt.clf()  # Clear the current figure
df.boxplot('tokens', by='type')

df.groupby('type')['pages'].describe()

# Relationships between variables - try different pairs
plt.clf()  # Clear the current figure
df.plot.scatter('pages', 'tokens', alpha=0.5)
plt.clf()  # Clear the current figure
df.plot.scatter('pages', 'sentences', alpha=0.5)
plt.clf()  # Clear the current figure
df.plot.scatter('sentences', 'tokens', alpha=0.5)

# Correlations - which variables relate?
numeric_cols = ['year', 'pages', 'sentences', 'tokens', 'primaryTopicPercentage']
df[numeric_cols].corr()
plt.clf()  # Clear the current figure
sns.heatmap(df[numeric_cols].corr(), annot=True)

# ==========================================
# MISSING DATA PATTERNS - What's missing and why?
# ==========================================

# Simple missing data check
df.isnull().sum()
df.isnull().sum() / len(df) * 100  # As percentages

# Missing data by time period - might reveal data collection issues
missing_by_decade = df.groupby('decade').apply(lambda x: x.isnull().sum())
plt.clf()  # Clear the current figure
missing_by_decade['author'].plot(title='Missing Authors by Decade')
plt.clf()  # Clear the current figure
missing_by_decade['doi'].plot(title='Missing DOIs by Decade')

# What's missing together?
df[['author', 'doi', 'jstorLink', 'language']].isnull().sum()

# Documents with no author info - what are they?
no_author = df[df['author'].isnull()]
no_author['type'].value_counts()
no_author['decade'].value_counts()

# ==========================================
# AUTHORSHIP - Simple exploration of who wrote what
# ==========================================

# Basic author patterns
df['author'].notna().sum()  # How many have author info?

# Simple author count (assumes | separator)
df_with_authors = df[df['author'].notna()].copy()
df_with_authors['n_authors'] = df_with_authors['author'].str.count('\|') + 1
df_with_authors['n_authors'].value_counts().sort_index()
df_with_authors['n_authors'].hist(bins=20)

# Collaboration over time
df_with_authors.groupby('decade')['n_authors'].mean().plot(marker='o', title='Average authors per paper')
df_with_authors.groupby('decade')['n_authors'].median().plot(marker='s', title='Median authors per paper')

# Single vs multi-author by topic
collab_topic = df_with_authors.groupby('primaryTopic')['n_authors'].mean().sort_values(ascending=False)
collab_topic.head(10).plot(kind='barh', title='Most collaborative topics')

# ==========================================
# JOURNALS & PUBLICATION PATTERNS
# ==========================================

# What journals are in here?
df['journal'].value_counts()
df['jrnl'].value_counts()  # Abbreviated journal names

# Journal patterns over time
df['journal'].value_counts().head(5)  # Top journals
main_journal = df['journal'].value_counts().index[0]
df[df['journal'] == main_journal]['decade'].value_counts().sort_index().plot(title=f'{main_journal} over time')

# Volume numbers over time - shows publication frequency
df.plot.scatter('year', 'volume', alpha=0.5)
df.groupby('year')['volume'].max().plot(title='Max volume per year')

# ==========================================
# QUALITY & COMPLETENESS PATTERNS
# ==========================================

# DOI patterns - when did they start?
df[df['doi'].notna()]['year'].min()  # First DOI year
df.groupby('decade')['doi'].apply(lambda x: x.notna().sum()).plot(title='DOIs by decade')

# JSTOR availability
df.groupby('decade')['jstorLink'].apply(lambda x: x.notna().sum()).plot(title='JSTOR links by decade')

# Language info - when available?
df[df['language'].notna()]['decade'].value_counts().sort_index().plot(title='Language info by decade')
df['language'].value_counts()

# ==========================================
# SIMPLE COMPARATIVE ANALYSIS
# ==========================================

# Compare different time periods
early = df[df['year'] < 1800]
late = df[df['year'] >= 1800]

print("Early period (pre-1800):")
print(f"  Average pages: {early['pages'].mean():.1f}")
print(f"  Average tokens: {early['tokens'].mean():.0f}")
print(f"  Most common topic: {early['primaryTopic'].mode()[0]}")

print("\nLate period (1800+):")
print(f"  Average pages: {late['pages'].mean():.1f}")
print(f"  Average tokens: {late['tokens'].mean():.0f}")
print(f"  Most common topic: {late['primaryTopic'].mode()[0]}")

# Compare document types
for doc_type in df['type'].unique():
    subset = df[df['type'] == doc_type]
    print(f"\n{doc_type}s:")
    print(f"  Count: {len(subset)}")
    print(f"  Avg length: {subset['pages'].mean():.1f} pages")
    print(f"  Common topics: {list(subset['primaryTopic'].value_counts().head(3).index)}")

