from dataclasses import replace
from os import sep
import pandas as pd 

'''Read in tagged_corpus.csv as data frame'''
df = pd.read_csv('tagged_corpus.csv', sep= ";")

'''Iterrating of lyrics column, if line in lyrics starts with [ the line will be deleted.'''
for i in range(df.shape[0]):
    if type(df.loc[i, "lyrics"]) != float:
        if '['in df.loc[i, "lyrics"]:
            oldlyrics = df.loc[i, "lyrics"].split('\n')
            newlyrics = ""
            for row in oldlyrics:
                if '[' not in row:
                    newlyrics = newlyrics + '\n'+ row
            df.loc[i, "lyrics"] = newlyrics

'''Iterrating of lyrics column, if line in lyrics starts with 'Lyrics' the line will be splitted into two lines.
The second line will be the first one, after that step. Step was necessary to delete song information 
on the first line.'''
for i in range(df.shape[0]):
    if type(df.loc[i, "lyrics"]) != float:
        if 'Lyrics'in df.loc[i, "lyrics"]:
            oldlyrics = df.loc[i, "lyrics"].split('\n')
            newlyrics = ""
            for row in oldlyrics:
                if 'Lyrics' in row:
                    splitrow =row.split('Lyrics')
                    row=splitrow[1]

                if 'Lyrics' not in row:
                    newlyrics = '"' + newlyrics + '\n'+ row
        
            df.loc[i, "lyrics"] = newlyrics
    
    
'''Preprocessed corpus stored in new CSV file.'''
df.to_csv(r'tagged_corpus_preprocessed.csv', sep = ';')