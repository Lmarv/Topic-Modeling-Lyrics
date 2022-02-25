from dataclasses import replace
from os import sep
import pandas as pd 

df = pd.read_csv('C:\\Users\LWag9\Documents\R\lyrics_topic_modeling\\tagged_corpus_preprocessed.csv', sep= ";")

for i in range(df.shape[0]):
    if type(df.loc[i, "lyrics"]) != float:
        if '['in df.loc[i, "lyrics"]:
            oldlyrics = df.loc[i, "lyrics"].split('\n')
            newlyrics = ""
            for row in oldlyrics:
                if '[' not in row:
                    newlyrics = newlyrics + '\n'+ row
            df.loc[i, "lyrics"] = newlyrics

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
                    newlyrics = newlyrics + '\n'+ row
        
            df.loc[i, "lyrics"] = newlyrics
    
    

df.to_csv(r'C:\Users\LWag9\Documents\R\lyrics_topic_modeling\tagged_corpus_preprocessed.csv', sep = ';')