# script to retrieve lists of the top albums and their artists from HTMLs previously scraped from LastFM 
# author: Julia Güttler

from bs4 import BeautifulSoup
import pandas as pd

genre = "pixar"

# path to import the HTMLs from LastFM
path = "/home/julia/1SemMDH/MethodsApplicationsDH/Projekt/Data/HTMLs_LastFM/" + genre + "albums3"

# path to save the lists of albums and artists
endPath = "/home/julia/1SemMDH/MethodsApplicationsDH/Projekt/Data/HTMLs_LastFM/" + genre + "albumstuplesdf3"

albums = []
artists = []

# initialize a BeautifulSoup 
soup = BeautifulSoup(open(path), features="lxml")


# extract album names from h3s in HTML file and store them in the list albums
section = soup.find("section", {"id":"artist-albums-section"})

h3s = section.find_all("h3")

albums = [h3.a.get_text() for h3 in h3s]


# extract artist names from spans in HTML file and store them in the list artists
spans = section.find_all("span")

for span in spans:
    a = span.find_all("a")
    for i in a:
        if i != []:
            artists.append(i.get_text())


# save albums and artists in data frame
df = pd.DataFrame(columns=["albums", "artists"])

if len(albums) != len(artists):
    print("Unequal lengths")
else:
    df["albums"] = albums
    df["artists"] = artists

    with open(endPath, "w") as file:
        file.write(df.to_csv(header=True, index=False, sep=";"))
        print("data written into file: " + endPath)
