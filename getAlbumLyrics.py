# script to retrieve lyrics of whole albums; album and artist names needed in CSV files (change file path if neccessary)
# author: Julia Güttler

import lyricsgenius as lg
import pandas as pd

genre = "pixar"

# path to import the lists of albums and artists
importPath = "/home/julia/1SemMDH/MethodsApplicationsDH/Projekt/Data/HTMLs_LastFM/" + genre + "albumstuplesdf3"

# path to save the csv file with the lyrics
endPath = "/home/julia/1SemMDH/MethodsApplicationsDH/Projekt/Data/" + genre + ".csv"

'''
Parameters: tuples: a list of tuples (x,y) with x being the album name and y the artist name
            genius: the genius object needed to access the Genius API
This function scrapes the lyrics for each song in each album in the tuples list
song names and release dates are also extracted 
for each song the info about artist, album name, song name, release date, and lyrics is saved in the data frame albumDf
album df is returned
'''
def getAlbumLyrics(tuples, genius):
    # create new data frame to save artist, albumname, release date and lyrics
    albumDf = pd.DataFrame(columns=["artist", "albumname", "songname", "releasedate", "lyrics"])
    
    # loop over list of albumname, artist paris in tuples
    for albumname, artist in tuples:
        album = genius.search_album(albumname, artist)
        if album is None:
            print("album is None")
            continue

        try:
            # try to extract album release date from album info
            releasedate = int(str(album.release_date_components).split("-")[0])

        except ValueError:
            # if there is no info about album release date, set it to 0
            print("Unknown release date")
            releasedate = 0

        # loop over every track in the album
        for track in album.tracks:
            # extract lyrics and song name from track info
            lyrics = track.to_text()
            songname = track.to_dict().get("song").get("title")

            # insert info about new song into the albumDf
            newRow = {"artist":artist, "albumname":albumname, "songname":songname, "releasedate":releasedate, "lyrics":lyrics}
            albumDf = albumDf.append(newRow, ignore_index=True)

    return albumDf

def main():

    # insert genius access token
    token = ""
    # initialize genius object
    genius = lg.Genius(access_token=token, timeout=1000, retries=1)

    #import CSV and create tuples list
    df = pd.read_csv(importPath, sep=";")

    tuples = []
    for i in range(len(df["albums"])):
        tuples.append((list(df["albums"])[i], list(df["artists"])[i]))

    # get lyrics with getAlbumLyrics()
    albumLyrics = getAlbumLyrics(tuples, genius)

    # save data frame in CSV file
    with open(endPath, "a") as file:
        file.write(albumLyrics.to_csv(header=True, index=False, sep=";"))
        print("data written into file: " + endPath)

main()