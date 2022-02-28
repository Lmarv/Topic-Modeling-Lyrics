# script to get song tags from LastFM 
# author: Julia Güttler

import pylast
import pandas as pd

genre = "spiritual"

# path to import the CSV files with artist, album, song name, release date and lyrics info
importpath = "/home/julia/1SemMDH/MethodsApplicationsDH/Projekt/Data/"+genre+".csv"

# path to save the tagged data frame
endpath = "/home/julia/1SemMDH/MethodsApplicationsDH/Projekt/Data/"+genre+"_tagged.csv"

# accepted tags for the songs
acceptedTags = ["blues", "children", "country", "disney", "folk", "gospel", "heavy metal", "hip hop", "hip-hop", "jazz", "kids", "metal", "pixar", "pop", "rap","reggae", "rnb", "r'n'b", "rock", "soul", "spiritual"]

'''
Parameters: df: data frame, must have colums songname and artist
This function takes the names of songs and artists and assigns to each son the highest weighed LastFM tag, which is also in the list acceptedTags
a list with a tag for each song in the data frame df is returned
if there is no (matching) tag in LastFMs tags for the song, "noTag" is the placeholder
'''
def getTrackTags(df):

    # insert LastFM API key ad API secret
    apiKey = ""
    apiSecret = ""

    # initialize LastFM network
    network = pylast.LastFMNetwork(api_key=apiKey, api_secret=apiSecret)

    # load song and artist names from passed data frame
    songnames = df["songname"]
    artistnames = df["artist"]
    if len(songnames) != len(artistnames):
        print("not the same length!")
        return
    
    tags = []

    # loop over every song in the data frame
    for i in range(len(songnames)):

        # get the track from LastFM
        track = network.get_track(artist=artistnames[i], title=songnames[i])
        tagList = []

        # try to get the top tags from LastFM for the song, save them in the list tagList as string
        try:
            topTags = track.get_top_tags()
            for line in topTags:
                tagList.append(str(line.__getnewargs__()[0]))
        except:
            print("get top tags problem")
            tags.append("topTagsError")
            continue

        # if the tag list is empty, the song has not been tagged -> no tag
        finalTag = ""
        if tagList == []:
            finalTag = "noTag"

        # loop over every tag in the tag list
        # selects the first tag that is accepted and assigns it to final tag with some spelling corrections for certain tags
        # break if an accepted tag is found in tag list
        for tag in tagList:
            if tag.lower() in acceptedTags:
                finalTag = tag.lower()
                if finalTag in ["children", "disney", "pixar"]:
                    finalTag = "kids"
                elif finalTag == "hip-hop":
                    finalTag = "hip hop"
                elif finalTag == "metal":
                    finalTag = "heavy metal"
                elif finalTag == "r'n'b":
                    finalTag = "rnb"
                break
        
        # if final tag is still not assigned, there was no accepted tag in the tag list -> no tag
        if finalTag == "":
            finalTag = "noTag"

        # append tag of new song to list tags
        tags.append(finalTag)
    
    # return the list of tags to become new column in data frame 
    return tags


def main():
    # open data frame with song info
    df = pd.read_csv(importpath, sep=";")

    # get tags for each song with function getTrackTags and save them in data frame column "tag"
    tags = getTrackTags(df)
    df["tag"] = tags

    # save new data frame with tag info in endpath
    with open(endpath, "w") as file:
        file.write(df.to_csv(header=True, index=False, sep=";"))
        print("data written into file: " + endpath)

main()