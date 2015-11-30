#!/home/kkk/anaconda3/bin/python3


import sys
song_name=sys.argv[1]

import zipfile
import os
if zipfile.is_zipfile(song_name):
    zip_ref = zipfile.ZipFile(song_name, 'r')
    zip_ref.extractall(os.getcwd())
    zip_ref.close()
    import re
    z=re.compile('[^.]+')
    song_name=z.findall(song_name)[0]

import songCreator
song=songCreator.loadSong(song_name)
songCreator.saveSong(song,song_name)


