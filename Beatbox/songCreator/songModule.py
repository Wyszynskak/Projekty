"""
Module includes all of functions needed to combine tracks to 
a song and saving it as a .wav file.

@author: Karolina Wyszynska
"""
import numpy as np
from . import trackModule
def loadSong(song_name):
    """
    Function read a song vector from file song_name and returns a vector
    with music function values - it can be listened after saving as .wav file.
    Attention: function reads sample files which numbers are included in all 
    the track matrixes used in the song. Make sure they are available to read!
    """
    song=np.genfromtxt(song_name+'/song.txt', dtype=str, delimiter="\t")

    sample=set(song.tolist())
    sample=list(sample)
    list1=[trackModule.loadTrack('track'+i+'.txt',song_name) for i in sample]

    song2=np.array([0])

    for row in song:
        which=[i in row for i in sample]
        z=0;
        for i in which:
            if i: song2=np.r_[song2,list1[z]]
            z+=1
    
    return song2
    
def saveSong(song_vector,song_name):
    """
    Function takes a song written as a vector and saves 
    it as a .wav file with a name "song_name.wav"
    """
    tmp=open(song_name+'/defs.txt','r').read()
    tmp=eval(tmp)
    bmp=tmp["bmp"]
    import scipy.io.wavfile
    scipy.io.wavfile.write(song_name+".wav",int(44100*bmp/60), 
                           np.int16(song_vector/max(
                           np.abs(song_vector))*32767))
                           
