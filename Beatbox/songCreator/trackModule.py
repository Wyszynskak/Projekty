
"""
This module includes all functions needed to read track files and creating 
the track from samples.

@author: Karolina Wyszynska
"""
import numpy as np
import os

def loadTrack(track_name, song_name):
    """
    Function read a track marix from file track_name and returns a vector
    with music function values - it can be listened after saving as .wav file.
    Attention: function reads sample files which numbers are included in 
    the track matrix. Make sure they are available to read!
    """
    assert os.path.isfile(song_name+"/"+track_name)==True
    track=np.genfromtxt(song_name+"/"+track_name, 
                        dtype=str, delimiter="\t")
    tmp=track.ravel()[track.ravel()!='-']
    sample=set(tmp.tolist())
    sample=list(sample)
    
    import warnings
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        list1=[readSoundFile(song_name+'/sample'+i+'.wav') for i in sample]
    
    list2=[flatSound(y) for (fs,y) in list1]
    l=np.array([len(i) for i in list2]).max()
    t=np.zeros(np.shape(track)[0]*44100+l-44100)
    j=0
    for row in track:
        which=[i in row for i in sample]
        z=0
        for i in which:
            if i==True: t[j*44100:(j*44100+len(list2[z]))]+=list2[z]
            z+=1
        j+=1    
    t=t[0:np.shape(track)[0]*44100+1]
    
    return t
    
def readSoundFile(file_name):
    """
    Function checks is a sund file exists and then read it into a sound_vector.
    """
    import scipy.fftpack
    import scipy.io.wavfile
    import os.path
    assert os.path.isfile(file_name)==True
    
    return scipy.io.wavfile.read(file_name)
    
def flatSound(sound_array):
    """
    Function takes a sound_array and flattens it to a sound_vector using 
    mean() function.
    """
    if len(sound_array.shape)==1: return sound_array
    else: return np.mean(sound_array, axis=1)
