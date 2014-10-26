# Krishna Mooroogen PhD student.
# krishna.mooroogen@northumbria.ac.uk
# Northumbria University.
# Analysis tools for astronomy using fits files.
# Note may become more speciliased towards Solar astronomy.
# Include Iris spectra code and refactor.

import sys
import pyfits
import matplotlib.pyplot as plt
import matplotlib.cm as cmm
import pylab
import itertools
import sunpy
from sunpy.cm import cm
import numpy as np


def open_fits(file,info=0) :
    #Opens fits file.
    #Use optional argument string 'info' to display data info and load header.
    #If header is loaded then output will be data array and header.

    data = pyfits.open(file)[0]

    array = data.data
    header=None
    
    if info==1:

        print 'Data shape: ',array.shape
    
        try:
    
            header = data.header
            print 'Header loaded.'
                
        except:
    
            print 'No header found.'

    if header==None:

        return array

    else:

        return array,header

def colors(type='sunpy') :
    # prints color tables.
    # kewword args can equal 'sunpy' color or matplot.
    
    if type=='sunpy':
    
        cm.show_colormaps()
    
    elif type=='matplot':

        cmaps = [('Sequential',['Blues', 'BuGn', 'BuPu','GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd','PuBu', 'PuBuGn','PuRd', 'Purples', 'RdPu','Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd']),
             
         ('Sequential (2)', ['afmhot', 'autumn', 'bone', 'cool', 'copper',
                             'gist_heat', 'gray', 'hot', 'pink',
                             'spring', 'summer', 'winter']),
         ('Diverging',      ['BrBG', 'bwr', 'coolwarm', 'PiYG', 'PRGn', 'PuOr',
                             'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
                             'seismic']),
         ('Qualitative',    ['Accent', 'Dark2', 'Paired', 'Pastel1',
                             'Pastel2', 'Set1', 'Set2', 'Set3']),
         ('Miscellaneous',  ['gist_earth', 'terrain', 'ocean', 'gist_stern',
                             'brg', 'CMRmap', 'cubehelix',
                             'gnuplot', 'gnuplot2', 'gist_ncar',
                             'nipy_spectral', 'jet', 'rainbow',
                             'gist_rainbow', 'hsv', 'flag', 'prism'])]



        nrows = max(len(cmap_list) for cmap_category, cmap_list in cmaps)
        gradient = np.linspace(0, 1, 256)
        gradient = np.vstack((gradient, gradient))

        for cmap_category, cmap_list in cmaps:

            fig, axes = plt.subplots(nrows=nrows)
            fig.subplots_adjust(top=0.95, bottom=0.01, left=0.2, right=0.99)
            axes[0].set_title(cmap_category + ' colormaps', fontsize=14)
    
            for ax, name in itertools.izip(axes, cmap_list):
                ax.imshow(gradient, aspect='auto', cmap=plt.get_cmap(name))
                pos = list(ax.get_position().bounds)
                x_text = pos[0] - 0.01
                y_text = pos[1] + pos[3]/2.
                fig.text(x_text, y_text, name, va='center', ha='right', fontsize=10)
    
            # Turn off *all* ticks & spines, not just the ones with colormaps.
            for ax in axes:
                ax.set_axis_off()


            plt.show()

def show_im(data,index=0,crop=(None,None,None,None),col=('Greys_r',0)) :
    
    # Plots image of the data with correct orientation
    # Colors can be chnaged using keyword argument col as tuple (c,i)
    # where i is 1 or 0 for sunpy or matplotlib library respectively.
    # if color string does not match library default is used, currently no exception for this.
    
    
    c,i=col
    
    
    x1,x2,y1,y2 = crop
    
    if x1 == None:
    
        scale=[0,data[index].shape[1],0,data[index].shape[0]]
    
    else:
        scale=[x1,x2,y1,y2]
    
    
    if i==1:

        color = cm.cmlist.get(c)

    elif i ==0:
    
        color = getattr(cmm,c)
        
        
    pylab.imshow(np.flipud(data[index][y1:y2,x1:x2]),cmap=color,extent=scale)


def save(filename='image.png') :
    
    
    if "." in filename:
        param, value = filename.split(".",1)
    else:
        print 'No file type specified'
        sys.exit()
    
    plt.savefig(filename,format=value,dpi=1000)


def cross_cut(data,cood,index=0) :
    # Use whole numbers for data points.
    
    for i in cood:
        if type(i)==float in cood:
            print 'Please use integer coordiates.'
            sys.exit()

    #length = abs(sqrt((cood[1]-cood[0])**2+(cood[3]-cood[2])**2))

    grad = float((cood[3]-cood[1]))/float((cood[2]-cood[0]))

    inter = float(-cood[0])*grad+cood[1]

    if cood[0]>cood[2]:

        x=np.arange(cood[2],cood[0],1)+1

    elif cood[2]>cood[0]:
        
        x=np.arange(cood[0],cood[2],1)+1
    
    y=grad*x+inter

    plt.plot(x,y)

    cc=[]
    
    for i,j in itertools.izip(x,y):
        cc.append(data[index][i][j])


    return cc

def fit():
    pass
def time_series():
    pass
def power_spectra():
    pass

