
import glob
import pandas as pd
from systole.detection import ppg_peaks
from systole.plots import (plot_raw, plot_rr)
#from systole.viewer import Viewer
from systole.hrv import (time_domain, frequency_domain)
from systole.correction import correct_rr 
from systole.utils import (input_conversion, get_valid_segments)
from bokeh.plotting import show


import os as os
from os import listdir
from os.path import isfile, join
os.chdir('xx')

files = glob.glob('*PPG.csv')


for l in range(0,x): 
    
    signal = pd.read_csv(files[l], index_col=(0))['Preprocessed.PPG']
    
    #plot_raw(signal, sfreq = 75)
            
    show(
        plot_raw(signal, sfreq = 75, backend = 'bokeh')
        )
    
    
    ppg_pk, peaks = ppg_peaks(signal, sfreq = 75, clipping = False)
    
    rr_ms = input_conversion(peaks, input_type = 'peaks', output_type='rr_ms')
    
   # show( 
    #    plot_rr(rr_ms, input_type = "rr_ms", show_artefacts = True, line = False, backend = "bokeh")
    #    )
    
    corrected_rr, _ = correct_rr(rr_ms)
        
    time_df = time_domain(rr = corrected_rr, input_type = 'rr_ms')
    freq_df = frequency_domain(rr = corrected_rr, input_type = 'rr_ms')
    
    freq_wide = pd.pivot_table(freq_df, values = 'Values', columns = 'Metric')
    time_wide = pd.pivot_table(time_df, values = 'Values', columns = 'Metric')
    
    time_wide = time_wide.merge(freq_wide, left_index=True, right_index=True)
    time_wide['p_id'] = files[l]
    
    time_wide.to_csv(f"D:\PhD\Two Arm Biofeedback\TABio Python Outputs\{files[l]}")

    



