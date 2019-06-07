#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 13 09:55:13 2018

@author: eobrien
"""
import hddm
import numpy as np
import pandas
import os
## Load data from csv file into a NumPy structured array
data = hddm.load_csv('Clean_Motion_Data.csv')

# Drop the stim = 100 condition from this analysis, since it behaves oddly
data = data[data.stim != 100]


# Log2 transform and scale the coherence level
data['stim'] = np.log2(data['stim'])
data['stim'] = data['stim'] - np.mean(data['stim'])


# Remove any rows where the reaction time is too fast or too slow
#data = data[data.keep_block == "Keep"] # can't be real
data = data[data.rt < 10] # somewhat arbitrary
data = data[data.rt > 0.2] # too short

# Find out which subjects we have already fit
fit_list = os.listdir("../Motion/Exp/Data/Processed/Fit_Files")
already_fit = []
for file in fit_list:
    tmp = int(file.split("_")[-1].split(".")[0])
    already_fit.append(tmp)


subj_params = []
sub_list = []
for subj_idx, subj_data in data.groupby('subj_idx'):
    if not (subj_idx in already_fit):
       m_subj = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                                  include = {'st','sv','sz'},
                                                  p_outlier = 0.05)
       subj_params = m_subj.optimize('ML', n_runs = 5)
       print(subj_idx)
       this_subj = subj_idx
       params = pandas.DataFrame([subj_params])
    
        # Add the subject list
       params = params.assign(subj_idx = this_subj )
        # Save parameters from DDM
       fid = "fit_" + str(subj_idx) + ".csv"
       params.to_csv("../Motion/Exp/Data/Processed/Fit_Files/" + fid)
