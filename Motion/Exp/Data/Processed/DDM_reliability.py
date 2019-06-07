#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 12 15:00:02 2018

@author: eobrien
"""

import hddm
import pandas
import numpy as np

## Load data from csv file into a NumPy structured array
data = hddm.load_csv('/home/eobrien/bde/Projects/Parametric_public/Analysis/Clean_Motion_Data.csv')


# Remove any rows where the reaction time is too fast or too slow
#data = data[data.keep_block == "Keep"] # can't be real
data = data[data.rt < 10] # somewhat arbitrary
data = data[data.rt > 0.2] # too short
subj_params = []
sub_list = []

# Drop the stim = 100 condition from this analysis, since it behaves oddly
data = data[data.stim != 100]


# Do a 50-50 split on the data by assigning random 0s/1s to each row of the dataframe
for subj_idx, subj_data in data.groupby('subj_idx'):
    subj_data['split'] = np.random.randint(0, 2, subj_data.shape[0])
    for s in range(0,2):
        subj_split = subj_data[subj_data.split == s]
        m_subj = hddm.HDDM(subj_split, depends_on={'v': ['stim']},
                                             include = ("sv","st","sz"),
                                             p_outlier = 0.05)
        out_params = m_subj.optimize('ML', n_runs = 5)
        out_params['run'] = s
        subj_params.append(out_params)
        print(subj_idx, s)
        sub_list.append(subj_idx)
params = pandas.DataFrame(subj_params)


# Add the subject list
params = params.assign(subj_idx = sub_list )
# Save parameters from DDM
params.to_csv("/home/eobrien/bde/Projects/Parametric_public/Motion/Exp/Data/Processed/DDM_Fit_reliability.csv")

