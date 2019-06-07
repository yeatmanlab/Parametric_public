#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan  7 14:19:30 2019

@author: eobrien
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 13 09:55:13 2018

@author: eobrien
"""
import hddm
import numpy as np
import pandas

## Load data from csv file into a NumPy structured array
data = hddm.load_csv('/home/eobrien/bde/Projects/Parametric_public/Analysis/Clean_Motion_Data.csv')

# Drop the stim = 100 condition from this analysis, since it behaves oddly
data = data[data.stim != 100]


# Log2 transform and scale the coherence level
data['stim'] = np.log2(data['stim'])
data['stim'] = data['stim'] - np.mean(data['stim'])


# Remove any rows where the reaction time is too fast or too slow
#data = data[data.keep_block == "Keep"] # can't be real
data = data[data.rt < 10] # somewhat arbitrary
data = data[data.rt > 0.2] # too short
subj_params = []
sub_list = []

for subj_idx, subj_data in data.groupby('subj_idx'):
   m_subj_full = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                              include = {'sz','st','sv'},
                                              p_outlier = 0.05)
   m_subj_sz = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                              include = {'sz'},
                                              p_outlier = 0.05)
   m_subj_sv = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                              include = {'sv'},
                                              p_outlier = 0.05)
   m_subj_st = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                              include = {'st'},
                                              p_outlier = 0.05)
   m_subj_st_sv = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                              include = {'st','sv'},
                                              p_outlier = 0.05)
   m_subj_st_sz = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                              include = {'st','sz'},
                                              p_outlier = 0.05)
   m_subj_sv_sz = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                              include = {'sv','sz'},
                                              p_outlier = 0.05)   
   m_subj_no = hddm.HDDM(subj_data, depends_on={'v': ['stim']},
                                                p_outlier = 0.05)
   
   
   # Fit each model
   m_subj_full.optimize('ML')
   m_subj_sz.optimize('ML')
   m_subj_sv.optimize('ML')
   m_subj_st.optimize('ML')
   m_subj_st_sv.optimize('ML')
   m_subj_st_sz.optimize('ML')
   m_subj_sv_sz.optimize('ML')
   m_subj_no.optimize('ML')
   
   out = np.array([m_subj_full.aic, m_subj_full.bic, m_subj_sz.aic, m_subj_sz.bic, m_subj_sv.aic, m_subj_sv.bic, m_subj_st.aic, m_subj_st.bic,
                   m_subj_st_sv.aic, m_subj_st_sv.bic, m_subj_st_sz.aic, m_subj_st_sz.bic, m_subj_sv_sz.aic, m_subj_sv_sz.bic, m_subj_no.aic, m_subj_no.bic])
   
   
   subj_params.append(out)
   print(subj_idx)
   sub_list.append(subj_idx)
params = pandas.DataFrame(subj_params)
params = params.rename(columns = {0: "Full_AIC", 1: "Full_BIC", 2:"sz_AIC", 3:"sz_BIC", 4:"sv_AIC",5:"sv_BIC",6:"st_AIC", 7:"st_BIC",
                                  8:"st_sv_AIC", 9:"st_sv_BIC",10:"st_sz_AIC", 11:"st_sz_BIC",12:"sv_sz_AIC",13:"sv_sz_BIC", 14:"no_AIC",15:"no_BIC"})


# Add the subject list
params = params.assign(subj_idx = sub_list )
# Save parameters from DDM
params.to_csv("./DDM_Fit_AIC_BIC.csv")
