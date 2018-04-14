#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 14 10:34:10 2018

@author: sleek_eagle
"""

import csv
import pandas as pd
import numpy as np

df = pd.read_csv('data.csv')
#list column names
list(df.columns.values)
#number of rows and columns
df.shape

#unique objectAtributeID
df.ObjectAttributeID.unique().shape


