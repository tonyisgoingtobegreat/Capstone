#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# import necessary libraries

# data processding
import pandas as pd
import numpy as np
import dask.dataframe as dd
import shap
import imblearn
from imblearn.over_sampling import RandomOverSampler

#sklearn
from sklearn.preprocessing import StandardScaler, RobustScaler, FunctionTransformer
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_val_score
from sklearn.metrics import (roc_auc_score, confusion_matrix,
                             accuracy_score, roc_curve,
                             precision_recall_curve, f1_score,classification_report)
# plot
import matplotlib.pyplot as plt
import seaborn as sns
from pandas.plotting import scatter_matrix

