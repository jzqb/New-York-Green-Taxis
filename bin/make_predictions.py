#!/usr/bin/env python
"""
Building Models to predict tip percentage
"""
__author__ = "jzqb"
__copyright__ = "None"
__credits__ = ["jzqb"]
__license__ = "GPL"
__version__ = "1.0.0"
__maintainer__ = "jzqb"
__email__ = "gaurav.singh@columbia.edu"
__status__ = "Prototype"

import os
import re
import pandas as pd
import numpy as np
import pickle
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV

os.chdir("...YOUR WORKING DIRECTORY HERE...")
model_data = pd.read_csv("model_data.csv")
factor_dummies = pd.get_dummies(model_data[["VendorID", "Store_and_fwd_flag", "RateCodeID", "Payment_type", "Trip_type", "pick_up_weekday"]], drop_first = True)
model_data = model_data[model_data.columns.difference(["VendorID", "Store_and_fwd_flag", "RateCodeID", "Payment_type", "Trip_type", "pick_up_weekday"])]
model_data = pd.concat([model_data, factor_dummies], axis = 1)
write_file_path = "report_data/rf_clf.txt"
with open(write_file_path, "rb") as fp:
    rf_gs = pickle.load(fp)
predicted_vals = rf_gs.best_estimator_.predict(model_data.ix[:, model_data.columns != 'tip_perc'])
results = pd.DataFrame({'true_value':model_data['tip_perc'], 'predicted_values':predicted_vals})
results.to_csv("results.csv", index = False)