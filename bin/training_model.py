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
os.chdir("/Users/gauravsingh/Desktop/drive/dsi/spring-18/internship/capone/capone_ds_challenge/")
model_data = pd.read_csv("model_data.csv")
factor_dummies = pd.get_dummies(model_data[["VendorID", "Store_and_fwd_flag", "RateCodeID", "Payment_type", "Trip_type", "pick_up_weekday"]], drop_first = True)
model_data = model_data[model_data.columns.difference(["VendorID", "Store_and_fwd_flag", "RateCodeID", "Payment_type", "Trip_type", "pick_up_weekday"])]
model_data = pd.concat([model_data, factor_dummies], axis = 1)
train_data, test_data = train_test_split(model_data, test_size=0.5)

lr = LinearRegression().fit(train_data.ix[:, train_data.columns != 'tip_perc'], train_data['tip_perc'])
train_predicted = lr.predict(train_data.ix[:, train_data.columns != 'tip_perc'])
train_rmse_lr = np.sqrt(sum((train_predicted - train_data['tip_perc'])**2)/train_data.shape[0])
test_predicted = lr.predict(test_data.ix[:, test_data.columns != 'tip_perc'])
test_rmse_lr = np.sqrt(sum((test_predicted - test_data['tip_perc'])**2)/test_data.shape[0])

rf_regr = RandomForestRegressor(n_jobs=-1, random_state = 42)
rf_parameters = {
        "n_estimators": (15, 20, 25),
        "max_depth": (100, 200, 300)
        }
rf_gs = GridSearchCV(rf_regr, rf_parameters, n_jobs= -1)
rf_gs = rf_gs.fit(train_data.ix[:, train_data.columns != 'tip_perc'], train_data['tip_perc'])

write_file_path = "rf_clf_2.txt"
with open(write_file_path, "wb") as fp:
    pickle.dump(rf_gs, fp)

write_file_path = "rf_clf.txt"
with open(write_file_path, "rb") as fp:
    rf_gs = pickle.load(fp)
train_predicted = rf_gs.best_estimator_.predict(train_data.ix[:, train_data.columns != 'tip_perc'])
train_rmse_rf = np.sqrt(sum((train_predicted - train_data['tip_perc'])**2)/train_data.shape[0])
test_predicted = rf_gs.best_estimator_.predict(test_data.ix[:, test_data.columns != 'tip_perc'])
test_rmse_rf = np.sqrt(sum((test_predicted - test_data['tip_perc'])**2)/test_data.shape[0])
train_benchmark_rmse = np.sqrt(sum((train_predicted - np.mean(train_data['tip_perc']))**2)/train_data.shape[0])
test_benchmark_rmse = np.sqrt(sum((test_predicted - np.mean(test_data['tip_perc']))**2)/test_data.shape[0])


n_estimator = [10, 15, 20]
max_depth = [5, 10, 15]
scores = [x[1] for x in rf_gs.grid_scores_]
scores = np.array(scores).reshape(len(n_estimator), len(max_depth))

features = train_data.ix[:, train_data.columns != 'tip_perc'].columns
importances = rf_gs.best_estimator_.feature_importances_
indices = np.argsort(importances)

imp_feat = pd.DataFrame({'feature':features, 'importance':importances})
imp_feat.to_csv("imp_feat.csv", index = False)
