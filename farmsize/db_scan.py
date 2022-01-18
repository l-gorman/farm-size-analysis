import numpy as npp
from sklearn.cluster import DBSCAN
from sklearn import metrics
from sklearn.preprocessing import StandardScaler
from sklearn.datasets import make_blobs
import numpy as np

def cluster_gps_points(data, lon_col, lat_col, epsilon):

    # Getting data in correct format for DBSCAN
    # and standardising for fit
    X_gps = data.loc[:,[lon_col, lat_col]].values
    X_gps_standard = StandardScaler().fit_transform(X_gps)

    db_fit = DBSCAN(eps=epsilon).fit(X_gps_standard)

    return db_fit.labels_


    # plt.plot(X_gps_standard[:,0], X_gps[:,1], 'o', markersize=1)