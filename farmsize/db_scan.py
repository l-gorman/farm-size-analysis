from sklearn.cluster import DBSCAN
from sklearn.preprocessing import StandardScaler

def cluster_gps_points(data, lon_col, lat_col, epsilon):
    """Function for spatial clustering of
    point data based on GPS coordinates

    Can be plot with
    # plt.plot(X_gps_standard[:,0], X_gps[:,1], 'o', markersize=1)

    Args:
        data (pd.DataFrame): A data frame of points, containing latitute and longitude
        lon_col (String): The name of the column containing the longitudes
        lat_col (String): The name of the column containing the latitudes
        epsilon (Int): The maximum distance between samples. see `here <https://scikit-learn.org/stable/modules/generated/sklearn.cluster.DBSCAN.html>`_

    Returns:
        np.array: An array of cluster numbers
    """
    # Getting data in correct format for DBSCAN
    # and standardising for fit
    X_gps = data.loc[:,[lon_col, lat_col]].values
    X_gps_standard = StandardScaler().fit_transform(X_gps)

    db_fit = DBSCAN(eps=epsilon).fit(X_gps_standard)

    return db_fit.labels_


