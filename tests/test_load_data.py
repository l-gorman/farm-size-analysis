
import pytest
import pandas as pd
from farm_size_analysis_module.load_data import subset_data
# import datatest as dt


def test_subset():
    test_df = pd.DataFrame(data={
    "ID": [1,2,3,4,5,6],
    "GPS_LAT": [0.1, 2, -54, 35, pd.NA, pd.NA],
    "GPS_LON": [56, 3, pd.NA, 14, -50, pd.NA],
    "ID_COUNTRY": ["ET", "KE", "KE", "VN", pd.NA, "DC"],
    "random_other_column":["a","b","c",pd.NA,"e", pd.NA]
    })

    new_df = subset_data(data=test_df, complete_gps=True)