'''
Series of functions for loading and extracting the necessary data
'''
import pandas as pd
import json

def subset_data(data, complete_gps, countries):
    """Load and subset RHoMIS data
    Arguments:
        data {DataFrame} -- A pandas data frame. 
        complete_gps {boolean} -- Whether or not to only keep data with complete gps coordinates
        countries {array} -- Which countries to keep (use two letter ISO country codes)
    """   
    subset = pd.Series(data=[True]*data.shape[0]) # Making a single column of True

    if (complete_gps):
        subset = subset & \
                data["GPS_LAT"].notna() & \
                data["GPS_LON"].notna()
    
    if (countries):
        subset = subset & \
                data["ID_COUNTRY"].isin(countries)
    
    return data.loc[subset,:]

def load_json(path):
    with open("./data/country_mappings.json","r") as read_file:
        country_mappings = json.load(read_file)
    return pd.DataFrame(country_mappings)

