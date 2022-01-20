import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import requests
from urllib3.util.retry import Retry
from requests.adapters import HTTPAdapter

from shapely.geometry import shape

def get_country_admin_boundaries(iso_a_3, admin_level):
    """Get country administrative boundaries
    from the GeoBoundaries database:
    https://www.geoboundaries.org/

    Args:
        iso_a_3 (String): Three letter ISO country code of the country
        admin_level (String): Can select "1", "2", or "3". Numbers represent subnational boundaries of increasing detail 

    Returns:
        [geopandas.GeoDataFrame]: A data frame containing the subnational boundaries of that country
    """
    # Allowing Retries
    s = requests.Session()
    retries = Retry(total=5,
                backoff_factor=0.1,
                status_forcelist=[ 500, 502, 503, 504 ])
    s.mount('http://', HTTPAdapter(max_retries=retries))

    # See documentation on GeoBoundaries API
    url = "https://www.geoboundaries.org/gbRequest.html?ISO="+iso_a_3+"&ADM=ADM"+admin_level
    r = s.get(url)
    download_url  = r.json()[0]['gjDownloadURL']
    geoBoundary = s.get(download_url).json()

    names = [feature["properties"]["shapeISO"] for feature in geoBoundary["features"] ]
    shapeISO = [feature["properties"]["shapeName"] for feature in geoBoundary["features"] ]
    shapeID = [feature["properties"]["shapeID"] for feature in geoBoundary["features"] ]
    shapeType = [feature["properties"]["shapeType"] for feature in geoBoundary["features"] ]
    geometry = [shape(feature["geometry"]) for feature in geoBoundary["features"] ]

    geo_data_frame = gpd.GeoDataFrame(data={
        "iso_a3": iso_a_3,
        "region_name": names,
        "shapeISO": shapeISO,
        "shapeID": shapeID,
        "shapeType": shapeType,
        "geometry": geometry
    })

    return geo_data_frame

def get_admin_boundaries_multiple_countries(countries, admin_level):

    """Get administrative boundaries
    Returns:
        [type]: [description]

    Examples:
        countries_iso_3=["ETH","KEN"]
        admin_1 = mapping.get_admin_boundaries_multiple_countries(countries_iso_3, "1")
        admin_1.to_csv("./data/mapping/subnational_boundaries_admin_1",index=False)

        admin_2 = mapping.get_admin_boundaries_multiple_countries(countries_iso_3, "2")
        admin_2.to_csv("./data/mapping/subnational_boundaries_admin_2",index=False)

    """

    list_of_dfs = []
    for country in countries:
            print(f"Fetching data for {country}")
            list_of_dfs.append(get_country_admin_boundaries(country, admin_level))
    combined_dfs = gpd.GeoDataFrame(pd.concat(list_of_dfs,ignore_index=True))

    return combined_dfs

def get_admin_boundaries_africa(admin_level):
    countries = ['TZA', 'COD', 'SOM', 'KEN', 'SDN', 'TCD', 'ZAF', 'LSO',
       'ZWE', 'BWA', 'NAM', 'SEN', 'MLI', 'MRT', 'BEN', 'NER', 'NGA',
       'CMR', 'TGO', 'GHA', 'CIV', 'GIN', 'GNB', 'LBR', 'SLE', 'BFA',
       'CAF', 'COG', 'GAB', 'GNQ', 'ZMB', 'MWI', 'MOZ', 'SWZ', 'AGO',
       'BDI', 'MDG', 'GMB', 'TUN', 'DZA', 'ERI', 'MAR', 'EGY', 'LBY',
       'ETH', 'DJI', 'UGA', 'RWA', 'SSD']
    
    combined_df = get_admin_boundaries_multiple_countries(countries, admin_level)
    return combined_df


def read_geo_csv(path):
    data = pd.read_csv(path)
    data['geometry'] = gpd.GeoSeries.from_wkt(data['geometry'])
    data = gpd.GeoDataFrame(data, crs="epsg:4326").set_geometry('geometry')

    return data
