'''
Series of functions for loading and extracting the necessary data
'''
import pandas as pd
import json
from shapely.geometry import Point
import geopandas as gpd


def subset_data(data, complete_gps, countries):
    """Load and subset RHoMIS data
    Arguments:
        data {DataFrame} -- A pandas data frame. 
        complete_gps {boolean} -- Whether or not to only keep data with complete gps coordinates
        countries {array} -- Which countries to keep (use two letter ISO country codes)
    """
    subset = pd.Series(data=[True]*data.shape[0]
                       )  # Making a single column of True

    if (complete_gps):
        subset = subset & \
            data["gps_lat"].notna() & \
            data["gps_lon"].notna()

    if (countries):
        subset = subset & \
            data["iso_country_code"].isin(countries)

    return data.loc[subset, :]


def load_json(path):
    with open("./data/country_mappings.json", "r") as read_file:
        country_mappings = json.load(read_file)
    return pd.DataFrame(country_mappings)


def link_points_to_ipums_and_world(country_code,
                                   lat_column,
                                   lon_column,
                                   point_df_path,
                                   ipums_data,
                                   world_shapefile):

    point_df = pd.read_csv(
        point_df_path, encoding="latin1", na_values=[-999, "-999"])

    if country_code != "BFA":
        point_df["geometry"] = [Point(xy) for xy in zip(
            point_df[lon_column], point_df[lat_column])]
        point_df = gpd.GeoDataFrame(
            point_df, crs="epsg:4326").set_geometry('geometry')
        point_df = point_df.sjoin(
            world_shapefile, how="left", predicate="within")
        point_df = point_df.rename(
            columns={"index_right": "index_world_shapefile"})
        point_df = point_df.sjoin(ipums_data, how="left", predicate="within")
        point_df = point_df.rename(
            columns={"index_right": "index_ipums_terra"})

        return point_df

    if country_code == "BFA":
        ipums_data["district_lower"] = ipums_data["LABEL"].str.lower()
        point_df["district_lower"] = point_df["District"].str.lower()

        point_df = point_df.merge(ipums_data, how="left", on="district_lower")
        point_df.drop(columns=["district_lower"], inplace=True)

        point_df = point_df.merge(
            world_shapefile, how="left", left_on="iso_3", right_on="iso_a3")

        return point_df
