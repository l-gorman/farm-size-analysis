import re
import pandas as pd
import geopandas as gpd

base_path = "./data/ipums/raw/"
base_shape_file_string = "boundaries_15086_IPUMS"
data_sets = ["BF_SLAD_2006","ET_SLAD_2007","GH_SLAD_2010","KE_SLAD_2009","ML_SLAD_2009","NG_SLAD_2010","RW_SLAD_2012","TZ_SLAD_2012","UG_SLAD_2002"]

iso_2 = [string[0:2] for string in data_sets]
year =  [string[-4:] for string in data_sets]
id =  [string[0:2]+string[-4:] for string in data_sets]



for index in range(0,len(data_sets)):
    df_path = base_path + "csvs/data_15086_IPUMS_" + data_sets[index] + ".csv"
    shape_file_path = base_path + "shapefiles/boundaries_15086_IPUMS_" + data_sets[index] + ".shp" 

    new_data_set = pd.read_csv(df_path)
    new_data_set.columns = [re.sub(id[index],"", string) for string in new_data_set.columns]
    new_data_set.columns=[re.sub("__","", string) for string in new_data_set.columns]
    new_data_set.columns = [string.rstrip("_") for string in new_data_set.columns]
    new_data_set["iso_2"] = iso_2[index]
    new_data_set["year"] = year[index]
    new_data_set["id"]=id[index]

    new_shape_file = gpd.read_file(shape_file_path)
    new_shape_file["GEOID"]=new_shape_file["GEOID"].astype("int64")

    new_data_set = new_shape_file.merge(new_data_set, left_on="GEOID", right_on="GEO2")

    if index==0:
        ipums_terra_data = new_data_set
    if index>0:
        ipums_terra_data = ipums_terra_data.append(new_data_set)
    

ipums_terra_data.to_csv("./data/ipums/processed/ipums_terra_merged.csv")