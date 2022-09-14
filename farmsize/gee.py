
import ee
import folium
import geopandas as gpd
import pandas as pd


def add_ee_layer(self, ee_image_object, vis_params, name):
    '''
    Add an earth engine layer to a folium map
    '''

    map_id_dict = ee.Image(ee_image_object).getMapId(vis_params)
    folium.raster_layers.TileLayer(
        tiles=map_id_dict['tile_fetcher'].url_format,
        attr='Map Data &copy; <a href="https://earthengine.google.com/">Google Earth Engine</a>',
        name=name,
        overlay=True,
        control=True
    ).add_to(self)


def point_reading(
    lat,
    lon,
    image_collection,
    band,
    date_start,
    date_end,
    scale=1000,
    stat="mode"
):
    '''
    Returns the summary category for a particular
    band, from a particular image collection. This 
    function will take an image collection, select 
    the band, and filter between the two dates 
    provided. Depending on the date range selected, 
    this can return multiple "images" in a collection.
    This function then takes a statistic over all 
    of the images (e.g. mean, mode, min) and samples
    the statistic for the point of interest.

            Parameters:
                    lat (float): A decimal integer
                    lon (float): Another decimal integer
                    image_collection (ee.ImageCollection) A google earth engine collection
                    band (str) The name of the band of interest
                    date_start (yyyy-mm-dd) The start date of interest
                    date_end (yyyy-mm-dd) The end date of interest
                    scale (float) Scale (in meters) of the projection to sample.
                    stat (str) Which statistic you would like to use to aggregate multiple time points. Options include min, max, mean, mode, median



            Returns:
                    value (float): Returns the array value for that point
    '''
    # Take latitude and longitude and make it into an "earth engine" point
    point = ee.Geometry.Point([lon, lat])
    # Filter the collection for the band of interest
    # and for the
    image_collection = (image_collection.
                        # Select the band
                        select(band).
                        # Filter Date
                        filterDate(date_start, date_end))
    # Get a point reading

    count = image_collection.size().getInfo()
    print('Number of Images in Range: ', count)

    if (stat == "mode"):
        image = image_collection.mode()

    if (stat == "max"):
        image = image_collection.max()

    if (stat == "min"):
        image = image_collection.min()

    if (stat == "mean"):
        image = image_collection.mean()

    if (stat == "median"):
        image = image_collection.median()

    point_reading = (image.
                     # Sample that point at a particular scale
                     sample(point, scale).
                     # Select the band of interest
                     select(band).
                     # Make the final call to the API and subset
                     # to get the result
                     getInfo())["features"][0]["properties"]
    return point_reading


def get_coords_from_polygon(polygon):
    if polygon.geom_type == "Polygon":
        # If polygon boundary geometry i a line string, then this is a polygon with holes
        polygon = [point for point in polygon.exterior.coords]
        polygon = ee.Geometry.Polygon(polygon)
        return polygon

    if polygon.geom_type == "MultiPolygon":
        multi_polygon = polygon
        geometry_list = []
        for subpolygon in multi_polygon.geoms:
            # subpolygon_coords = [point for point in subpolygon.boundary.coords]

            subpolygon_coords = get_coords_from_polygon(subpolygon)
            geometry_list.append(subpolygon_coords)
        multi_polygon = ee.Geometry.MultiPolygon(geometry_list)

        return multi_polygon


def polygon_category_histogram(
    polygon,
    image_collection,
    band,
    date_start,
    date_end,
    conversion_table,
    scale=30,
    maxPixels=1e9
):
    '''
    Returns the fractional coverage of pixel categories, 
    from a particular image collection. This 
    function will take an image collection, select 
    the band, and filter between the two dates 
    provided. Depending on the date range selected, 
    this can return multiple "images" in a collection.
    This function then take the mode over all 
    of the images (all of the dates). Finally it will
    calculate the proportion of pixels from each class.

        Parameters:
                polygon (Polygon) A geopandas polygon
                image_collection (ee.ImageCollection) A google earth engine collection
                band (str) The name of the band of interest
                date_start (yyyy-mm-dd) The start date of interest
                date_end (yyyy-mm-dd) The end date of interest
                conversion_table (pd.DataFrame) A pandas dataframe to convert numeric classes to table
                scale (float) Scale (in meters) of the projection to sample.
                stat (str) Which statistic you would like to use to aggregate multiple time points


        Returns:
                value (float): Returns the array value for that point
    '''
    # Making geo
    polygon = get_coords_from_polygon(polygon)

    image_collection = (image_collection.
                        # Select the band
                        select(band).
                        # Filter Date
                        filterDate(date_start, date_end))

    # Import the Landsat 8 TOA image collection.
    image = image_collection.mode()
    # Get the frequency information
    pixel_frequency = image.reduceRegion(
        reducer=ee.Reducer.frequencyHistogram(),
        geometry=polygon,
        scale=scale,
        maxPixels=maxPixels
    ).getInfo()

    # Get the count per pixel
    pixel_count = image.reduceRegion(
        reducer=ee.Reducer.count(),
        geometry=polygon,
        scale=30,
        maxPixels=1e9
    ).getInfo()

    keys = list(pixel_frequency[band].keys())
    values = list(pixel_frequency[band].values())
    pixel_ratio = [pixel_cat/pixel_count[band] for pixel_cat in values]

    pixel_coverage = pd.DataFrame({
        "class": keys,
        "pixel_coverage": pixel_ratio
    })

    pixel_coverage["class"] = pixel_coverage["class"].astype('int')
    conversion_table[conversion_table.columns[0]
                     ] = conversion_table[conversion_table.columns[0]].astype('int')

    left_on = "class"
    right_on = conversion_table.columns[0]

    result = pd.merge(pixel_coverage, conversion_table,
                      left_on=left_on, right_on=right_on, how="outer")

    result.loc[pd.isna(result["pixel_coverage"]), "pixel_coverage"] = 0
    result["new_index"] = 1

    result = result.pivot(index="new_index", columns="Tag",
                          values="pixel_coverage")
    result = result.reset_index(drop=True)
    result.columns.name = ''
    return(result)
