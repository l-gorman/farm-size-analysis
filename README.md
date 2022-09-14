# Spatial Analysis Project in Python

An initial project for exploring potential for a farm size analysis.

The premise of the project is to 


* Python package for analysis can be found in the `farmsize` folder.
* Data was downloaded from IPUMS terra and was prepared using the `./src/clean_ipums_data.py` script.
* Main data preperation and initial EDA occurred in the `explore.ipynb` notebook.
* Linking to google earth engine occures in `google_earth_engine.ipynb`
* Data analysis taking place in `heterogeneity-analysis.Rmd` notebook.






# Useful Reading

* A good [tutorial](https://anbasile.github.io/posts/2017-06-25-jupyter-venv/) for reproducible jupyter environments and project structure.
* [Geo Boundaries Database](https://www.geoboundaries.org/) 
* All geographic files IPUMS [found here](https://international.ipums.org/international/geography_gis.shtml)
* World map and geographical information from IPUMS [found here](https://international.ipums.org/international/gis.shtml)
* Spatially harmonized level 2 geography dataset from IPUMS [found here](https://international.ipums.org/international/gis_harmonized_1st.shtml)
* Spatially harmonized level 2 geography dataset from IPUMS [found here](https://international.ipums.org/international/gis_harmonized_2nd.shtml)
* Use [this program](https://codap.concord.org/app/static/dg/en/cert/index.html#) for opening codap files

# Useful tips

* [Using jupyter with venv](https://anbasile.github.io/posts/2017-06-25-jupyter-venv/)
* On the markdown presentation, press S to enter presenter view and see notes
* Look at terrapop extract for information on Terra Data

# Rspatial Issues

Installing Rspatial packages alongside a virtual environment can be difficult. These
libraries rely on "openssl", "proj", "sqlite3", and "GDAL". Below are the steps that
I needed to take. 
* To install openssl:
```
install.packages(
  'openssl', 
  "--with-proj-include=/opt/homebrew/include/ --with-proj-lib=/opt/homebrew/lib/",
  configure.vars = 'LIB_DIR=/opt/homebrew/opt/openssl@1.1/lib INCLUDE_DIR=/opt/homebrew/opt/openssl@1.1/include'
)
```

* To install s2:
```
install.packages('s2', 
  "--with-proj-include=/opt/homebrew/include/ --with-proj-lib=/opt/homebrew/lib/",
  configure.vars = 'LIB_DIR=/opt/homebrew/opt/openssl@1.1/lib INCLUDE_DIR=/opt/homebrew/opt/openssl@1.1/include'
)
```
* To install sf:
```
install.packages('sf', 
  configure.args=c(
    '--with-gdal-config=/opt/homebrew/opt/gdal/bin/gdal-config',
    '--with-geos-config=/opt/homebrew/opt/geos/bin/geos-config',
    '--with-proj-lib=/opt/homebrew/lib/'
  ), 
  configure.vars='GDAL_DATA=/opt/homebrew/opt/gdal/share/gdal/'
)
```

* To install lwgeom:
```
install.packages('lwgeom', 
  configure.args=c(
    '--with-gdal-config=/opt/homebrew/opt/gdal/bin/gdal-config',
    '--with-geos-config=/opt/homebrew/opt/geos/bin/geos-config',
    '--with-proj-lib=/opt/homebrew/lib/',
    '--with-proj-include=/opt/homebrew/include/'
  ), 
  configure.vars='GDAL_DATA=/opt/homebrew/opt/gdal/share/gdal/'
)

```

* To install stars:
```
install.packages('stars', 

  configure.args=c(
    '--with-gdal-config=/opt/homebrew/opt/gdal/bin/gdal-config',
    '--with-geos-config=/opt/homebrew/opt/geos/bin/geos-config',
    '--with-proj-lib=/opt/homebrew/lib/',
    '--with-proj-include=/opt/homebrew/include/'
  ), 
  configure.vars='GDAL_DATA=/opt/homebrew/opt/gdal/share/gdal/'
)
```



