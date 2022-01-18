from farmsize import mapping
from os.path import exists

countries = ["ETH", "KEN", "TZA", "UGA", "BFA", "MLI", "GHA","CIV", "NER", "SLE", "NGA","RWA", "BDI"]
admin_level="2"
base_file_path="./data/mapping/subnational_boundaries/admin"+admin_level+"/"

for country in countries:
    print(country)
    new_file=base_file_path+country+".csv"
    file_exists = exists(new_file)
    if file_exists is False:
        new_data=mapping.get_country_admin_boundaries(country, admin_level)
        new_data.to_csv(new_file,index=False)
    if file_exists:
        print("Skipped as data already downloaded")

# admin_1 = mapping.get_admin_boundaries_multiple_countries(countries_iso_3, "1")
# admin_1.to_csv("./data/mapping/subnational_boundaries_admin_1.csv",index=False)

# admin_2 = mapping.get_admin_boundaries_multiple_countries(countries_iso_3, "2")
# admin_2.to_csv("./data/mapping/subnational_boundaries_admin_2.csv",index=False)

# admin_3 = mapping.get_admin_boundaries_multiple_countries(countries_iso_3, "3")
# admin_3.to_csv("./data/mapping/subnational_boundaries_admin_3.csv",index=False)