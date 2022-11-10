import pandas as pd
import numpy as np
import plotly.express as px
import pycountry

east = pd.read_csv('https://raw.githubusercontent.com/bridg3r/myclasses/main/datasets/Onsmei_Footprint_East.csv')
west = pd.read_csv('https://raw.githubusercontent.com/bridg3r/myclasses/main/datasets/Onsmei_Footprint_West.csv')
asia = pd.read_csv('https://raw.githubusercontent.com/bridg3r/myclasses/main/datasets/Onsemi_Footprint_Asia.csv')
cities = pd.read_csv('https://raw.githubusercontent.com/datasets/world-cities/master/data/world-cities.csv')

world_cities = cities.name.to_list()
world_countries = []
for country in pycountry.countries:
  world_countries.append(country.name)

world_countries.append('Vietnam')

world_cities = cities.name.to_list()

fabs = ['Aizu', 'Bucheon', 'Gresham', 'Mountain Top', 'Pocatello', 'Nampa', 'ISMF Seremban', 'Rožnov']
ATO = ['Carmona', 'Cebu', 'Tarlac', 'Leshan', 'Shenzhen', 'Suzhou', 'Seremban', 'Burlington', 'OSV', 'Bình Dương']


all_cities = fabs + ATO
all_cities = all_cities + (world_cities)

def find_locations(region, us=0):

  region = region.drop_duplicates(subset = ['Address']).dropna(subset= ['Address'])

  country = []
  for i in range(len(region.Address)):
    if us == 1: 
      country.append('United States')
      region['city']=region.Address.str.split(',', expand = True).iloc[:,-3].str.strip()
      region['country'] = 'United States'
      return region
    country.append(np.NaN)

  index = 0
  for i in region.Address:
    for j in world_countries:
      if i.find(j) != -1:
        country[index] = j
        break
    index+=1

  region['country'] = country

  city = []
  for i in range(len(region.Address)):
   city.append(np.NaN)

  index = 0
  for i in region.Address:
    for j in all_cities:
      if i.find(j) != -1:
        city[index] = j
        break
    index+=1

  region['city'] = city

  return region

west = find_locations(west, 1)
east = find_locations(east)
asia = find_locations(asia)

dat = west.append([east, asia]).reset_index()

dat = dat.assign(
    site_type = lambda x: 
    np.where(
        x.city.isin(fabs),
        'Wafer Fab Site',
          np.where(
            x.city.isin(ATO),
            'ATO',
            'other'
          )
    )
)

fig = px.scatter_geo(dat, lat=dat.Latitude, hover_data = ['country'], lon=dat.Longitude, hover_name = 'city', color = 'site_type')
fig.update_layout(
            title={
            'text' : 'Onsemi Footprint',
            'x':0.5,
            'xanchor': 'center'
        })
chart_studio.tools.set_config_file(world_readable=False, sharing='private')
fig.show()