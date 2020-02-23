# 2019 District Council Election Map 
Interactive map of proportion of pro-democracy voters in 2019 Hong Kong District Council Election

2019年區議會選舉結果黃藍選民分佈圖

View map [here](https://elgarteo.github.io/dce_2019/dce_2019.html)

## What it does
The R script generates a map that shows the proportion of pro-democracy voters in each constituency with
varying depth of yellow and blue. The more yellowish indicate higher proportion of pro-democracy voters, and vice versa.
The classification of candidates is based on [the Stands News' election site](https://dce2019.thestandnews.com/).

## What you need
* [Shapefile](https://accessinfo.hk/en/request/shapefileshp_for_2019_district_c) of the 2019 DCCA
(kudos to [accessinfo.hk](https://accessinfo.hk))
* [helixcn/HK80](https://github.com/helixcn/HK80)

The R script automatically fetches the constituency details and election result from the json database file of
the Stands News' election site.

All other packages are obtainable from CRAN.
