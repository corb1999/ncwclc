# ncwclc
north carolina workers comp loss costs

The purpose of this analysis project is to download, clean, then analyze the north carolina workers comp annual loss costs from the rating bureau.

http://www.ncrb.org/ncrb/workerscompensation/ratefilings.aspx

ETL folder houses all the data cleaning. Downloaded files should be saved in the ore subfolder, and then after being cleaned/refined, should output a cleaned data object to the ingot folder which can be picked up in analysis scripts.

EDA folder houses scripts to analyze the cleaned loss cost data.

Notes:
- Will only try to consume and compile 2018 loss costs and forward, as prior to that date, the file format in excel is different and would require a different cleaning script to read and consume those tables.


