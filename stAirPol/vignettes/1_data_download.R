require(RSQLite)
require(stAirPol)
require(data.table)

#' Here we specify the folder where all data should be saved
#' Also, the time range is specified here
path = '~/stAirPol_data'
start_date = "2017-12-01"
end_date = "2017-12-31"


# (1) Download data: luftdaten.info ---------------------------------------
#' Download the raw csv files
#' Downloaded files are stored in the subfolder ./luftdaten/
#' NOTE: That may take some time
dir.create(path)
dir.create(paste0(path, "/luftdaten/"))
LD_download(folder = paste0(path, "/luftdaten/"), start = start_date,
            end = end_date)

#' Transform the csv files to a useful format, and store them in a
#' SQLite database
dir.create(paste0(path, "/db/"))
LD_sql2(start = start_date, end = end_date,
        source = paste0(path, "/luftdaten/"),
        dbname = paste0(path, "/db/"))
dir.create(paste0(path, "/db/data"))
split_db_in_months(paste0(path, "/db"))
download_sensor_age(path = path)

# (2) Download data: DWD --------------------------------------------------
#' Download wind and precipitation from the DWD
#' All Information’s are stored in the subfolder ./dwd
dir.create(paste0(path, "/dwd/"))
download_dwd_data(obs = 'wind', path = paste0(path, "/dwd/"))
download_dwd_data(obs = 'precipitation', path = paste0(path, "/dwd/"))
setup_station_table(paste0(path, "/dwd/"))


# (3) Download data: Open Transport Map -----------------------------------
dir.create(paste0(path, "/otm/"))
download_OTM(path = paste0(path, "/otm/"))


#' Now you are able to run 2_data_preparation.R, please use there the correct
#' path and a time range which is included in the time range specified here

