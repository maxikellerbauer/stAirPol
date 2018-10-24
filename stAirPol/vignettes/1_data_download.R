require(RSQLite)
require(stAirPol)

#' Here we specify the folder where all data should be saved
#' Also the timerange is secified here
path = '~/stAirPol_data'
start_date = "2017-12-01"
end_date = "2017-12-05"


# (1) Download data: luftdaten.info ---------------------------------------
#' Download the raw csv files
#' Downloaded files are stored in the subfolder ./luftdaten/
#' NOTE: That may take some time
dir.create(paste0(path, "/luftdaten/"))
LD_download(folder = paste0(path, "/luftdaten/"), start = start_date,
            end = end_date)

#' Transform the csv files to a usefull format, and store them in a
#' SQLite database
dir.create(paste0(path, "/db/"))
LD_sql2(start = start_date, end = end_date,
        source = paste0(path, "/luftdaten/"),
        dbname = paste0(path, "/db/"))
split_db_in_months(paste0(path, "/db"))


# (2) Download data: DWD --------------------------------------------------
#' Download wind and precipitation from the DWD
#' All Informaionts are stored in the subfolder ./dwd
dir.create(paste0(path, "/dwd/"))
download_dwd_data(obs = 'wind', path = paste0(path, "/dwd/"))
download_dwd_data(obs = 'precipitation', path = paste0(path, "/dwd/"))



# (3) Download data: Open Transport Map -----------------------------------
#' currently not working due to a serverside issue on
#' http://opentransportmap.info. See the url for more information.
#' The data which is needed for the package, is stored on the CD
dir.create(paste0(path, "/otm/"))
download_OTM(path = paste0(path, "/otm/"))



# (4) Delete all unnecessary stuff ----------------------------------------
#' To save space on your disk, everything which isn't needed will deleted now
file.remove()

