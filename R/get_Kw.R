# ################################################################################
# Code started by Molly Stroud on 1/20/26
# Get Kw value from LAGOS if US
# Download dataset here: https://doi.org/10.6073/pasta/128700feb3bbc3ffe5800e7b232bd81f
# And lake IDs here: https://doi.org/10.6073/pasta/e5c2fb8d77467d3f03de4667ac2173ca
################################################################################
#pacman::p_load(tidyverse)
get_kw_US <- function(bbox){
  message("Downloading LAGOS Lake Information")
  # lakeinfo <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/854/1/007ca4f5ec02bb5809fc661dcfa7a903")
  # https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1
  lakeinfo <- read_csv("lake_information.csv")
  mylake <- lakeinfo |>
    #filter(str_detect(lake_namegnis, lakename))
    filter(bbox["left"] < lake_lon_decdeg & bbox["right"] > lake_lon_decdeg) |>
    filter(bbox["bottom"] < lake_lat_decdeg & bbox["top"] > lake_lat_decdeg)
  if(nrow(mylake) == 1){
    print(mylake)
  } else if (nrow(mylake) > 1){
    print(mylake)
    q <- readline("Which is the correct lake? Enter line number:   ")
    mylake <- mylake[q,]
  } else {
    message("Your lake may not exist in the LAGOS database. You may also search the database by lake name, NHDID, and more.")
    }
    message("Downloading LAGOS Secchi information.")
    #original lagos file (over 7GB)
   #### lagos_qual <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1427/1/3cb4f20440cbd7b8e828e4068d2ab734")
    lagos_qual <- read_csv("LAGOS_US_LANDSAT_Predictions_AVERAGED.csv", show_col_types = F)
    mylake_secchi <- lagos_qual |>
      filter(lagoslakeid == mylake$lagoslakeid)
    message("Mean secchi for this lake is ", mylake_secchi$mean_secchi)
    Kw <- 1.7 / mylake_secchi$mean_secchi
    return(Kw)
}

get_kw_global <- function(bbox){
  message("Downloading Global Lake Information")
  lakeinfo <- read_csv("https://raw.githubusercontent.com/roohollahnoori/AWQDFGL/refs/heads/main/Unique_Lake_Name.csv")
  mylake <- lakeinfo |>
    filter(bbox["left"] < Longitude & bbox["right"] > Longitude) |>
    filter(bbox["bottom"] < Latitude & bbox["top"] > Latitude)
  if(dim(mylake)[1] != 0){
    print(mylake)
    q <- readline("Is this the correct lake? (Y/N):  ")
    if(tolower(q) %in% c("Y", "y", "Yes", "yes")){
      message("Downloading Global Secchi information. This may take a minute.")
      globalsecchi <- read_csv("https://raw.githubusercontent.com/roohollahnoori/AWQDFGL/refs/heads/main/SDD.csv")
      mylake_secchi <- globalsecchi |>
        filter(`Unique Lake` == mylake$`Unique Lake`) 
      meansecchi <- mean(mylake_secchi$`Secchi (m)`, na.rm = T)
      message("Mean secchi for this lake is ", meansecchi)
      Kw <- 1.7 / meansecchi
      return(Kw)
    } else {message("Your lake may not exist in the database.")}
  } else {message("Your lake may not exist in the database.")
    }
}
#global <- read_csv("https://raw.githubusercontent.com/roohollahnoori/AWQDFGL/refs/heads/main/SDD.csv")
#lakenames <- read_csv("https://raw.githubusercontent.com/roohollahnoori/AWQDFGL/refs/heads/main/Unique_Lake_Name.csv")
