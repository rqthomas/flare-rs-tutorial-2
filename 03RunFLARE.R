################################################################################
# Author: Molly Stroud
# Started 2/5/26
################################################################################
# This script will run FLARE using the inputs created in 02Get_Inputs
pacman::p_load('tidyverse', 'lubridate')
remotes::install_github("FLARE-forecast/FLAREr", force = T, upgrade = 'never', ref = 'v3.1-dev')


# This need to be set to run each experiment
run_name <- "run"
config_flare_file <- "configure_flare.yml"
starting_index <- 1 
experiments <- c("with_rs")


# These don't need to be changed
config_set_name <- "analysis"
configure_run_file <- "configure_run.yml"
use_s3 <- FALSE

lake_directory <- here::here()
# Point FLARE to GLM location
Sys.setenv('GLM_PATH'=paste0(lake_directory, '/binary/macos/glm'))
options(future.globals.maxSize = 891289600)

#walk(list.files(file.path(lake_directory, "R"), full.names = TRUE), source)

### Set up simulation start and end dates

num_forecasts <- 1
days_between_forecasts <- 5
forecast_horizon <- 5
starting_date <- as_date(start_date) 
second_date <- as_date(end_date) - days(days_between_forecasts)

all_dates <- seq.Date(starting_date,second_date + days(days_between_forecasts * num_forecasts), by = 1)

potential_date_list <- list(with_rs = all_dates)

date_list <- potential_date_list[which(names(potential_date_list) %in% experiments)]

models <- names(date_list)

start_dates <- as_date(rep(NA, num_forecasts + 1))
end_dates <- as_date(rep(NA, num_forecasts + 1))
start_dates[1] <- starting_date
end_dates[1] <- second_date
for(i in 2:(num_forecasts+1)){
  start_dates[i] <- as_date(end_dates[i-1])
  end_dates[i] <- start_dates[i] + days(days_between_forecasts)
}

sims <- expand.grid(paste0(start_dates,"_",end_dates,"_", forecast_horizon), models)

names(sims) <- c("date","model")

sims$start_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,1]
sims$end_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,2]
sims$horizon <- stringr::str_split_fixed(sims$date, "_", 3)[,3]

sims <- sims |>
  dplyr::mutate(model = as.character(model)) |>
  dplyr::select(-date) |>
  distinct_all() |>
  dplyr::arrange(start_dates)

sims$horizon[1:length(models)] <- 0

i <- 1
for(i in starting_index:nrow(sims)){
  
  message(paste0("index: ", i))
  message(paste0("     Running model: ", sims$model[i], " "))
  
  model <- sims$model[i]
  sim_names <- paste0(config_set_name, "_", run_name , "_" ,model)
  
  config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, sim_name = sim_names, clean_start = TRUE)
  
  yml <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  yml$sim_name <- sim_names
  yml$configure_flare <- config_flare_file
  
  
  yaml::write_yaml(yml, file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  
  yml <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, config_flare_file))
  
  if(model == "no_da"){
    yml$da_setup$use_obs_constraint <- FALSE
  }else{
    yml$da_setup$use_obs_constraint <- TRUE
  }
  
  yaml::write_yaml(yml, file.path(lake_directory, "configuration", config_set_name, config_flare_file))
  
  run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  run_config$configure_flare <- config_flare_file
  run_config$sim_name <- sim_names
  run_config$start_datetime <- as.character(paste0(sims$start_dates[i], " 00:00:00"))
  run_config$forecast_start_datetime <- as.character(paste0(sims$end_dates[i], " 00:00:00"))
  run_config$forecast_horizon <- as.numeric(sims$horizon[i])
  run_config$configure_flare <- config_flare_file
  if(i <= length(models)){
    config$run_config$restart_file <- NA
  }else{
    run_config$restart_file <- paste0(config$location$site_id, "-", lubridate::as_date(run_config$start_datetime), "-", sim_names, ".nc")
    if(!file.exists(file.path(config$file_path$restart_directory, paste0(config$location$site_id, "-", lubridate::as_date(run_config$start_datetime), "-", sim_names, ".nc")) )){
      warning(paste0("restart file: ", run_config$restart_file, " doesn't exist"))
    }
  }
  
  
  
  yaml::write_yaml(run_config, file = file.path(lake_directory, "restart", site, sim_names, configure_run_file))
  
  config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, sim_name = sim_names, clean_start = FALSE)
  
  config <- FLAREr:::get_restart_file(config, lake_directory)
  
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())
  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  
  met_out <- FLAREr:::create_met_files(config, lake_directory = lake_directory, met_forecast_start_datetime, met_start_datetime)
  
  
  #Create observation matrix
  obs <- FLAREr:::create_obs_matrix(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-rs.csv")),
                                    obs_config = obs_config,
                                    config)
  
  obs_non_vertical <- FLAREr:::create_obs_non_vertical(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-rs.csv")),
                                                       obs_config,
                                                       start_datetime = config$run_config$start_datetime,
                                                       end_datetime = config$run_config$end_datetime,
                                                       forecast_start_datetime = config$run_config$forecast_start_datetime,
                                                       forecast_horizon =  config$run_config$forecast_horizon)
  states_non_vertical <- NULL
  states_non_vertical$depth_sd <- 0
  
  states_config <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)
  
  model_sd <- FLAREr:::initiate_model_error(config, states_config)
  
  init <- FLAREr:::generate_initial_conditions(states_config,
                                               obs_config,
                                               pars_config,
                                               obs,
                                               config,
                                               obs_non_vertical = obs_non_vertical)
  
  # create forecast!!!!
  da_forecast_output <- FLAREr:::run_da_forecast(states_init = init$states,
                                                 pars_init = init$pars,
                                                 aux_states_init = init$aux_states_init,
                                                 obs = obs,
                                                 obs_sd = obs_config$obs_sd,
                                                 model_sd = model_sd,
                                                 working_directory = config$file_path$execute_directory,
                                                 met_file_names = met_out$filenames,
                                                 config = config,
                                                 pars_config = pars_config,
                                                 states_config = states_config,
                                                 obs_config = obs_config,
                                                 da_method = config$da_setup$da_method,
                                                 par_fit_method = config$da_setup$par_fit_method,
                                                 obs_non_vertical = obs_non_vertical,
                                                 states_non_vertical = states_non_vertical)
  
  # Save forecast
  saved_file <- FLAREr:::write_restart(da_forecast_output = da_forecast_output,
                                       forecast_output_directory = config$file_path$restart_directory,
                                       use_short_filename = TRUE)
  
  forecast_df <- FLAREr:::write_forecast(da_forecast_output = da_forecast_output,
                                         use_s3 = use_s3,
                                         bucket = config$s3$forecasts_parquet$bucket,
                                         endpoint = config$s3$forecasts_parquet$endpoint,
                                         local_directory = file.path(lake_directory, "forecasts/parquet"))
  
  targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-rs.csv")),show_col_types = FALSE)
  
  targets_df <- obs_config |>
    dplyr::rename(variable = target_variable) |>
    dplyr::select(variable, obs_sd) |>
    right_join(targets_df, by = "variable") |>
    dplyr::mutate(up95 = observation + 1.96 * obs_sd,
           low95 = observation - 1.96 * obs_sd,
           low95 = ifelse(variable != "temperature" & low95 < 0, 0, low95))
  
  FLAREr:::plotting_general(forecast_df, targets_df, file_name = paste0(tools::file_path_sans_ext(basename(saved_file)),".pdf") , plots_directory = config$file_path$plots_directory)
}

