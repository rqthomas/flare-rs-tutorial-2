################################################################################
# Code started by Molly Stroud on 12/16/25
# Download data from dynamical.org and put into correct formatting for FLARE
# https://dynamical.org/catalog/noaa-gefs-forecast-35-day/
#
# This used to drive Python via reticulate, which broke on other machines due
# to reticulate/virtualenv version and configuration mismatches. All of the
# xarray/zarr/pandas logic now lives in R/get_met.py (pure Python, no
# reticulate). This file just provides helpers to set up a dedicated Python
# virtual environment and run get_met.py as a subprocess; 02Get_Inputs.R calls
# .run_get_met_py() directly for stage 2 / stage 3 downloads.
################################################################################

.get_met_py_script <- file.path("R", "get_met.py")
.get_met_venv <- file.path(getwd(), ".venv-met")

# path to the python executable inside the dedicated virtual environment
.get_met_venv_python <- function() {
  if (.Platform$OS.type == "windows") {
    file.path(.get_met_venv, "Scripts", "python.exe")
  } else {
    file.path(.get_met_venv, "bin", "python3")
  }
}

# make sure a python3 interpreter + dedicated venv with the right packages exist
.ensure_met_python_env <- function() {
  system_python <- Sys.which("python3")
  if (system_python == "") {
    stop(
      "python3 was not found on your PATH. Please install Python 3 ",
      "(https://www.python.org/downloads/) and try again."
    )
  }

  venv_python <- .get_met_venv_python()

  if (!file.exists(venv_python)) {
    message("Creating a dedicated Python environment for met data downloads (.venv-met)...")
    result <- system2(system_python, c("-m", "venv", shQuote(.get_met_venv)))
    if (result != 0) {
      stop("Failed to create the Python virtual environment used by get_met.py.")
    }
  }

  marker <- file.path(.get_met_venv, "packages_installed.txt")
  if (!file.exists(marker)) {
    message("Installing required Python packages for met data downloads (this only happens once)...")
    packages <- c(
      "dask==2025.1.0",
      "xarray[complete]==2026.1.0",
      "zarr==3.0.8",
      "certifi",
      "numpy",
      "pandas",
      "pyarrow",
      "requests",
      "aiohttp"
    )
    system2(venv_python, c("-m", "pip", "install", "--quiet", "--upgrade", "pip"))
    result <- system2(venv_python, c("-m", "pip", "install", "--quiet", packages))
    if (result != 0) {
      stop("Failed to install the Python packages required by get_met.py.")
    }
    writeLines("ok", marker)
  }

  venv_python
}

# run get_met.py with the given CLI args, stopping with a clear error on failure
.run_get_met_py <- function(args) {
  venv_python <- .ensure_met_python_env()
  result <- system2(venv_python, c(shQuote(.get_met_py_script), as.character(args)))
  if (result != 0) {
    stop("get_met.py failed. See the messages above for details.")
  }
  invisible(result)
}

