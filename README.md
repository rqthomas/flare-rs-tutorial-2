# flare-rs-tutorial
Repo to test running FLARE user tool for CEF 🛰️

To run this code:

✦ Clone the repo

✦ Input your desired lake info and date range in 01LakeInfo (or use one of the pre-selected lakes)

✦ Run 02GetInputs to get necessary inputs (remote sensing data, met data, etc) for FLARE

✦ Run 03RunFLARE to get your forecasts, which you can look at in /plots.


Note: The GLM binary used in this repo (/binary/macos/glm) is for MacOS users only. 
For Windows or Linux users, please replace this binary with the appropriate GLM file 
(see https://github.com/AquaticEcoDynamics/glm-aed).


## To do list

- Generate initial temperature profile from user supplied information about whether it is stratified, a calculation of thermocline depth from fetch (fetch can be calculated using the bathyometry), and value for the deep water temperature.  These can be combined to create a profile.
- Get the SWOT depth working
