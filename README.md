### [Bergelson Lab] Shiny for CDI & Motor Datasets

These are R scripts for Shiny app. CDI & Motor Datasets aren't publicly released at this point. Please change the CDI & Motor file paths (the "load data" section of `clean_data.R`)  to your local ones, and set the file path where you want to save the cleaned datasets (the "save cleaned data" section of `clean_data.R1`, and the "load datasets" section of `pre_processing.R`)

To run the Shiny app:
* Run `clean_data.R` if it's your first time running this app (one-time)；
* Run `pre_processing.R` script first;
* Open either `ui.R` or `server.R` script, and click the `Run App` button in Rstudio.
