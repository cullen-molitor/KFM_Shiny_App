# KFM Shiny Application

![](App/www/Photos/Kelp_Forest_Scenes/Laurie_Montgomery/1%20(2).jpg "Diver in Kelp forest of Santa Barbara Island")

Photo: Laurie Montgomery

## Data Visualization and Modeling of the Kelp Forest Monitoring Data collected by Channel Islands National Park
This is my second attempt at designing a shiny web application for the KFMP at Channel Islands National Park (CINP). This tool is designed to do exploratory data analysis through interactive data visualization and statistical modeling.

A working example of this app can be found at [Cloud Run App](https://app-4oc2gi4bqq-uw.a.run.app/) or [GKE Cluster](http://34.94.133.101/). I am currently working on finding the best/fastest/cheapest solution to hosting this in the cloud and these are my first two attempts. The Cloud Run service is updated regularly (whenever commits are pushed to github) while the GKE Cluster is not so there will be differences between the two.

# File Organization of Repository

This repository is organized with two sub-folders `App/` and `Raw_Data/` located in the `root`. 

## `root` Folder

The root of this directory contains a `dockerfile` used for building the app container image as well as several `.R` files for working with raw data. The `.R` files produce the necessary tidy data used by the app, satellite maps from google with transect information overlaid, and text summaries of the annual reports. These files take data from the `Raw_Data/` folder and produce outputs that get stored in the `App/` folder. 

## `Raw_Data/` Folder

This contains the raw data exported from the KFM database and a single subfolder `Shapefiles/` with the raw geospatial data. Everything in here is cleaned up and saved within the `App/` folder. None of these files are included with the final app.  

## `App/` Folder

This folder houses the application files, data, images, and PDFs used to build it. 

The four most important files are located directly in the `App/` folder

- `global.R`

  - This is the primary file for loading in stored data objects vectors used in the app

- `ui.R`

  - This builds the `Shiny` user interface of the app

- `server.R` 

  - This serves the app the data and produces the graphics

- `modules.R` 

  - This stores the modules used to build the app

There are several folders, and some have sub-folders

- `GIS_Data/`

  -  This houses the `.gpkg` files used in the maps
  
- `Meta_Data/`

  - This holds the `.csv` files used for creating tables and cross referencing data in the app
  
- `Models/` 

  - This holds the `.rds` files from our Random Forest models
  
- `Text/` 

  - This holds all of the `.md` files used to create the text chunks in the app
  
- `Tidy_Data/` 

  - This holds the `.feather` files that hold the various data frames used for the bulk of the plots
  
- `www/`

  - This holds most of the `.jpg`, `.png`, and `.pdf` files embedded in the app itself including
  
  - This includes `Annual_Reports/`, `Graphics/`, `Handbook/`, `Maps/`, and `Photos/`
  
  - Kind of a catch all for extra things
