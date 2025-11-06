# Source Folder 

Code that _can_ be removed from the report, should be. And it should be stored here in `/src/`. 

There will, inevitably be work that you have to do to transform, structure, and clean each data set that you use. The primary  source data should be stored in (or downloaded to upon making) the `/data/raw/` folder. Code that cleans, structures, renames, etc this raw data should be stored in `/src/data/` and should have the goal of cleaning each data set and moving a cleaned version of this dataset into `/data/interim/`. 

Once each piece of your data is cleaned and structured in `/data/interim/` you can proceed to merge the data and write your analysis dataset into `/data/processed/`. 

There may be other work that you do in the course of conducting your project that doesn't belong in the Report. Code that does this work should be stored here. 
