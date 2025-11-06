# lab_2

The final project for MIDS w203. In this lab, students will apply what they have learned about building linear models to produce a report that analyzes a specific research question. 

# Assignment Prompt

The assignment prompt is in `./prompt/assignment.qmd` and rendered in `./prompt/assignment.pdf`. 

# Project Organization

We have created a folder structure to help you organize your work. This is based on [cookiecutter data science](https://drivendata.github.io/cookiecutter-data-science).


    ├── LICENSE
    ├── README.md          <- The top-level README describing the project aims
    ├── data
    │   ├── external       <- Data from third party sources.
    │   ├── interim        <- Intermediate data that has been transformed.
    │   └── processed      <- The final, canonical data sets for modeling.
    ├── prompt             <- The assignment prompt.
    ├── peer_review        <- A starting place for each person's peer evaluation. 
    ├── notebooks          <- .Rmd notebooks. 
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    └── src                <- Source code for use in this project.
        └── data           <- Scripts to download or generate data

## How we would use this structure. 

1. At the beginning of your assignment, you and your team are going to be working on several things, all at the same time. You're going to be exploring data, asking questions, and learning about what is going on. All of this work is nicely contained in the `./notebooks/` section of folder. `./notebooks`, when it is included on the `main` branch of the repository should be more than just your first, very messy look at some question -- you should have brought it at least to the point that others can follow your logic and potentially contribute to your code. 

2. In the `./data/` folder and its sub-folders go all the data that you're going to use or derive for use in this project. We have added any `.csv` file in this folder structure to the `.gitignore` file, which means that they will not be stored in version control. This is a best practice, because version control should really be about storing code and not data. 

3. We have built this around a "Project" in Rstudio. These projects allow you to isolate all of your code, but also much of your environment to that specific project. To manage the environment, we have used the `renv` package, and to give you a starting point, we have installed many of the packages that you might be interested in using: `tidyverse` (which includes `dplyr` and `ggplot2`), `here` for locating your data (more on that in a moment), `data.table` if you've got GB of data, but also packages like `sandwich` and `lmtest` which we have used earlier in the semeseter. In order for _you_ to install these packages on the computer you're using, you will simply need to issue `renv::restore()` once, and these will install. 

4. We recommend that you use the `here` package [link](https://here.r-lib.org) for all of your file paths. Here is why: You're working on a team of 3-4 people, and each of you have different locations where you're working on your computer. This is very frustrating to collaborate, and it leads to just enough friction that you  won't end up actually collaborating. `here` starts from the top-level of the project, but no deeper, and then creates conforming file paths. If you were looking to reference your raw data, in any file, you would do so by issuing `here("data", "raw", "my_data.csv")` where `"my_data.csv"` is actually the name of the file that you're trying to load. 

5. We would recommend that you write a small service to yourselves -- perhaps storing this in `./src/` that takes on each step of your data gathering and cleaning pipeline. It is really, really nice to be able to interactively work with data while you're modeling. However, as your project develops, we would _strongly_ recommend that you move this work to clean and transform your data to as early as possible, and certainly before you move into your `Report.Rmd` file. Here's why: (a) As a team, you need to know what the canonical form of "this variable" is; you can't have two competing versions; (b) As a contributor, you want to know what is available to you, and what you've got to derive anew -- if there are no new variable derivations in the modeling code, then you know that any new thing you create has to get moved into a new place at some point; (c) as a contributor, the modeling gets _wild_ every. single. time. You want to isolate the hard work that you're doing to read people's modeling code from the sometimes unclear work of the variable transforms. 
