#install "checkpoint" and "here" to perform package setup 
install.packages("checkpoint")
install.packages("here")

#use "here" to set package folder
library("here")
dir.create(file.path(here(), ".checkpoint_pkg"), recursive = TRUE, showWarnings = FALSE)
options(install.packages.compile.from.source = "no")

#use "checkpoint" to install the correct version of all the needed packages 
library("checkpoint")
checkpoint("2018-09-26", project = here(), checkpointLocation = ".checkpoint_pkg")
