## Download data directly from CMS and unzip the bundle

library(tidyverse)

setwd("~/Documents/PhD/Projects/medicare-quality/physician-compare")

## Here are the data
URL <- "https://data.medicare.gov/views/bg9k-emty/files/02ffa617-c42c-4766-8d53-4911e3ecd987?content_type=application%2Fzip%3B%20charset%3Dbinary&filename=Physician_Compare.zip"

## NB this takes some time, the zip is about 170mb and the uncompressed file is about 700mb.
download.file(url = URL, destfile = "physician-compare-bundle.zip", method = "curl")

## Unzip bundle to the working directory
system("unzip -o physician-compare-bundle.zip")

system("rm -rf physician-compare-bundle.zip")


