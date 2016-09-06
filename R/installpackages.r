##
## [installpackages.r]
##
## author     : Ed Goodwin
## project    : helpeR
## createdate : 06.11.2016
##
## description:
##    simple script to install packages I use
##
## version: 0.01
## changelog:
##

packlist = c("ggplot2",
             "ggmap",
             "tidyr",
             "maptools",
             "quantmod",
             "dplyr",
             "class",
             "stringr",
             "reshape",
             "lubridate",
             "fma",
             "caret",
             "rpart",
             "rpart.plot",
             "RColorBrewer",
             "rattle",
             "ggthemes",
             "PerformanceAnalytics",
             "rvest",
             "shiny",
             "httpuv",
             "devtools",
             "testthat",
             "purrr",
             "RMySQL",
             "rglpk")

install.packages(packlist)