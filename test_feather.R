# https://blog.rstudio.com/2016/03/29/feather
# https://cran.r-project.org/web/packages/feather/readme/README.html

  
library(tidyverse)
library(feather)

# basic usage ####

# write_feather(mtcars, "mtcars.feather")
# mtcars2 <- read_feather("mtcars.feather")



# test more complex data
x <- runif(1e7)
x[sample(1e7, 1e6)] <- NA # 10% NAs

df <- as.data.frame(replicate(10, x))

write_feather(df, 'test.feather')

system.time(read_feather('test.feather'))

myfeatherdf <- read_feather('test.feather')
