ptm <- proc.time() # times how long the process takes (in seconds). At the end
# of the code is the flip side of this. If you don't care or aren't interested, 
# rem out both statements.

###############################################################################
###############################################################################
# Load Funnctions required for elsewhere
getFileNames <- function(kPath, patt, all_ = T, full_ = T, recur_ = T){
   list.files(path = kPath,
              pattern = patt,
              all.files = all_,
              full.names = full_,
              recursive = recur_)
              }

ImageRead <- function(image.name){
   if ( image_type(image.name) == "bmp"  | image_type(image.name) == "BMP") {
      hex2dec(gsub("#","",as.vector(as.raster(read.bitmap(image.name) / 255))))
      
   } else if ( image_type(image.name) == "png"  | image_type(image.name) == "PNG" ) {
      hex2dec(gsub("#","",as.vector(as.raster(read.bitmap(image.name) * 1 ))))
      
   } else
      hex2dec(gsub("#","",as.vector(as.raster(read.bitmap(image.name)))))
   }

MyMode <- function(.data){
   as.numeric(names(table(.data)[which(max(table(.data)) == table(.data))]))
   }

##############################################################################
##############################################################################
# User defined variables

# Set path to the known and unknown images. Must be full path and slashes must
# "/" . Use the "?" key
k.Files.Path <- "G:/Images/Known"
u.Files.Path <- "G:/Images/Unknown"

# Determine how many bins the color palette should be cut into. This is
# a power of 2. For instance, pal.Power = 2 means there will be 2^2 bins.
# Whereas, pal.Power = 8 means there will be 2^8 bins. 2^20 seems to be about
# the maximum bins. Most testing is done around 2^12 and seems to be effective
pal.Power <- 12

# Reporting threshold - not currently used
# This is a number that determines the minimum results threshold that will 
# be reported for further examination. A value of 0 will pass everything.
# If this value is small then the 
# number of records reported out for review could be massive. This value cannot
# exceed 1.0. 
reporting.threshold <- 0

# set extensions of files you want include in the analysis. Most combinations
# are avalable here. Rem out the patterns you don't want so that all that's left
# is the one you do want. If more than one is un-remmed then only the last will
# apply.
# pattern.ext <- "^.*(\\.jpg|\\.JPG|\\.jpeg|\\.JPEG|\\.bmp|\\.BMP|\\.png|\\.PNG)$"
 pattern.ext <- "^.*(\\.jpg|\\.JPG|\\.jpeg|\\.JPEG|\\.bmp|\\.BMP)$"
# pattern.ext <- "^.*(\\.jpg|\\.JPG|\\.jpeg|\\.JPEG|\\.png|\\.PNG)$"
# pattern.ext <- "^.*(\\.jpg|\\.JPG|\\.jpeg|\\.JPEG)$"
# pattern.ext <- "^.*(\\.jpg|\\.JPG|\\.jpeg|\\.JPEG)$"
# pattern.ext <- "^.*(\\.bmp|\\.BMP|\\.png|\\.PNG)$"
# pattern.ext <- "^.*(\\.bmp|\\.BMP)$"
# pattern.ext <- "^.*(\\.png|\\.PNG)$"

# End of user defined variables
##############################################################################
##############################################################################

.libPaths("G:/RPack")
library(readbitmap)
library(broman) # required for hex2dec

# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Check for updated version of readbitmap
# if (!require("devtools")) install.packages("devtools")
# devtools::install_github('jefferis/readbitmap','jefferis')
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# options(digits=22) # temp for testing
# setwd("C:/Users/toronto$123/Documents/R/ImageTesting")

# Make empty DF to hold known filenames, paths and binning results
k.Files.df <- data.frame("k.image.name" = character(),
                         "k.image.bytes" = integer(),
                         "k.image.palette.prop" = double(),
                         "k.image.mean" = numeric(), # reserved for future research
                         "k.image.max" = numeric(), # reserved for future research
                         "k.image.min" = numeric(), # reserved for future research
                         "k.image.mode" = numeric(), # reserved for future research
                         "k.image.median" = numeric(), # reserved for future research
                         "k.image.sd" = numeric(), # reserved for future research
                         # Adds sufficient columns to dataframe to 
                         # accommodate the numbers of groups
                         # the 2^24 palette is carved into
                         data.frame(matrix(nrow = 0, ncol = 2 ^ pal.Power)),
                         stringsAsFactors = FALSE
                        )

# Make empty DF to hold known filenames and paths
u.Files.df <- data.frame("u.image.name" = character(),
                         "u.image.bytes" = integer(),
                         "u.image.palette.prop" = numeric(),
                         "u.image.mean" = numeric(), # reserved for future research
                         "u.image.max" = numeric(), # reserved for future research
                         "u.image.min" = numeric(), # reserved for future research
                         "u.image.mode" = numeric(), # reserved for future research
                         "u.image.median" = numeric(), # reserved for future research
                         "u.image.sd" = numeric(), # reserved for future research
                         # Adds sufficient columns to dataframe to 
                         # accommodate the numbers of groups
                         # the 2^24 palette is carved into
                         data.frame(matrix(nrow = 0, ncol = 2 ^ pal.Power)),
                         stringsAsFactors = FALSE
                        )

# Make empty DF to hold results of comparison
out.Info.df <- data.frame("k.File" = character(),
                          "u.File" = character(),
                          "match.cor" = numeric(),
                          stringsAsFactors = FALSE
                        )

k.Files <- getFileNames(k.Files.Path,pattern.ext,recur = T)

u.Files <- getFileNames(u.Files.Path,pattern.ext,recur = T)



for (i in seq(k.Files)){

   temp.File <- ImageRead(k.Files[i])
   
   k.Files.df[i,] <- c(0, # filename
                    length(temp.File), # total pixels
                    length(unique(temp.File)) / 2^24, #proportion of palette
                    mean(subset(temp.File, temp.File > 0)), # reserved for future research 
                    max(subset(temp.File, temp.File > 0)), # reserved for future research
                    min(subset(temp.File, temp.File > 0)), # reserved for future research
                    MyMode(temp.File), # reserved for future research
                    median(temp.File), # reserved for future research
                    sd(temp.File), # reserved for future research
                    as.integer(table(cut(temp.File, breaks = 2 ^ pal.Power)))
                  )
                  k.Files.df[i,1] <- k.Files[i]
                  }

for (i in seq(u.Files)){
   
   temp.File <- ImageRead(u.Files[i])
   
   u.Files.df[i,] <- c(0, # filename
                       length(temp.File), # total pixels
                       length(unique(temp.File)) / 2^24, #proportion of palette
                       mean(subset(temp.File, temp.File > 0)), # reserved for future research 
                       max(subset(temp.File, temp.File > 0)), # reserved for future research
                       min(subset(temp.File, temp.File > 0)), # reserved for future research
                       MyMode(temp.File), # reserved for future research
                       median(temp.File), # reserved for future research
                       sd(temp.File), # reserved for future research
                       as.numeric(table(cut(temp.File, breaks = 2 ^ pal.Power)))
                        )
                        u.Files.df[i,1] <- u.Files[i]
                        }

z <- 0
for (i in seq(nrow(k.Files.df))){
   
   for (j in seq(nrow(u.Files.df))){
      z <- z + 1
      out.Info.df[z,] <- c(0,
                           0,
                           as.numeric(cor(as.numeric(k.Files.df[i,10:ncol(k.Files.df)]),as.numeric(u.Files.df[j,10:ncol(u.Files.df)])))
                           )
      out.Info.df[z,1] <- k.Files.df[i,1]
      out.Info.df[z,2] <- u.Files.df[j,1]
      
   }
}
proc.time() - ptm


