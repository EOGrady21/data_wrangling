########################################################################
# Author: Reid Steele (Reid.Steele@dfo-mpo.gc.ca)                      #
# Purpose: Database and clean raw PP data files for all regions        #
# Date: Wed March 18, 2020                                             #
########################################################################

# Libraries
library(data.table)

# Load in data
seawifs = fread('../Data/PP/1998-2007-seawifs-spring-bloom.csv', skip = 'Region')
modis = fread('../Data/PP/2003-2017-modis-spring-bloom.csv', skip = 'Region')
viirs = fread('../Data/PP/2012-2019-viirs-spring-bloom.csv', skip = 'Region')

# Add survey identifier
seawifs.i = data.table(seawifs, 'seawifs')
modis.i = data.table(modis, 'modis')
viirs.i = data.table(viirs, 'viirs')

# Combine
ppraw_w = rbind(seawifs.i,modis.i,viirs.i)

# Rename columns
colnames(ppraw_w) = c('region', 'year', 'start', 'amplitude', 'duration', 'magnitude', 'label')

# gsub out '_' for ' '
ppraw_w$region = gsub('_', ' ', ppraw_w$region)

# Separate response variables to create _l version

  ## Bloom start
start = ppraw_w[,c('region', 'year', 'label', 'start')]
start = data.table(start[,1:3], 'start', start[,4])
colnames(start) = c('region', 'year', 'label', 'variable', 'value')

  ## Bloom amplitude
amplitude = ppraw_w[,c('region', 'year', 'label', 'amplitude')]
amplitude = data.table(amplitude[,1:3], 'amplitude', amplitude[,4])
colnames(amplitude) = c('region', 'year', 'label', 'variable', 'value')

  ## Bloom duration
duration = ppraw_w[,c('region', 'year', 'label', 'duration')]
duration = data.table(duration[,1:3], 'duration', duration[,4])
colnames(duration) = c('region', 'year', 'label', 'variable', 'value')

  ## Bloom magnitude
magnitude = ppraw_w[,c('region', 'year', 'label', 'magnitude')]
magnitude = data.table(magnitude[,1:3], 'magnitude', magnitude[,4])
colnames(magnitude) = c('region', 'year', 'label', 'variable', 'value')

# rbind to create _l raw data file
ppraw_l = rbind(start, amplitude, duration, magnitude)
