######################################
# Grab MISR 4.4km global data from LARC
# Change dates.range
# Pick MISR paths in nc.list
# see https://eosweb.larc.nasa.gov/PRODOCS/misr/images/paths/
######################################

require(ncdf4)
require(data.table)
require(XML)
require(downloader)
setwd('Volumes/Projects/Satellite/MISR/')

# steps to get list of relevant NC file download links from OPeNDAP -------
# ## grabNcdfUrl is a function that takes in a MISR OPeNDAP url and
# ## returns vector of URL(s) to NC file in chosen paths

grabNcdfUrl = function(url){
  # the html file names have dates in them, we want that
  date = substr(url, 6, 15)
  # reads html table of the OPeNDAP page, collects the first column (file names)
  nc.list = readHTMLTable(htmlParse(url),
                          stringsAsFactors=F)[[1]][,1]

  # collect files within paths 008:050, also exclude XMLs (strings longer than 44)
  nc.list = nc.list[substr(nc.list,22,24) %in% sprintf('%03d', 008:050) & 
                      nchar(nc.list) == 44]
  if(length(nc.list) == 0){
    # if vector is empty, returns it
    return(nc.list)
  } else{
    # otherwise, add the URL heading to complete URL for each NC file.
    return(paste0(
      'https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/',
      date, '/', nc.list))
  }
}


setwd('Volumes/Projects/Satellite/MISR/ouput')
# create sequence of dates to generate HTML links (replace '-' with '.')
dates.range = seq.Date(as.Date('2017-03-02'), as.Date('2018-05-31'), 'days')
url.dates = gsub('-','.',dates.range)
# create vector of sequential URL links by date
OPeNDAP.urls = paste0('https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/',
                      url.dates, '/contents.html')



# loop to download OPeNDAP webpages (for scraping later on)
system.time({
  sapply(OPeNDAP.urls, function(x) tryCatch({
    # download OPeNDAP webpage
    downloader::download(x, paste0('urls/',substr(x,66,75), '.contents.html'), quiet=T)
  }, error = function(cond){
    # in case the page does not exist, print date
    message(paste0(substr(x,66,75), ': URL does not exist.'))
  })
  )
})

# collect URLs of OPeNDAP pages
misr.urls = paste0('urls/', list.files('urls'))
# misr.urls = paste0('urls/', list.files('urls', pattern='contents'))
# 
# # go through each page, parse the HTML table and grab relevant NC files.
system.time({
  ncdf.urls = sapply(misr.urls, function(x) grabNcdfUrl(x))
})

ncdf.urls = unlist(ncdf.urls, use.names = F)
# saveRDS(ncdf.urls, 'ncdf_urls.rds')
# download ncdf files
ncdf.urls = readRDS('ncdf_urls.rds')
indices = 1:10
for(i in indices){
  start.time=Sys.time()
  cat('Downloading ',sprintf('%04d',i),'/',max(indices),' - ',
      substr(ncdf.urls[i],66,75),': ',sep='')
  tryCatch({
    download.file(ncdf.urls[i], 
                  paste0('ncdf/', substr(ncdf.urls[i],77,nchar(ncdf.urls[i]))),
                  mode='wb', quiet=T)
    dl.time = Sys.time() - start.time
    if(dl.time < 10){dl.time=round(dl.time*60,2)} else{dl.time=round(dl.time,2)}
    cat(dl.time,' secs...\n',sep='')
  }, error = function(cond){
    cat(substr(ncdf.urls[i],77,nchar(ncdf.urls[i])), 'failed to download.\n')
  })
}

