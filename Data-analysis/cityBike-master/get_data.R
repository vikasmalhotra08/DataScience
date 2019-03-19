
library(RCurl)
library(XML)

get_data.f <- function(l){
  folder <- './raws'
  dir.create(file.path(folder), showWarnings = FALSE)
  for(i in 1:length(links)){
    x <- links[i]
    filename <- paste(folder, strsplit(x, '/')[[1]][5], sep = '/')
    download.file(url = x,destfile = filename, method = "wget")
    unzip(filename, exdir = "./raws")
    file.remove(filename)
  }  
}

script <- getURL('http://www.citibikenyc.com/system-data')
doc <- htmlParse(script)
links <- getHTMLLinks(doc)
links <- links[grepl('zip', links)]
lapply(links, get_data.f)