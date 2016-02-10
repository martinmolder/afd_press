# This is a script to automatically downlaod and save the press releases of the Alternative für Deutchland party from their website https://www.alternativefuer.de/

# The code is quick and dirty, but seems to work.

# Martin Mölder

# Updated: 10.02.2016

library(XML)

# Specify the main link for the web page. This will be used to paste together the full links to the sub-pages that contain the press releases. 

main_link <- "https://www.alternativefuer.de/"

# This section will extract the ealiest year-month and the latest year month that the press releases are linked to in the archinve and use them as the range. For a smaller range, these values can be set manually as well, but this section should automatically select the earliest and the latest. 

arch_dates <- readLines("https://www.alternativefuer.de/pr-social-media/fuer-medienvertreter/pressemitteilungen/", warn=F)

first_row <- max(grep("<li><a href=", arch_dates))
last_row <- min(grep("<li><a href=", arch_dates))

start_year <- unlist(strsplit(arch_dates[first_row], "/"))[4]
start_month <- unlist(strsplit(arch_dates[first_row], "/"))[5]

end_year <- unlist(strsplit(arch_dates[last_row], "/"))[4]
end_month <- unlist(strsplit(arch_dates[last_row], "/"))[5]

#start_year <- "2013"
#start_month <- "04"

#end_year <- "2016"
#end_month <- "02"

# This created the yearly and monthly sequences that make up the links of the archive.

years <- as.numeric(start_year):as.numeric(end_year)
months <- c(paste("0", 1:9, sep=""), "10", "11", "12")

# Paste the years and months together.

range <- as.vector(sapply(years, function(x) paste(x, months, sep="/")))

# Adjust the range so that it fits the data in the archive. 

range <- range[-(1:which(range==paste(start_year, start_month, sep="/"))-1)]
range <- range[-((which(range==paste(end_year, end_month, sep="/"))+1):length(range))]

# Extract the number of press realeases for each year-month.

temp_nr <- arch_dates[grep("<li><a href='https:", arch_dates)]
temp_nr <- sapply(temp_nr, function(x) substr(x, nchar(x)-8, nchar(x)))
temp_nr <- unlist(regmatches(temp_nr, gregexpr("[[:digit:]]+", temp_nr)))
names(temp_nr) <- NULL
temp_nr <- rev(temp_nr)

# The following loop, which is rahter long, I must admit, does everything else -- for each year month, extract all the press releases for that month and saves them as text files. 

# NB The working directory is not set, so they will be saved into whatever the working directory in R at that point happens to be. 

N <- length(range) # This is the total number of months across the years.

for (i in 1:N){
    
    year <- substr(range[i],1,4)
    month <- substr(range[i],6,7)
    
    # This pastes together the full link for each year month.
    
    link <- paste(main_link, range[i], sep="")
    
    # If there are more than 10 press releases, then they will not be displayed on the same page. In this case we have to do the following. 
    
    if (as.numeric(temp_nr[i]) > 10){
        
        # How many pages are there, given the number of press releases for that month? (Each containing 10)
        
        extra <- 1:ceiling(as.numeric(temp_nr[i])/10)
        
        # For each of these extra pages, do the following.
        
        for (k in extra){
            
            # For the first page we do this. 
          
            if (k==1) {
                
                # Read in the web page.
              
                raw <- readLines(link, warn=F)
                
                # Parse it and work your way to the text of the press release, which are under the node "article".
                
                parsed <- htmlTreeParse(raw, useInternal=T)
                
                root <- xmlRoot(parsed)        
                
                body <- root[["body"]]
                
                articles <- getNodeSet(body, "//article")
                
                n <- length(articles)
                
                # Extract the titles and the links for each individual press release. 
                
                headers <- lapply(articles, function(x) x[["header"]])
                headers2 <- lapply(headers, function(x) x[["h1"]])
                links <- lapply(headers2, function(x) x[["a"]])
                
                titles <- unlist(lapply(links, xmlValue))
                links <- unlist(lapply(links, function(x) xmlGetAttr(x,name = "href")))
                
                # Read in the web pages of each individual press release.
                
                temps <- sapply(links, readLines, warn=F)
                
                # I do not remember what was the occasion for which this was necessary.
                
                if (class(temps)=='matrix'){
                    
                    temps <- list(temps[,1])
                    
                }
                
                # Parse all the pages and work your way to the text of the article. 
                
                temps <- lapply(temps, function(x) htmlTreeParse(x, useInternal=T))
                temps <- lapply(temps, xmlRoot)
                temps <- lapply(temps, function(x) x[["body"]])
                locations <- unlist(lapply(temps, function(x) which(unlist(xpathApply(x, "//div[@class]", xmlGetAttr, "class"))=="entry-content beitrag")))
                texts <- sapply(1:length(temps), function(x) xpathApply(temps[[x]], "//div[@class]", xmlValue)[[locations[x]]])
                
                # Remove some junk characters.
                
                texts <- sapply(texts, function(x) gsub("\n", " ", x))
                texts <- sapply(texts, function(x) gsub("^\\ +", "", x))
                texts <- sapply(texts, function(x) gsub("\\ +$", "", x))
                names(texts) <- NULL
                
                # For each article, put together the title and the text.

                all <- sapply(1:n, function(x) c(titles[x], "\n", texts[x]))
                
                # Save them with year and month and the number of the press release as the title of the file.
                
                for (j in 1:n){
                    
                    writeLines(all[,j], paste(year, "_", month, "_", j, ".txt", sep=""))
                    
                }
            
            # For all the next pages we do this. 
                    
            } else {
                
                link_extra <- paste(link, "/page/", k, sep="")
                
                # Read in the web page.
                
                raw <- readLines(link_extra, warn=F)
                
                # Parse it and work your way to the text of the press release, which are under the node "article".
                
                parsed <- htmlTreeParse(raw, useInternal=T)
                
                root <- xmlRoot(parsed)        
                
                body <- root[["body"]]
                
                articles <- getNodeSet(body, "//article")
                
                n <- length(articles)

                # Extract the titles and the links for each individual press release. 
                
                headers <- lapply(articles, function(x) x[["header"]])
                headers2 <- lapply(headers, function(x) x[["h1"]])
                links <- lapply(headers2, function(x) x[["a"]])
                
                titles <- unlist(lapply(links, xmlValue))
                links <- unlist(lapply(links, function(x) xmlGetAttr(x,name = "href")))
                
                # Read in the web pages of each individual press release.
                
                temps <- sapply(links, readLines, warn=F)
                
                # I do not remember what was the occasion for which this was necessary.
                
                if (class(temps)=='matrix'){
                    
                    temps <- list(temps[,1])
                    
                }
                
                # Parse all the pages and work your way to the text of the article.
                
                temps <- lapply(temps, function(x) htmlTreeParse(x, useInternal=T))
                temps <- lapply(temps, xmlRoot)
                temps <- lapply(temps, function(x) x[["body"]])
                locations <- unlist(lapply(temps, function(x) which(unlist(xpathApply(x, "//div[@class]", xmlGetAttr, "class"))=="entry-content beitrag")))
                texts <- sapply(1:length(temps), function(x) xpathApply(temps[[x]], "//div[@class]", xmlValue)[[locations[x]]])
                
                # Remove some junk characters.
                
                texts <- sapply(texts, function(x) gsub("\n", " ", x))
                texts <- sapply(texts, function(x) gsub("^\\ +", "", x))
                texts <- sapply(texts, function(x) gsub("\\ +$", "", x))
                names(texts) <- NULL
                
                # For each article, put together the title and the text.
                
                all <- sapply(1:n, function(x) c(titles[x], "\n", texts[x]))
                
                # Save them with year and month and the number of the press release as the title of the file.
                
                for (j in 1:n){
                    
                    writeLines(all[,j], paste(year, "_", month, "_", j+(k-1)*10, ".txt", sep=""))
                    
                }
                
            }
            
        }
    
    # If there are less than 10 press releases for the month, then they are on the same web page and we do this.    
            
    } else {
        
        # Read in the web page.
      
        raw <- readLines(link, warn=F)
        
        # Parse it and work your way to the text of the press release, which are under the node "article".
        
        parsed <- htmlTreeParse(raw, useInternal=T)
        
        root <- xmlRoot(parsed)        
        
        body <- root[["body"]]
        
        articles <- getNodeSet(body, "//article")
        
        n <- length(articles)

        # Extract the titles and the links for each individual press release. 
        
        headers <- lapply(articles, function(x) x[["header"]])
        headers2 <- lapply(headers, function(x) x[["h1"]])
        links <- lapply(headers2, function(x) x[["a"]])
        
        titles <- unlist(lapply(links, xmlValue))
        links <- unlist(lapply(links, function(x) xmlGetAttr(x,name = "href")))
        
        # Read in the web pages of each individual press release.
        
        temps <- sapply(links, readLines, warn=F)
        
        # I do not remember what was the occasion for which this was necessary.
        
        if (class(temps)=='matrix'){
            
            temps <- list(temps[,1])
            
        }
        
        # Parse all the pages and work your way to the text of the article.
        
        temps <- lapply(temps, function(x) htmlTreeParse(x, useInternal=T))
        temps <- lapply(temps, xmlRoot)
        temps <- lapply(temps, function(x) x[["body"]])
        locations <- unlist(lapply(temps, function(x) which(unlist(xpathApply(x, "//div[@class]", xmlGetAttr, "class"))=="entry-content beitrag")))
        texts <- sapply(1:length(temps), function(x) xpathApply(temps[[x]], "//div[@class]", xmlValue)[[locations[x]]])
        
        # Remove some junk characters.
        
        texts <- sapply(texts, function(x) gsub("\n", " ", x))
        texts <- sapply(texts, function(x) gsub("^\\ +", "", x))
        texts <- sapply(texts, function(x) gsub("\\ +$", "", x))
        names(texts) <- NULL
        
        # For each article, put together the title and the text.
        
        all <- sapply(1:n, function(x) c(titles[x], "\n", texts[x]))
        
        # Save them with year and month and the number of the press release as the title of the file.
        
        for (j in 1:n){
            
            writeLines(all[,j], paste(year, "_", month, "_", j, ".txt", sep=""))
            
        }
        
    }
    
}




