#Download the data
#First lets do a little side project to build an autonaming download function
#it should take the last part of the URL and make it into the filename

smartDownload <- function(address,folder = "."){
        if(!dir.exists(folder)) dir.create(folder)
        name <- gsub("%[0-9]{2}","",address) #remove those weird %xx expressions
        name <- gsub("^.*/","",name)
        download.file(address, destfile = paste(folder,name,sep="/"))
}

xlsxToDT <- function(filePath){
        require(data.table)
        require(magrittr)
        sheetNames <- excel_sheets(filePath)
        Data.Tables <- lapply(sheetNames,function(x)read_xlsx(filePath,sheet = x) %>%
                                      as.data.table()) 
        names(Data.Tables) <- sheetNames
        Data.Tables
}

AddAllVarNames <- function(dictionaryDT){
        dictionaryDT[,initialOrder := 1:.N]
        setkey(dictionaryDT,initialOrder)
        varNames <- dictionaryDT[,unique(`VARIABLE NAME`)]
        #store all of the row numbers for each of the first instances of each variable
        rowstoNameBy <- dictionaryDT[!is.na(`VARIABLE NAME`),initialOrder]
        # somehow an NA value got into the varNames vector. Get rid of it
        varNames <- varNames[!is.na(varNames)]
        for(i in 1:length(rowstoNameBy)){
                dictionaryDT[initialOrder>=rowstoNameBy[i],AllVarNames:=varNames[i]]
        }
}


showSummaries <- function(summaryList,tempmax=3000){
        options(max.print = tempmax)
        print(summaryList)
        options(max.print = 1000)
}

DictionarySearch <- function(searchterm,dictionary,begginingWith = FALSE){
                                        #this takes a data.table and single element character
        dictionary[grep(searchterm,AllVarNames,value = TRUE)]
}

plotPlotly <- function(p) {
        viewer<-getOption("viewer")
        options(viewer = NULL)
        print(p)
        options(viewer = viewer)
}


addColor <- function(g){
        require(randomcoloR)
        k <- 
                palette = distinctColorPalette(k)
        g + scale_fill_manual(values=palette)
}

variableChooser <- function(data,columns,...){#data is a data table,
        require(magrittr)
        arguments = list(...)
         lapply(columns,DictionarySearch,dictionary=arguments[[1]]) %>%
                 lapply('[[',"AllVarNames") %>% unlist()
        
}

downloadlinksWithin <- function(mainURL,mainSelector,selectorWithin,first = 1,last = NULL){
        require(magrittr)
        require(rvest)
        individualPageLinks <- read_html(mainURL) %>% html_nodes(css = mainSelector) %>% 
                html_attr("href")
        
        if(is.null(last)) last <- length(individualPageLinks)
        
        downloadLinkNodes <- lapply(individualPageLinks[first:last],read_html) %>%
                lapply(html_nodes, css = selectorWithin)
        
        downloadLinks <- character()
        for(i in seq_along(downloadLinkNodes)){
                downloadLinks[i] <- downloadLinkNodes[[i]] %>% html_attr("href")
        }
        downloadLinks
}
        
removeNAcolumns <- function(DT) {
        namesofVars <- names(DT) %>% copy() #copy required beacuse: as it deletes columns
        #relative positions change..
        for(i in 1:length(namesofVars)){
                print(paste(namesofVars[i],i))
                print(all(is.na(DT[,(namesofVars[i]),with=FALSE])))
                if(all(is.na(DT[,(namesofVars[i]),with=FALSE]))){
                        DT[,(namesofVars[i]) := NULL]
                }
        }
}
