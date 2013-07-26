
setRefClass("Config",
  fields = list(config = "matrix", lastSection = "character"),
  methods = list(
    initialize = function(configFile){
      if (!file.exists(configFile)) {
        return()
      }
      configFile <- readLines(configFile)
      
      # Takes a string and splits it by the first "="
      # If the split is successful, returns a vector of the line's [key, value, section]
      sectionize <- function(input){
        # Note: strsplit does not split API keys correctly, since multiple "=" may be adjacent to each other
        inputChars <- unlist(strsplit(input, ""))
        canSplit <- grep("=", inputChars, fixed=TRUE)
        if (length(canSplit)) {
          canSplit <- canSplit[1]
          itemKey <- do.call(paste, as.list(c(head(inputChars, n=canSplit-1), sep="")))
          itemVal <- do.call(paste, as.list(c(tail(inputChars, n=length(inputChars)-canSplit), sep="")))
          return(c(itemKey, itemVal, lastSection))
        }
        
        if (regexpr("\\[.*\\]", input) >= 0) lastSection <<- input
        return()
      }
      config <<- do.call("rbind", lapply(configFile, sectionize))
      
      # Trim whitespace
      config <<- apply(config, c(1, 2), function(vec){ gsub("(^ +)|( +$)", "", vec) })
    },
    
    has.option = function(section, option){
      return(length(get.section(section, option)) > 0)
    }, 
    
    get.option = function(section, option){
      section <- paste("[", section, "]", sep="")
      section <- grep(section, config, fixed=TRUE)
      option <- grep(option, config[section - 2 * dim(config)[1]], fixed=TRUE)
      return(config[section[option] - dim(config)[1]])
    }
  )
)
