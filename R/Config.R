
setClass("Config", 
    representation(data = "list", filepath = "character"))
    
ConfigParser <- function(path) {
	if (missing(path)) {
		path <- "~/.synapseConfig"
	}
	
    config <- new("Config")
    if (!file.exists(path)) {
        stop(sprintf("Configuration file '%s' cannot be found.", path))
    }

    config@filepath <- path
    configFile <- readLines(path)
    section = NULL
    for (line in configFile) {
        # Sections
        matches <- regexec("\\[(.+)\\]", line)
        matches <- unlist(regmatches(line, matches))
        if (length(matches) > 1) {
            section <- matches[2]
            config@data[[section]] <- c()
            next
        }
        
        # Options and values
        matches <- regexec("\\s*([^=]+)\\s*=\\s*([^=]+)\\s*", line)
        matches <- unlist(regmatches(line, matches))
        if (length(matches) > 2) {
            
            matches <- sapply(matches, function(vec){ gsub("(^ +)|( +$)", "", vec) })
            config@data[[section]][[matches[2]]] <- matches[3]
        }
    }
    
    return(config)
}

Config.hasOption <- function(config, section, option) {
    return(option %in% names(config@data[[section]]))
}

Config.getOption <- function(config, section, option) {
    return(config@data[[section]][[option]])
}