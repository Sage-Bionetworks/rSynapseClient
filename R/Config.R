
ConfigParser <- function(path) {
    config <- new("Config")
	configFile <- .checkAndReadFile(path)
    section = NULL
    for (line in configFile) {
        # Sections
        matches <- regexec("\\[(.+)\\]", line)
        matches <- unlist(regmatches(line, matches))
        if (length(matches) > 1) {
            section <- matches[2]
            # Keep all items if there are repeated sections
            if (!(section %in% names(config@data))) {
                config@data[[section]] <- list()
            }
            next
        }
        
        # Options and values
        matches <- regexec("\\s*([^=]+)\\s*=\\s*([^=]+)\\s*", line)
        matches <- unlist(regmatches(line, matches))
        if (length(matches) > 2) {
            matches <- sapply(matches, function(vec){ gsub("(^ +)|( +$)", "", vec) })
            config@data[[section]][[matches[[2]]]] <- matches[[3]]
        }
    }
    
    return(config)
}

.checkAndReadFile <- function(path) {
    if (missing(path)) {
		path <- "~/.synapseConfig"
	}
    if (!file.exists(path)) {
        stop(sprintf("Configuration file '%s' cannot be found.", path))
    }
    return(readLines(path))
}

Config.hasSection <- function(config, section) {
    return(section %in% names(config@data))
}

Config.hasOption <- function(config, section, option) {
    return(option %in% names(config@data[[section]]))
}

Config.getOption <- function(config, section, option) {
    return(config@data[[section]][[option]])
}
