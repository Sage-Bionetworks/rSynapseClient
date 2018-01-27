hook_synapseMdSyntax_plot <- function(x, options){
  synPlotMdOpts <- character()
  
  ## SET URL ENCODING STRINGS
  urlEncodings <- c('{' = "%7B",
                    '}' = "%7D",
                    '-' = "%2D",
                    '_' = "%5F",
                    '.' = "%2E",
                    '!' = "%21",
                    '~' = "%7E",
                    '*' = "%2A",
                    '`' = "%60",
                    '\'' = "%27",
                    '(' = "%28",
                    ')' = "%29",
                    '[' = "%5B",
                    ']' = "%5D",
                    ':' = "%3A",
                    ';' = "%3B",
                    '\n' = "%0A",
                    '\r' = "%0D",
                    '/' = "%2F",
                    '?' = "%3F",
                    '&' = "%26",
                    '=' = "%3D",
                    '+' = "%2B",
                    ',' = "%2C",
                    '#' = "%23",
                    '$' = "%24")
  
  ## CHECK FOR ALIGN OPTION BEING SET
  if( any(names(options) == "align") ){
    ## CHECKS FOR ALIGN OPTION
    if( !is.character(options$align) ){
      stop("align must be one of none, left, right, or center")
    }
    if( !(options$align %in% c("none", "left", "right", "center")) ){
      stop("align must be one of none, left, right, or center")
    }
    synPlotMdOpts <- paste(synPlotMdOpts, "&align=", options$align, sep="")
  } else{
    synPlotMdOpts <- paste(synPlotMdOpts, "&align=none", sep="")
  }
  
  ## CHECK FOR SCALE OPTION BEING SET
  if( any(names(options) == "scale") ){
    ## RANGE CHECKS FOR SCALE OPTION
    if( !is.numeric(options$scale) ){
      stop("scale option must be numeric")
    }
    if( options$scale <= 0 | options$scale > 500 ){
      stop("scale option must be greater than 0 and less than 500")
    }
    
    synPlotMdOpts <- paste(synPlotMdOpts, "&scale=", options$scale, sep="")
  } else{
    synPlotMdOpts <- paste(synPlotMdOpts, "&scale=100", sep="")
  }
  
  paste("${image?fileName=", RCurl::curlPercentEncode(basename(paste(x, collapse=".")), codes=urlEncodings), synPlotMdOpts, "}\n", sep="")
}