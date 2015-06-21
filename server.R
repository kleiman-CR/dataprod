library(shiny)
library (ENmisc)
library (ggplot2)
library (scales)

# reads the data
goods <- read.csv ("goods.csv", header = FALSE,
                   col.names = c ("id", "name"),
                   colClasses = c ("numeric", "character"))

# estimates scaling factor
get.scaling.factor <- function (x) {
  maxval <- max (x)
  res <- c (0, "")
  if (maxval >= 1e+13) {
    res[1] <- 1e+12
    res[2] <- "trillions of of "
  } else if (maxval >= 1e+10) {
    res[1] <- 1e+9
    res[2] <- "billions of  "
  } else if (maxval >= 1e+7) {
    res[1] <- 1e+6
    res[2] <- "millions of "
  } else if (maxval >= 1e+4) {
    res[1] <- 1e+3
    res[2] <- "thoudsands of "
  } else {
    res[1] <- 1
    res[2] <- ""
  }
  names (res) <- c ("value", "name")
  res
}

shinyServer(
  function(input, output) {
    output$plot <-renderPlot ({
      item.name <- input$good
      
      id <- goods$id[goods$name == item.name]
      
      data <- read.csv (paste0("data/", id, ".csv"), header = TRUE,
                colClasses = c ("Date", "numeric", "numeric", "numeric"))
      
      # scale data for better representation
      factor <- get.scaling.factor (data$Upper.Quantile)
      data$Median <- data$Median / as.numeric (factor[['value']])
      data$Lower.Quantile <- data$Lower.Quantile / as.numeric (factor[['value']])
      data$Upper.Quantile <- data$Upper.Quantile / as.numeric (factor[['value']])
      
      p <- ggplot(data) + theme_bw() +
        geom_ribbon(aes (x = Day, ymin = Lower.Quantile, ymax = Upper.Quantile),
                    alpha=0.1, color="grey") +
        
        geom_line(aes(x=Day, y=Median), color="steelblue", size=2) +
        
        annotate ("text", label = item.name,
                  x = data$Day[1], y = max (data$Upper.Quantile),
                  hjust = -0.1, vjust = 1, color = "grey", fontface = 2, size = 8) +
        
        annotate ("text", label = sprintf ("%.1f", data$Median[1]),
                  x = data$Day[1], y = data$Median[1],
                  hjust = -0.1, vjust = -1, color = "steelblue", fontface = 2) +
        
        annotate ("text", label = sprintf ("%.1f", data$Median[length(data$Median)]),
                  x = data$Day[length(data$Day)], y = data$Median[length(data$Median)],
                  hjust = 1.1, vjust = -1, color = "steelblue", fontface = 2) +
        
        scale_x_date(labels = date_format("%d.%m"),
                     limits = c(data$Day[1], data$Day[length(data$Day)]),
                     expand = c(0,0)) +
        
        labs(x = "", y = paste0 ("Price, ", factor[['name']], "R"))
      
      print (p)
    })
  }
)