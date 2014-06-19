require(ggplot2)

# check for data directory and create if necessary
if (!file.exists("data")) {
        dir.create("data")
}

# check for data file else download and extract archive to data directory
dataFile1 = "data/Source_Classification_Code.rds"
dataFile2 = "data/summarySCC_PM25.rds"
if (!file.exists(dataFile1) || !file.exists(dataFile2)) {
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip";
        download.file(url, destfile="data/data.zip", method="curl")
        unzip("data/data.zip", exdir="data")
}

# read data
src <- readRDS(dataFile1)
emi <- readRDS(dataFile2)
# ensure we are only dealing with PM25-PRI emissions data
emi <- emi[emi$Pollutant=='PM25-PRI',]

# open image for writing
png("plot4.png", width=800, height=600)

# define a color palette to use for all charts
cPalette <- c("#ff0000",
              "#00ff00",
              "#0000ff",
              "#ff00ff",
              "#ff8000",
              "#33ffff",
              "#aacc00")

# limit emissions to those from coal combustion sources
coalEmi <- emi[emi$SCC %in% sources[grep("coal", src$EI.Sector, ignore.case=TRUE), 1], ]

# merge sector name with emission data
srcNames <- src[, c(1, 4)]
coalEmi <- merge(coalEmi, srcNames, by.x = "SCC", by.y = "SCC")

# define a function to rename our facets with something easier to read
facetLabeller <- function(var,val){
        val <- as.character(val)
        if (var=="EI.Sector") { 
                val[val=="Fuel Comb - Electric Generation - Coal"] <- "Electric Generation"
                val[val=="Fuel Comb - Comm/Institutional - Coal"]   <- "Comm./Institutional"
                val[val=="Fuel Comb - Industrial Boilers, ICEs - Coal"]   <- "Industrial Boilers, ICEs"
        }
        return(val)
}

# create faceted plot by sector
plot4 <- (ggplot(coalEmi, aes(x=year, y=Emissions))
          + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
          + facet_grid(.~EI.Sector, labeller=facetLabeller) 
          + geom_smooth(size=1, col="black", linetype=1, method="lm")
          + labs(title="Emissions from Coal-Combustion Sources",
                 x="Year",
                 y="Emissions (Tons)")
          + theme_bw()
          + theme(legend.position="none")
          + scale_colour_manual(values=cPalette)
          + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

print(plot4)

# close graphics device
dev.off()