require(ggplot2)
require(gridExtra)

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
png("plot6.png", width=1000, height=800)

# define a color palette to use for all charts
cPalette <- c("#ff0000",
              "#00ff00",
              "#0000ff",
              "#ff00ff",
              "#ff8000",
              "#33ffff",
              "#aacc00")

# get vehicle emissions in Baltimore City
bEmi <- emi[emi$fips=="24510" & emi$SCC %in% src[grep("-road", src$EI.Sector, ignore.case=TRUE), 1], ]
bEmi$cityName <- "Baltimore City"

# get vehicle emissions in LA
laEmi <- emi[emi$fips=="06037" & emi$SCC %in% src[grep("-road", src$EI.Sector, ignore.case=TRUE), 1], ]
laEmi$cityName <- "Los Angeles County"

# combine the data from the two cities
vehEmi <- rbind(bEmi, laEmi)

# merge sector name with emission data
srcNames <- src[, c(1, 4)]
vehEmi <- merge(vehEmi, srcNames, by.x = "SCC", by.y = "SCC")

# define a function to rename our facets with something easier to read
facetLabeller <- function(var,val){
        val <- as.character(val)
        if (var=="EI.Sector") { 
                val[val=="Mobile - Non-Road Equipment - Diesel"] <- "Non-Road\nDiesel"
                val[val=="Mobile - Non-Road Equipment - Gasoline"]   <- "Non-Road\nGasoline"
                val[val=="Mobile - Non-Road Equipment - Other"]   <- "Non-Road\nOther"
                val[val=="Mobile - On-Road Diesel Heavy Duty Vehicles"] <- "On-Road\nDiesel Heavy Duty"
                val[val=="Mobile - On-Road Diesel Light Duty Vehicles"]   <- "On-Road\nDiesel Light Duty"
                val[val=="Mobile - On-Road Gasoline Heavy Duty Vehicles"] <- "On-Road\nGasoline Heavy Duty"
                val[val=="Mobile - On-Road Gasoline Light Duty Vehicles"]   <- "On-Road\nGasoline Light Duty"
        }
        return(val)
}

# create a plot faceted by sector and city
byType <- (ggplot(vehEmi, aes(x=year, y=Emissions))
           + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
           + facet_grid(cityName~EI.Sector, labeller=facetLabeller) 
           + geom_smooth(size=1, col="black", linetype=1, method="lm", se=FALSE)
           + labs(title="Emissions by Motor Vehicle Type", 
                  xlab="Year", 
                  ylab="Emissions (Tons)")
           + theme_bw()
           + theme(legend.position="none",
                   strip.text=element_text(size=12),
                   axis.title.x=element_text(size=12),
                   axis.title.y=element_text(size=12),
                   axis.text.x=element_text(angle=90, vjust=0.5, size=10),
                   axis.text.y=element_text(size=10))
           + scale_colour_manual(values=cPalette)
           + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

# aggregate the data to give total emissions counts by year and city
tEmi <- aggregate(Emissions~year+cityName, 
                  vehicleEmissions, 
                  na.rm=TRUE, 
                  FUN=sum)

# create a bar chart of the totals
totals <- (ggplot(tEmi, aes(x=year, y=Emissions, fill=cityName))
           + geom_bar(stat="identity", position=position_dodge()) 
           + labs(title="Total Motor Vehicle Emissions", 
                   xlab="Year", 
                   ylab="Emissions (Tons)")
           + theme(axis.title.x=element_text(size=12),
                   axis.title.y=element_text(size=12),
                   axis.text.x=element_text(size=10),
                   axis.text.y=element_text(size=10))
           + theme_bw()
           + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

# display the two plots as a single chart
plot6 <- grid.arrange(byType, 
             totals, 
             nrow=2, 
             main=textGrob("\nComparison of Motor Vehicle Emissions in Baltimore City and Los Angeles",
                           gp=gpar(fontsize=18)))

print(plot6)

# close graphics device
dev.off()