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
        fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip";
        download.file(fileUrl, destfile="data/data.zip", method="curl")
        unzip("data/data.zip", exdir="data")
}

# read data
src <- readRDS(dataFile1)
emi <- readRDS(dataFile2)
# ensure we are only dealing with PM25-PRI emissions data
emi <- emi[emi$Pollutant=='PM25-PRI',]

# open image for writing
png("plot5.png", width=800, height=800)

# define a color palette to use for all charts
cPalette <- c("#ff0000",
              "#00ff00",
              "#0000ff",
              "#ff00ff",
              "#ff8000",
              "#33ffff",
              "#aacc00")

# I'm using the dictionary definition of "motor vehicle" to include any
# "self-propelled wheeled conveyance, such as a car or truck, that does not run 
# on rails" and including both on-road and off-road vehicles
# these can be identified in the emissions data by the string "-road" in the
# EI.Sector field
vehEmi <- emi[emi$fips=="24510" & emi$SCC %in% src[grep("-road", src$EI.Sector, ignore.case=TRUE), 1], ]

# merge sector name with emission data
srcNames <- src[, c(1, 4)]
vehEmi <- merge(vehEmi, srcNames, by.x = "SCC", by.y = "SCC")

# define a function to rename our facets with something easier to read
facetLabeller <- function(var,val){
        val <- as.character(val)
        if (var=="EI.Sector") { 
                val[val=="Mobile - Non-Road Equipment - Diesel"] <- "Diesel"
                val[val=="Mobile - Non-Road Equipment - Gasoline"]   <- "Gasoline"
                val[val=="Mobile - Non-Road Equipment - Other"]   <- "Other"
                val[val=="Mobile - On-Road Diesel Heavy Duty Vehicles"] <- "Diesel Heavy Duty"
                val[val=="Mobile - On-Road Diesel Light Duty Vehicles"]   <- "Diesel Light Duty"
                val[val=="Mobile - On-Road Gasoline Heavy Duty Vehicles"] <- "Gasoline Heavy Duty"
                val[val=="Mobile - On-Road Gasoline Light Duty Vehicles"]   <- "Gasoline Light Duty"
        }
        return(val)
}

# create faceted plot for "on-raod" vehicles
onRoad <- (ggplot(vehEmi[vehEmi$type=="ON-ROAD",], aes(x=year, y=Emissions))
          + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
          + facet_grid(.~EI.Sector, labeller=facetLabeller) 
          + geom_smooth(size=1, col="black", linetype=1, method="lm", se=FALSE)
          + labs(title="On-Road Vehicles", xlab="", ylab="")
          + theme_bw()
          + theme(legend.position="none", 
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  strip.text.x = element_text(size=12))
          + scale_colour_manual(values=cPalette[1:4])
          + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

# create second faceted plot for "non-road" vehicles
nonRoad <- (ggplot(vehicleEmissions[vehicleEmissions$type=="NON-ROAD",], 
                   aes(x=year, y=Emissions))
           + geom_point(aes(color=EI.Sector), size=8, alpha=0.2, shape=18) 
           + facet_grid(.~EI.Sector, labeller=facetLabeller) 
           + geom_smooth(size=1, col="black", linetype=1, method="lm", se=FALSE)
           + labs(title="Non-Road Vehicles & Equipment", xlab="", ylab="")
           + theme_bw()
           + theme(legend.position="none", 
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   strip.text.x = element_text(size=12))
           + scale_colour_manual(values=cPalette[5:7])
           + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

# display the two rows of facets as a single chart
plot5 <- grid.arrange(onRoad, 
             nonRoad, 
             nrow=2, 
             left=textGrob("\nEmissions (Tons)",
                           gp=gpar(fontsize=16),
                           rot=90),
             sub=textGrob("\nYear",
                          gp=gpar(fontsize=16)),
             main=textGrob("\nMotor Vehicle Emissions in Baltimore City",
                           gp=gpar(fontsize=18)))

print(plot5)

# close graphics device
dev.off()