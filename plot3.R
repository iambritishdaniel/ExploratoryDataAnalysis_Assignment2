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
png("plot3.png", width=500, height=500)

# define a color palette to use for all charts
cPalette <- c("#ff0000",
              "#00ff00",
              "#0000ff",
              "#ff00ff",
              "#ff8000",
              "#33ffff",
              "#aacc00")

# draw faceted plot by type
plot3 <- (ggplot(emi[emi$fips=="24510",], aes(x=year, y=Emissions)) 
          + geom_point(aes(color=type), size=8, alpha=0.2, shape=18) 
          + facet_wrap( ~ type, ncol=2)
          + geom_smooth(size=1, col="black", linetype=1, method="lm", se=FALSE)
          + labs(title="Emissions by Type in Baltimore City", 
                 x="Year",
                 y="Emissions (Tons)")
          + theme_bw()
          + theme(legend.position="none")
          + scale_colour_manual(values=cPalette)
          + scale_x_continuous(breaks=c(seq(1999, 2008, by=3))))

print(plot3)

# close graphics device
dev.off()