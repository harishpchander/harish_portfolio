#REQUISITES FOR SQL DATABASE INTO R:
install.packages(c('RSQLite', repos='http://cran.rstudio.com',dependecies=TRUE))
install.packages("RSQLite")
library(RSQLite)

#ESTABLISHING CONNECTION:
conn <- dbConnect(RSQLite::SQLite(), "Finaldb_lab4.sqlite")

#CREATING TABLES:
#CROP_DATA:
df1 <- dbExecute(conn, "CREATE TABLE CROP_DATA (
                                      CD_ID INTEGER NOT NULL,
                                      YEAR DATE NOT NULL,
                                      CROP_TYPE VARCHAR(20) NOT NULL,
                                      GEO VARCHAR(20) NOT NULL, 
                                      SEEDED_AREA INTEGER NOT NULL,
                                      HARVESTED_AREA INTEGER NOT NULL,
                                      PRODUCTION INTEGER NOT NULL,
                                      AVG_YIELD INTEGER NOT NULL,
                                      PRIMARY KEY (CD_ID)
                                      )", 
                 errors=FALSE
)

if (df1 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}

#FARM_PRICES:
df2 <- dbExecute(conn, "CREATE TABLE DAILY_FX( DFX_ID INTEGER NOT NULL,
                                              DATE DATE NOT NULL, 
                                              FXUSDCAD FLOAT(6),
                                              PRIMARY KEY (DFX_ID))",
                    errors=FALSE)

    if (df2 == -1){
        cat ("An error has occurred.\n")
        msg <- odbcGetErrMsg(conn)
        print (msg)
    } else {
        cat ("Table was created successfully.\n")
    } 

# DAILY_FX:
df3 <- dbExecute(conn,
                "CREATE TABLE DAILY_FX (
                                    DFX_ID INTEGER NOT NULL,
                                    DATE DATE NOT NULL,
                                    FXUSDCAD FLOAT(6) NOT NULL,
                                    PRIMARY KEY (DFX_ID)
                                    )"
                if (df3 == -1){
                  cat ("An error has occurred.\n")
                  msg <- odbcGetErrMsg(conn)
                  print (msg)
                } else {
                  cat ("Table was created successfully.\n")
                }

#MONTHLY_FX:              
df4 <- dbExecute(conn, "CREATE TABLE MONTHLY_FX ( DFX_ID INTEGER NOT NULL,
                                                  DATE DATE NOT NULL, 
                                                  FXUSDCAD FLOAT(6),
                                                  PRIMARY KEY (DFX_ID))",
                    errors=FALSE)

    if (df4 == -1){
        cat ("An error has occurred.\n")
        msg <- odbcGetErrMsg(conn)
        print (msg)
    } else {
        cat ("Table was created successfully.\n")
    }  


# READING AND LOADING FILES IN R:
crop <- read.csv('C:/Users/haris/Downloads/Annual_Crop_Data.csv', colClasses = c(YEAR= "character"))
dailyfx <- read.csv('C:/Users/haris/Downloads/Daily_FX.csv', colClasses=c(DATE= "character"))
monthlyfx <- read.csv('C:/Users/haris/Downloads/Monthly_FX.csv', colClasses=c(DATE= "character"))
monthlyprices <- read.csv('C:/Users/haris/Downloads/Monthly_Farm_Prices.csv', colClasses=c(DATE= "character"))

head(crop)
head(dailyfx)
head(monthlyfx)
head(monthlyprices)
dbListTables(conn)


#SQL QUERIES IN R:

#How many records are in the farm prices dataset?
dbGetQuery(conn, 'SELECT COUNT(CD_ID) FROM MONTHLY_PRICE')

#Which geographies are included in the farm prices dataset?
dbGetQuery(conn, "SELECT GEO AS GEOGRAPHIES_IN_DATASET FROM FARM_PRICES 
GROUP BY GEO" )

#How many hectares of Rye were harvested in Canada in 1968?
dbGetQuery(conn, "SELECT SUM(HARVESTED_AREA) AS CANADA_HARVESTED_RYE_1968 FROM CROP_DATA
WHERE GEO = 'Canada' AND CROP_TYPE = 'Rye' AND YEAR(YEAR) = 1968")

#Query and display the first 6 rows of the farm prices table for Rye
dbGetQuery(conn, "select * FROM MONTHLY_PRICE  where CROP_TYPE= 'Rye' LIMIT 6 ")

#Which provinces grow Barley?
dbGetQuery(conn, "SELECT GEO AS BARLEY_GROWING_PROVINCES FROM CROP_DATA 
WHERE CROP_TYPE='Barley' AND NOT GEO = 'Canada' GROUP BY GEO")

#Find the first and last dates for the farm prices data.
dbGetQuery(conn, "select min(DATE) AS FIRSTDATE, MAX(DATE) AS LASTDATE FROM MONTHLY_PRICE")

#Which crops have ever reached a farm price greater than or equal to $350 per metric tonne?
dbGetQuery(conn, "SELECT DISTINCT (CROP_TYPE), 
PRICE_PRERMT, YEAR(DATE) AS YEAR FROM FARM_PRICES 
WHERE PRICE_PRERMT >= 350 ORDER BY PRICE_PRERMT LIMIT 10")

#(A) Rank the crop types harvested in Saskatchewan in the year 2000 by their average yield.
dbGetQuery(conn, "SELECT CROP_TYPE AS CROP_RANK, MAX(AVG_YIELD)AS AVG_YIELD_YEAR_2000 
FROM CROP_DATA 
WHERE YEAR(YEAR)=2000 AND GEO='Saskatchewan' 
GROUP BY CROP_TYPE 
ORDER BY AVG_YIELD_YEAR_2000 desc")

#(B) Which crop performed best?
dbGetQuery(conn, "SELECT CROP_TYPE AS BEST_PERFORMING_CROP, AVG_YIELD AS YIELD 
FROM CROP_DATA 
WHERE YEAR(YEAR)=2000 AND GEO='Saskatchewan' LIMIT 1")

#(A) Rank the crops and geographies by their average yield (KG per hectare) since the year 2000.
dbGetQuery(conn, "SELECT CROP_TYPE, GEO, AVG_YIELD 
FROM CROP_DATA
WHERE YEAR(YEAR) >=2000
ORDER BY AVG_YIELD DESC LIMIT 10")

#Use a subquery to determine how much wheat was harvested in Canada in the most recent year of the data.
dbGetQuery(conn, "SELECT MAX(YEAR) AS MOST_RECENT_YEAR, 
SUM(HARVESTED_AREA) AS TOT_HARVESTED_WHEAT_CANADA 
FROM CROP_DATA
WHERE YEAR(YEAR)=(SELECT MAX(YEAR(YEAR))FROM CROP_DATA 
WHERE GEO = 'Canada' AND CROP_TYPE  = 'Wheat')" )

#Use an implicit inner join to calculate the monthly price per metric tonne of Canola grown in Saskatchewan in both Canadian and US dollars. Display the most recent 6 months of the data.
dbGetQuery(conn, "SELECT A.DATE,A.CROP_TYPE,A.GEO,A.PRICE_PRERMT AS CANADIAN_DOLLARS, 
(A.PRICE_PRERMT / B.FXUSDCAD) AS US_DOLLARS, B.FXUSDCAD 
FROM FARM_PRICES A , MONTHLY_FX B 
WHERE YEAR(A.DATE) = YEAR(B.DATE) 
AND MONTH(A.DATE) = MONTH(B.DATE) 
AND CROP_TYPE ='Canola' 
AND GEO = 'Saskatchewan' 
ORDER BY DATE DESC LIMIT 6")