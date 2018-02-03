library(data.table)
library(ggmap)

stateConversion <- function(x, faclevs = 'selected') {
  
  st.codes <- data.frame(state = as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
                                             "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                                             "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
                                             "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT",
                                             "WA", "WI", "WV", "WY")),
                         full = as.factor(c("Alaska","Alabama" ,  "Arkansas", "Arizona","California" , "Colorado" ,
                                            "Connecticut", "District of Columbia","Delaware" ,  "Florida" , "Georgia" ,
                                            "Hawaii","Iowa" ,"Idaho" , "Illinois" , "Indiana" ,  "Kansas" ,
                                            "Kentucky" , "Louisiana" , "Massachusetts", "Maryland" ,"Maine" ,
                                            "Michigan" , "Minnesota" , "Missouri" ,"Mississippi" ,  "Montana" ,
                                            "North Carolina","North Dakota", "Nebraska" , "New Hampshire" , "New Jersey" ,  "New Mexico" ,
                                            "Nevada" ,"New York" , "Ohio" , "Oklahoma" ,
                                            "Oregon" , "Pennsylvania" , "Puerto Rico", "Rhode Island" , "South Carolina", "South Dakota" ,
                                            "Tennessee" , "Texas" , "Utah" ,  "Virginia","Vermont" ,
                                            "Washington" , "Wisconsin", "West Virginia" , "Wyoming"))
  )
  
  if (nchar(x[1]) == 2) { st.x <- data.frame(state = x); refac.x <- st.codes$full[match(tolower(st.x$state), tolower(st.codes$state))] }
  else { st.x <- data.frame(full = x); refac.x <- st.codes$state[match(tolower(st.x$full), tolower(st.codes$full))] }
  
  if(faclevs == 'all') {return(refac.x)}
  else {return(factor(refac.x))}
  
}


annualPM <- fread("data/annual_all_2005.csv")

monitors <- fread("data/monitor.locations.csv")[ , V1:= NULL]


lookup <- apply(as.matrix(monitors[ ,2:3]),1,revgeocode)

annualPM[ ,Monitor := paste("M",sprintf("%02d", as.numeric(`State Code`)),
                            sprintf("%03d", as.numeric(`County Code`)),
                            "-",sprintf("%04d", as.numeric(`Site Num`)), sep = "")]

annualPM <- unique(annualPM[ , .(Monitor,`County Name`, `City Name`, `State Name`) ])

annualPM$receptor.state <- as.character(stateConversion(annualPM$`State Name`))
annualPM[ , `State Name` := NULL]

colnames(annualPM) <-c("ID", "receptor.county", "receptor.city", "receptor.state")
setkey(annualPM, ID)
setkey(M_locations, ID)

temp <- annualPM[M_locations]
head(temp)
temp <- temp[ , c(1,5:6,2:4)]
head(temp)

temp$receptor.region <- getRegion(temp$receptor.state)

write.csv(temp, file = "data/monitor.locations.csv")


