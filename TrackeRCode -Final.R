


install.packages("trackeR")  #Installing the trackeR package
library(trackeR) #loading trackeR package

# Reading TCX files for three different riders

#reading data of rider1 from rider1.tcx file using readTCX function 
rider1 = readTCX(file ='E:/Class Notes/R Class/Project/working/rider1.tcx', timezone = "GMT")

#reading data of rider2 from rider2.tcx file using readTCX function
rider2 = readTCX(file ='E:/Class Notes/R Class/Project/working/rider2.tcx', timezone = "GMT")

################# Rider1 Analysis ######################################3

str(rider1) # Viewing the structure of dataframe rider1
View(rider1) #viewing rider1 data

#Applying consructor (trackeRdata) function on the rider1 data and saving the result dataframe as rider1_session
#units=NULL gives default units 
#cycling=True for cycling data
#sessionThreshold is uses to set threshold for sessions
#lgap,lskip,m parameters are used for imputation of missing observations
#correctDistances controls distance correction for altitude changes

rider1_session=trackeRdata(rider1, units = NULL, cycling = T, sessionThreshold = 2,
            correctDistances = FALSE, country = NULL, mask = TRUE,
            fromDistances = TRUE, lgap = 30, lskip = 5, m = 11)

View(rider1_session) #Viewing rider1_session data

#plot function in TrackeR shows heartrate and pace by default
#plotting rider1_session for 2 to 5 sessions
plot(rider1_session,session=2:5)

#plotting route of session1 for rider1_session
plotRoute(rider1_session, session = 1, zoom = 13, source = "osm")

#interactive Plot using leafletroute for session1 route of rider1_session
leafletRoute(rider1_session,session =1)
#-------------------------summary-------------------------------------------#

#loading summary of rider1_session for sessions 1 to 3 into runsummary_1
runsummary_1 = summary(rider1_session,session =1:3)

# printing runsummary_1 with precision of 3 digits(ex 0.333)
print(runsummary_1,digits=3) 

#to summarize entire rider1_session and load it into runsummaryFull1
runsummaryFull1 = summary(rider1_session)

#plotting avgspeed,distance,duration,avgHeartRate of runsummaryFull1 by total and moving groups
plot(runsummaryFull1,group=c("total","moving"),what = c("avgSpeed","distance","duration","avgHeartRate"))

#--------------------------speed in zones------------------------------------#

# Dividing the speed into zones and claculating time spent in each zone 
runZones1 <- zones(rider1_session[1:3], what = "speed", breaks = c(0, 2:6, 12.5))
plot(runZones1) #visualizing runZones1

#------------------------work capacity----------------------------------------#

#Calculate work capacity of the rider1_session and load into wexp1
wexp1 = Wprime(rider1_session,session=9,quantity = "expended",cp = 4,version = "2012")
#plotting wexp1
plot(wexp1,scaled=TRUE) 

#---------------Distribition profile and concentration profile--------------#

# Applying distributionprofile function on rider1_session and loading it to dprofile1
dProfile1 <- distributionProfile(rider1_session, session = 1:4,
             what = c("speed", "heart.rate"),
             grid = list(speed = seq(0, 12.5, by = 0.05), heart.rate = seq(0, 250)))

plot(dProfile1, multiple = TRUE) #plotting dprofile1

#Applying concentrationprofile on dprofile1
cProfile1 <- concentrationProfile(dProfile1, what = "speed")

plot(cProfile1, multiple = TRUE)  #plotting cprofile

#--------------------------Threshold and smoothing--------------------------#
plot(rider1_session, session = 4, what = "speed") #by default threshold exists while plotting
#plotting rider1_session with threshold=false
plot(rider1_session, session = 4, what = "speed", threshold = FALSE)
#plotting rider1_session with default threshold and smoothing with median
plot(rider1_session, session = 1, what = "speed", smooth = TRUE, fun = "median",width = 20)
#Applying threshold function for session 4 of rider1_session and saving to rider1_session_t
rider1_session_t <- threshold(rider1_session[4])
#Applying smoother function with median on speed of rider1_session_t and loading to run4s
run4S <- smoother(rider1_session_t, what = "speed", fun = "median", width = 20)
plot(run4S, what = "speed", smooth = FALSE) #plotting run4S with smooth=False

#To see units of rider1_session
getUnits(rider1_session)
#To change units of speed in rider1_session from m_per_sec to mi_per_h
ChangeUnits = changeUnits(rider1_session, variable = "speed", unit = "mi_per_h")
#changing speed to ft per hour

#function unitold2unitnew for changing m_per_s to ft_per_h
m_per_s2ft_per_h <- function(x) x * 3937/1200 * 3600
#changing speed in rider1_session to ft_per_h
changeUnits(rider1_session, variable = "speed", unit = "ft_per_h")


########################### Rider 2 #############################
# Applying consructor function on the rider data and saving the result df as rider_session
rider2_session=trackeRdata(rider2, units = NULL, cycling = T, sessionThreshold = 2,
                           correctDistances = FALSE, country = NULL, mask = TRUE,
                           fromDistances = TRUE, lgap = 30, lskip = 5, m = 11)
# visualizing the session 2 data
plot(rider2_session,session=2)

# Sumary function and visualizing the summary function
runsummary_2 = summary(rider2_session,session =1)
print(runsummary_2,digits=3)

runsummaryFull2 = summary(rider2_session)
plot(runsummaryFull2,group=c("total","moving"),what = c("avgSpeed","distance","duration","avgHeartRate"))
plotRoute(rider2_session, session = 1, zoom = 13, source = "osm")

# Dividing the speed into zones and claculating time spent in each zone and visualizing it
runZones2 <- zones(rider2_session[1:3], what = "speed", breaks = c(0, 2:6, 12.5))
plot(runZones2)

#Calculate and visualize work capacity of the rider
wexp2 = Wprime(rider2_session,session=1,quantity = "expended",cp = 4,version = "2012")
plot(wexp2,scaled=TRUE) # axplained reading the function

# Distribution and Concentration Profiles and its visualization
dProfile2 <- distributionProfile(rider2_session, session = 1:4,
                                 what = c("speed", "heart.rate"),
                                 grid = list(speed = seq(0, 12.5, by = 0.05), heart.rate = seq(0, 250)))

plot(dProfile2, multiple = TRUE)

cProfile2 <- concentrationProfile(dProfile2, what = "speed")

plot(cProfile2, multiple = TRUE)  # concentration of speed arount parti time

#Plot the route covered by rider
trackeR::leafletRoute(rider2_session)

getUnits(rider2_session)
ChangeUnits = changeUnits(rider2_session, variable = "speed", unit = "mi_per_h")
getUnits(ChangeUnits)
#changing speed to ft per hour
#function unitold2unitnew
m_per_s2ft_per_h <- function(x) x * 3937/1200 * 3600
changeUnits(rider2_session, variable = "speed", unit = "ft_per_h")
#smoothing
plot(rider2_session, session = 4, what = "speed")
plot(rider2_session, session = 4, what = "speed", threshold = FALSE)
plot(rider2_session, session = 4, what = "speed", smooth = TRUE, fun = "median",width = 20)
rider2_session_t <- threshold(rider2_session[4])
run4S <- smoother(rider2_session_t, what = "speed", fun = "median", width = 20)
plot(run4S, what = "speed", smooth = FALSE)
