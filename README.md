# teambavz

setwd('')
#=====================================================================================
#extract data from OD api

library(httr)
library(dplyr)
library(jsonlite)
library(openssl)

#parameters
consumer.key <- "d0a8b4f3-2507-352f-8bd1-534ff6449978"
consumer.secret <- "R3ORzSxGAFwBIKVZLq1fRoWNm98a"
key.secret.b64 <- base64_encode(paste(consumer.key,consumer.secret, sep=":"))
token.response <- POST("https://apistore.datasparkanalytics.com/token", 
                       body = "grant_type = client_credentials", 
                       add_headers(Authorization = paste("Basic",key.secret.b64)))
token <- content(token.response)$accesstoken
discretevisitAPI = "https://apistore.datasparkanalytics.com:443/discretevisit/v2/query"
odmatrixAPI = "https://apistore.datasparkanalytics.com:443/odmatrix/v3/query"
staypointAPI = "https://apistore.datasparkanalytics.com:443/staypoint/v2/query"

#for loop
Jan17 <- do.call("c",as.list(seq(as.Date("2018-01-14"), as.Date("2018-01-20"), by="days")))
planningarea <- read.csv("planningarea.csv")

#query API
for (i in 1:length(Jan17)){
  for (j in 1:55){
    query.body <- list(
      date = Jan17[i],
      timeSeriesReference = "origin",
      dimensionFacets = list("destination_planningarea"),
      location = list(locationType = "locationHierarchyLevel", levelType = "origin_planningarea", id =planningarea$Area_code[j]),
      queryGranularity = list(type = "period", period = "PT1H"),
      aggregations = list(list(metric = "unique_agents", type = "hyperUnique"),list(metric = "total_records", type = "longSum")),
      filter = list(type="selector",dimension="dominant_mode",value="TRAIN")
    )
    
    # token variable contains a valid access token; see Getting Started.
    query.response <- POST(odmatrixAPI,
                           add_headers(Authorization=paste("Bearer",token)),
                           encode = "json",
                           body = query.body,
                           verbose())
    
    #convert query response to JSON
    
    data <- fromJSON(rawToChar(query.response$content))
    data.df <- do.call(what = "cbind", args = lapply(data, as.data.frame))
    names(data.df)[1] = names(data[1])
    
    if (i==1 & j==1){
      output <- data.df
    }
    else {
      output <- rbind(output,data.df)
    }
    
  }
}

#=======================================================================================
### Create a distance matrix S.
### Set all the impossible connections between nodes to a large number.
### Because the algorithm is looking for a minimum, very large distances will never be selected

library(gdata)

S=matrix(999,103,103)
## Green Line
S[1,2]=4 #Joo Koon - 1 to Pioneer - 2 
S[2,3]=2 #Pioneer - 2 to Boon Lay - 3
S[3,4]=3 # Boon Lay to Lakeside  - 4
S[4,5]=2 #Lakeside to Chinese Garden - 5
S[5,6]=2 #Chinese Garden to Jurong East - 6
S[6,7]=4 #Jurong East to Clementi - 7
S[7,8]=3 #Clementi to Dover - 8
S[8,9]=2  #Dover to Buona Vista - 9
S[9,10]=2 # BV to Commonwealth - 10
S[10,11]=2 #Commonwealth to Queenstown - 11
S[11,12]=2 #Queenstown to Redhill - 12 
S[12,13]=2 #Redhill to TB - 13 
S[13,14]=2 #Tiong Bahru to Outram - 14
S[14,15]=2 #Outram to Tanjong Pagar - 15 
S[15,16]=2 #Tanjong Pagar to RP - 16
S[16,17]=2 #Raffles Place to City Hall - 17
S[17,18]=2 #City Hall to Bugis - 18
S[18,19]=2 #Bugis to Lavender
S[19,20]=2 #Lavendar to Kallang 
S[20,21]=2 #Kallang to Aljunied
S[21,22]=2 #Aljunied to Paya Lebar - 22 
S[22,23]=2 #PL to Eunos 
S[23,24]=2 #Eunos to Kembagan 
S[24,25]=2 #Kembagan to Bedok
S[25,26]=2 #Bedok to Tanah
S[26,27]=3 #TM to Simei 
S[27,28]=3 #Simei to Tampinies 
S[28,29]=2 #Tamp to Pasir Ris 
S[26,30]=3 #TM to Expo
S[30,31]=5 #Expo to Changi Airport - 31 

#G- R Line
S[6,32]=8 #Jurong East Green to Red Line 
S[17,54]=4 # City Hall G to R Line
S[16,55]=4  #Raffles Place G to R Line

## Red Line 
S[32,33]=3 #Jurong East -32 to Bukit Batok - 33
S[33,34]=2 #Bukit Batok to Bukit Gombak - 34 
S[34,35]=4 #Bukit Gombak to Choa Chu Kang - 35
S[35,36]=2 #CCK to Yew Tee - 36
S[36,37]=5 #Yew Teee to Kranji - 37
S[37,38]=2 #Kranji to Marsiling - 38
S[38,39]=3 #Marsiling to Woodlands - 39
S[39,40]=2 #WL to Admiralty - 40
S[40,41]=3 #Admiralty to Sembawang - 41
S[41,42]=4 #Sembawang to Yishun - 42
S[42,43]=2 #Yishun to Khatib - 43
S[43,44]=6 #Khatib to YCK - 44
S[44,45]=2 #YCK to AMK - 45
S[45,46]=2 #AMK to Bishan - 46 
S[46,47]=2 #Bishan to Braddell - 47
S[47,48]=2 #Braddell to TP - 48
S[48,49]=2 #TP to Novena - 49
S[49,50]=2 #Novena to Newton - 50
S[50,51]=2 #Newton to Orchard - 51
S[51,52]=2 #Orchard to Somerset - 52
S[52,53]=1 #Somerset to DHBG - 53
S[53,54]=2 #DHBG to City Hall - 54
S[54,55]=2 #City Hall to Raffles Place - 55
S[55,56]=2 #Raffles Place to Marina Bay - 56 
S[56,57]=2 #Marina Bay to Marina South Pier - 57 

## Purple Line 
S[58,59]=1  #Punggol - 58 to SengKang - 59
S[59,60]=3 #Sengkang to Buangkok - 60
S[60,61]=2 #Buangkok to Hougang - 61 
S[61,62]=2 #Hougang to Kovan - 62
S[62,63]=3 #Kovan to Serangoon - 63 
S[63,64]=2 #Serangoon to Woodleigh - 64 
S[64,65]=2 #Woodleigh to Potong Pasir - 65
S[65,66]=2 #PP to BK - 66
S[66,67]=2 #Boon Keng to Farrer Park - 67
S[67,68]=2 #Farrer Park to Little India - 68
S[68,69]=2 #Little India to DHBG - 69
S[69,70]=2 #DHBG to Clarke Quay - 70
S[70,71]=2 #CQ to Chinatown - 71
S[71,72]=2 #CT to Outram Park - 72
S[72,73]=3 #OP to HarbourFront - 73

## P to Y Line
S[73,74]=8  #HBfront P to Y lines 
S[63,89]=8 #Serangoon P to Y lines
S[69,103]=8 #DHBG P to Y Lines

## P to G Line
S[14,72]=10  #Outram Park 

## Yellow Line (Circle)
S[74,75]=2 #HBFront - 74 to Telok Blangah - 75
S[75,76]=2 #Telok Blangah to Labrador Park - 76
S[76,77]=3 #LP to Pasir Panjang - 77
S[77,78]=2 #PP to Haw Par Villa - 78
S[78,79]=2 #HPV to Kent Ridge - 79
S[79,80]=2 #KR to one north - 80
S[80,81]=2 #one north to BV - 81
S[81,82]=2 #BV to HV - 82 
S[82,83]=3 #HV to Farrer Road - 83 
S[83,84]=2 #FR to Botanics  -84 
S[84,85]=5 #Bontanics to Caldecott - 85
S[85,86]=2 #Caldecott to Marymount - 86
S[86,87]=3 #Marymount to Bishan - 87
S[87,88]=2 #Bishan to Lorong Chuan - 88
S[88,89]=2 #LC to SRG - 89
S[89,90]=3 #SRG to Bartley - 90
S[90,91]=2 #Bartley to Tai Seng - 91 
S[91,92]=2 #Tai Seng to Macpherson - 92 
S[92,93]=2 #Macpherson to Paya - 93
S[93,94]=2 #Paya to Dakota - 94
S[94,95]=2 #Dakota to Mountbatten - 95
S[95,96]=2 #MBT to Stadium - 96
S[96,97]=2 #Stadium to Nicoll Highway - 97
S[97,98]=2 #Nicoll to Promenade - 98
S[98,99]=2 #Promenade to Bayfront - 99
S[99,100]=2 #BF to Marina Bay - 100

S[98,101]=2 #Promenade to Esplanade - 101 
S[101,102]=2 #Esp to Bras Basah - 102 
S[102,103]=2 #BB to DHBG - 103 

## Y to G Lines 
S[22,93]=6 #PayaLeb 
S[9,81]=8 #Buona 

## Y to R Lines 
S[53,69]=8 #DHBG P to R Line
S[53,103]=8 #DHBG Yellow to Red line
S[46,87]=8 #Bishan
S[56,100]=6 #Marina Bay 

#symmetric matric
lowerTriangle(S) <- upperTriangle(S,byrow=TRUE)
isSymmetric(S)

### List of input parameters for function
n=length(S[,1]) #number of nodes
cost=S #distance matrix

### Dijkstra's algorithm
dijkstra=function(n,v,cost,dest){
  
  #create empty variables to store data
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # for every node in the network
  for(i in 1:n){
    prev[i] = -1
    dest[i] = cost[v,i] #= distance from start node v to every other node i in the network
  }
  
  #initialise counter which keeps track of number of steps through network
  count=2
  
  # until we have reached our destination node n
  while(count <= n){
    min=999
    
    # loop over each node
    for(w in 1:n){
      #if the new path is less long than the existing smallest one and flag[w] is equal to zero (aka we've not already incuded that node in route)
      if(dest[w] < min && !flag[w]){
        # overwrite the minimum with the new shortest path and update counter
        min=dest[w]
        u=w
      }
    }
    flag[u] = 1 #indicate that we go to this site
    count = count+1
    
    # loop over each node again keeping in mind where we have already been
    for(w in 1:n){
      #if the new route is shorter than the previous route
      if((dest[u]+cost[u,w] < dest[w]) && !flag[w]){
        dest[w]=dest[u]+cost[u,w] #update the distance to destination
        prev[w]=u #keep track of the node visited
      }
    }
  }
  return(prev)
}

### create function which returns path
savepath = function(f,x){
  path=x
  while(f[x] != -1){
    path=c(path,f[x])
    x=f[x]
    savepath(f,x)
  }
  path=c(path,v)
  return(path)
}

### Run Dijkstra's algorithm with our distance matrix
#prev = dijkstra(n,v,cost,dest)
#path = savepath(prev,dest)

### Print path
#path
#prev
#==========================================================================
# create a 3 dimensional array that accounts for the hour, station and line


library(ggplot2)
library(reshape2)
library(dplyr)
library(digest)

data <- output

#data cleaning
colnames(data)[colnames(data)=="event.origin_planningarea"] <- "origin_code"
colnames(data)[colnames(data)=="event.destination_planningarea"] <- "dest_code"

sub <- subset(data, !dest_code==0 & !dest=="NA" & !node_dest=="NA")
sub <- subset(sub, !timestamp == "1:00" & !timestamp == "2:00" &
                !timestamp =="3:00" & !timestamp =="4:00")
sub <- subset(sub,event.longSum_total_records > 40)
sub <- subset(sub,event.hyperUnique_unique_agents > 40)
a <- subset(sub, !origin_code==0 & !origin=="NA" & !node_origin=="NA")
a$origin_code <- as.numeric(a$origin_code)
a$dest_code <- as.numeric(as.character(a$dest_code))
a$node_dest <- as.numeric(as.character(a$node_dest))
a$node_origin <-as.numeric(as.character(a$node_origin))

# code and node file
code_node <- read.csv('code_node.csv')
code = function(path){
  lookup <- code_node$code[code_node$node==path]
}

#array
stations <- array(0,dim=c(24,4,31))
for (i in 1:nrow(a)){
  print(i)
for (time in 0:23){
  timestamp <- paste(time,':00',sep="")
  if (timestamp == a$timestamp[i]){
    v = a$node_origin[i]
    dest=a$node_dest[i]
    prev = dijkstra(n,v,cost,dest)
    path = savepath(prev,dest)
  for (j in 1:length(path)){
     station_no = code(path[j])
    row <- station_no/100
    column <- station_no%%100
    stations[time+1,row,column] = stations[time+1,row,column] + as.numeric(a$event.longSum_total_records[i])
   }
  }
}
}

melt <- melt(stations)

write.csv(melt,file="stations_final.csv")
