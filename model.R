#### load packages####
library(ggplot2)
library(dplyr)
library(tidyr)


#####1. calculating the between farm transmission-𝜆nit=𝛽n∗Nit;∀i=1,...,nsuceptiblefarms ####
# Function to generate a random matrix of size 50x50 with T or F values, with F on the diagonal
generate_random_matrix <- function() {
  mat <- matrix(sample(c(0, 1), 50*50, replace = TRUE, prob= c(.95,.05)), nrow = 50)
  diag(mat) <- 0  # Set diagonal elements to "F"
  return(mat)
}

# Create a list to store the matrices
movement_matrix <- list()

# Generate 100 (timesteps) random matrices and store them in the list
for (i in 1:100) {
  movement_matrix[[i]] <- generate_random_matrix()
}
movement_matrix[[24]]




#####2. creating gravity matrix and distance matrix####
N <- 50
pig_capacity <- sample(500:4000, N, replace = TRUE)

# Initialize distance matrix
distance_matrix <- matrix(0, nrow = N, ncol = N)
p <- 0.75  # Probability of values under 100

# Fill the upper triangular part of the matrix with random distances
for (i in 1:(N-1)) {
  for (j in (i+1):N) {
    if (runif(1) <= p) {
      # Generate random distance with 75% probability under 100
      distance_matrix[i, j] <- runif(1, min = 38, max = 44)
    } else {
      # Generate random distance with 25% probability over 100
      distance_matrix[i, j] <- runif(1, min = 25, max = 50)
    }
    
    # Ensure symmetry in the distance matrix
    distance_matrix[j, i] <- distance_matrix[i, j]
  }
}

# Set attraction force to 0 if distance is greater than 35 km
distance_matrix[distance_matrix > 35] <- 0  #delete all distances above 35 km

# Calculate attraction force for each farm using the gravity model formula
gravity_matrix <- pig_capacity / (distance_matrix^2)
gravity_matrix[is.infinite(gravity_matrix)] <- 0

#####3. creating the vehicle transporting pigs to farms route####
#Eit <- elapsed time (min)
#Zit <- time the vehicle is inside the farm
#Edge <- Efarmi*Efarmj > time (72 h for cold months (from October until March) and
#within 24 h for warm months (from April until September))
#time the vehicle stayed inside the farm (time between 20-50 min, median is ~42, got it from S5)
#creating the z matrix, time the farm is inside the farm

TruckPigMatrixFunction <- function(N){

ZMatrix <- matrix(0, nrow=N, ncol=N)
for (i in 1:N) {
  for (j in 1:N) {
    if (runif(1) < 0.05) {
      ZMatrix[i, j] <- sample(c(25:60), 1)
    }
  }
}

#e matrix
EMatrix <- matrix(0, nrow=N, ncol=N)

for (i in 1:N) {
  for (j in 1:N) {
    if (ZMatrix[i, j] != 0) {
      EMatrix[i, j] <- sample(c(40:360), 1) # number get from indegree and outdegree(?) not sure about the format of edges
    }
  }
}

truck_pig_matrix  <- ZMatrix * EMatrix #edge weight 

return(truck_pig_matrix)
}

N <- 50 # farms
timesteps <- 100

truck_pig_matrix <- vector("list", timesteps)

for(t in 1:timesteps) {
  truck_pig_matrix[[t]] <- TruckPigMatrixFunction((N))
}

print(truck_pig_matrix[[1]])

#####4. Re-break force of infection####

#create the pro_survival dataset as a exponential decay y=e*−k*x
# Define the range of X values ~(time)
x <- seq(0, 100, by=1)

# Define the decay constant, k. Choose k such that the curve decays significantly over 100 days.

k <- log(2) / 20  # This is an example, you can adjust k to fit your needs

# Define the decay function
y <- exp(-k * x)

# Create a data frame for plotting
prob_survival <- data.frame(time = x, Probability = y)

# Plot using ggplot2
ggplot(prob_survival, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  labs(title = "Exponential Decay Curve",
       x = "X-axis (0 to 100)",
       y = "Y-axis (1 to 0)") +
  theme_minimal()
prob_survival
prob_survival <- prob_survival[-1,]
#create farm list
reinfected <- data.frame(farm_id = 1:N, last_outbreak_time = rep(0, N))
reinfected


#### 5. Detection transition Infected to detected (outbreak) ####
#type of farms

farmID <- 1:50
FarmType <- c(
  rep("sow", 7),
  rep("gdu", 3),
  rep("nursery", 9),
  rep("finisher", 31)
)

farms <- data.frame(farmID, FarmType)

# Display the first few rows of the farms data frame to check
head(farms)


## detection parameters for the logistic function
# L is the maximum detection probability
L_sow_GDU <- 0.95 #undergo periodic testing
L_nursery_finisher <- 0.70 # example value, fit based on empirical data
X0_A <- 4  # weeks, midpoint of the sigmoid, average time it takes a farm to detect the disease from a strain A
X0_B <- 5 #weeks for strain B
k <- 0.5 # logistic growth rate



#I * ((0.95/(1 + exp(-0.5*(time_infected - farm_detection_ratio))) ) * efetive_surveillance )
# Function to calculate detection probability
detection_probability <- function(time_infected, L) {
  L / (1 + exp(-k * (time_infected - X0)))
}

# Initialize time_infected vector                                                     
time_infected <- rep(0, N)

#detection only for infected farms A, B or AB. 





##### Set the total population size and parameters####
n_farms <- 50
# Set the time step for the simulation
ntimesteps <- 100
dt = 1



###########model with detection####



## detection parameters for the logistic function
# L is the maximum detection probability
#L_sow_GDU <- 0.95 #undergo periodic testing
#L_nursery_finisher <- 0.70 # example value, fit based on empirical data
#X0_A <- 4  # weeks, midpoint of the sigmoid, average time it takes a farm to detect the disease from a strain A
#X0_B <- 5 #weeks for strain B
#k <- 0.5 # logistic growth rate
print(FarmType)
print(Status)
print(time_infected)

# Define the detection probability function
detect_prob <- function(FarmType, Status, time_infected) {
  # Set L depending on FarmType
  L <- ifelse(FarmType %in% c("sow", "gdu"), 0.95, 0.7)
  # Set X0 depending on Status
  X0 <- ifelse(Status == "A", 4, 3)
  # Calculate and return the detection probability using the logistic function
  L / (1 + exp(-0.5 * (time_infected - X0)))
}



print(detect_prob)


#recovery proportion-transition to detected/infected to susceptible
#sow <- poisson distribution: mean 41 weeks
#nursery, GDU and finisher depend of the production schedule
#nursery <- 7
#gdu <- 22
#finisher <- 25

##### Set the total population size and parameters####
N <- 50
# Set the time step for the simulation
ntimesteps <- 100
dt = 1


# Parameters
#transmissionrate = (Ro)Basic reproduction number. Average of secondary 
#infections caused by a single inf ind. in a susceptible pop. 

beta_A <- 0.26 # Transmission rate strain A between-farm pig movements
beta_B <- 0.4 # Transmission rate strain B between-farm pig movements
beta_A_local <- 0.003 # Transmission rate strain A local transmission
beta_B_local <- 0.006 # Transmission rate strain B local transmission
beta_A_truckp <- 0.00026 #Transmission rate vehicle transporting pigs to fms strain A
beta_B_truckp <- 0.0006 #Transmission rate vehicle transporting pigs to fms strain B
beta_A_Rebreak <- 0.0019 #Transmission rate re-break for sow farms strain A
beta_B_Rebreak <- 0.0010 #Transmission rate re-break for sow farms strain A
#gamma_A <- 0.1 #  recovery rate strain A, not used anymore
#gamma_B <- 0.12 # recovery rate strain B, not used anymore

# Initialize vectors for susceptible (S), infectious (I) individuals
Status <- c("A", rep("B", 5),rep("S",44)) #50 values for 50 farms
print(Status)## S, A, B, AB   initial statuses for farms
#detection status matrix
DetectionStatus <- matrix("ND", nrow = n_farms, ncol = ntimesteps)
print(DetectionStatus)
time_infected <- rep(0, n_farms)
print(time_infected)
# Initialize the Results matrix with farms as rows and timesteps as columns
Results <- matrix(NA, nrow = n_farms, ncol = ntimesteps)
Results[,1] <- Status
print(Results)


#random function to check
print_with_title <- function(title, obj) {
  cat(title, ":\n")
  print(obj)
  cat("\n")
}
  # ---- Transition function ---- #
stepF <- function(Status, Results, movMatrix, gravMatrix, truck_pig_matrix,
                  prob_survival, farms, beta_A, beta_B, beta_A_local, beta_B_local,
                  beta_A_truckp, beta_B_truckp, beta_A_Rebreak, beta_B_Rebreak,
                  DetectionStatus, time_infected, timestep) {
  tmpS <- Status
  #status detection
  StatusA <- which(tmpS=="A" | tmpS=="AB")
  statusA <- which(tmpS=="A")
  statusB <- which(tmpS=="B")
  StatusB <- which(tmpS=="B" | tmpS=="AB")
  Sus <- which(tmpS=="S")
  
  print_with_title("StatusA", head(StatusA))
  print_with_title("StatusB", StatusB)
  print_with_title("Susceptible vector", Sus)
  
  # Update time infected
  #detections
  new_infected <- Status != "S" & Results[, timestep - 1] == "S"
  print_with_title("new infected", new_infected)
  continuing_infected <- Status != "S" & Results[, timestep - 1] != "S"
  print_with_title("still infected", continuing_infected)
  #update
  time_infected[new_infected] <- 1
  time_infected[continuing_infected] <- time_infected[continuing_infected] + 1
  time_infected[Status == "S"] <- 0
  
  print_with_title("time infected", time_infected)
  
  # Sum of asymptomatic farms to sent pigs to "i" susceptible (Sus)
  FtFA<-sapply(1:length(Sus),function(x) {sum(movMatrix[StatusA,x])}) #f to f A or AB to susceptible, result is a sum
  FtFB<-sapply(1:length(Sus),function(x) {sum(movMatrix[StatusB,x])}) # f to f B or AB to susceptible
  FtFAB <- sapply(1:length(which(tmpS=="A")),function(x) {sum(movMatrix[StatusB,x])}) # farm to farm B who sent pigs to status A
  FtFBA <- sapply(1:length(which(tmpS=="B")),function(x) {sum(movMatrix[StatusA,x])}) #farm to farm A who sent pigs to status B
  
  print_with_title("A", FtFA)
  print_with_title("B", FtFB)
  print_with_title("AB", FtFAB)
  print_with_title("BA", FtFBA)
  
  # Calculate transmission rates for each strain
  rateTransmission_net_A <- beta_A * FtFA #the original include the multiplication of (beta_net*seasonality[names(seasonality) == time_start[i]])
  rateTransmission_net_B <- beta_B * FtFB 
  rateTransmission_net_AB <- beta_B * FtFAB 
  rateTransmission_net_BA <- beta_A * FtFBA #because is about the farms with B that can get the A, they use the Beta A
  
  
  print_with_title("TR_net_A", rateTransmission_net_A)
  print_with_title("TR_net_B", rateTransmission_net_B)
  print_with_title("TR_net_AB", rateTransmission_net_AB)
  print_with_title("TR_net_BA", rateTransmission_net_BA)
  
  ###including local transmission
  #spatial_vector <- (I * as.numeric(quarantine_vector == 0)) %*% gravity_matrix
  
  spatialVectorA <- sapply(1:length(Sus),function(x) {sum(gravMatrix[StatusA,x])}) #attraction force susceptible farms to farms infected A or AB
  spatialVectorB <- sapply(1:length(Sus),function(x) {sum(gravMatrix[StatusB,x])}) #attraction force susceptible farms to farms infected with B or AB
  spatialVectorAB <- sapply(1:length(which(tmpS=="A")),function(x) {sum(gravMatrix[StatusB,x])}) # attraction force susceptible farms to farms infected with A
  spatialVectorBA <- sapply(1:length(which(tmpS=="B")),function(x) {sum(gravMatrix[StatusA,x])}) # attraction force susceptible farms to farms infected with B
  #enhance vegetation index(EVI), parameter e,  the barrier index from the recipient farm i which was scaled into values between [0, 1]
  #spatial_vector <- spatial_vector * evi_matrix[,i] 
  
  print_with_title("spatialvectorA", spatialVectorA)
  print_with_title("spatialvectorB", spatialVectorB)
  print_with_title("spatialvectorAB", spatialVectorAB)
  print_with_title("spatialvectorBA", spatialVectorBA)
  
  
  
  rateTransmission_local_A <- beta_A_local * spatialVectorA
  rateTransmission_local_B <- beta_B_local * spatialVectorB #(beta_local*seasonality[names(seasonality) == time_start[i]] ) * spatial_vector
  rateTransmission_local_AB <- beta_B_local * spatialVectorAB
  rateTransmission_local_BA <- beta_A_local * spatialVectorBA
  
  print_with_title("TR_localA", rateTransmission_local_A)
  print_with_title("TR_localB", rateTransmission_local_B)
  print_with_title("TR_localAB", rateTransmission_local_AB)
  print_with_title("TR_localBA", rateTransmission_local_BA)
  
  ### Including truck pig transmission
  truckPigVectorA <- sapply(1:length(Sus),function(x) {sum(truck_pig_matrix[StatusA,x])}) #attraction force susceptible farms to farms infected A or AB
  truckPigVectorB <- sapply(1:length(Sus),function(x) {sum(truck_pig_matrix[StatusB,x])}) #attraction force susceptible farms to farms infected with B or AB
  truckPigVectorAB <- sapply(1:length(which(tmpS=="A")),function(x) {sum(truck_pig_matrix[StatusB,x])}) # farm to farm B who sent pigs to status A
  truckPigVectorBA <- sapply(1:length(which(tmpS=="B")),function(x) {sum(truck_pig_matrix[StatusA,x])}) #farm to farm A who sent pigs to status B
  
  print_with_title("truckvectorA", truckPigVectorA)
  print_with_title("truckvectorB", truckPigVectorB)
  print_with_title("truckvectorAB", truckPigVectorAB)
  print_with_title("truckvectorBA", truckPigVectorBA)
  
  
  rateTransmission_truck_A <- beta_A_truckp * truckPigVectorA
  rateTransmission_truck_B <- beta_B_truckp * truckPigVectorB #(beta_local*seasonality[names(seasonality) == time_start[i]] ) * spatial_vector
  rateTransmission_truck_AB <- beta_B_truckp * truckPigVectorAB
  rateTransmission_truck_BA <- beta_A_truckp * truckPigVectorBA
  
  print_with_title("TR_truckA", rateTransmission_truck_A)
  print_with_title("TR_truckB", rateTransmission_truck_B)
  print_with_title("TR_truckAB", rateTransmission_truck_AB)
  print_with_title("TR_truckBA", rateTransmission_truck_BA)
  
  ### Including re-break only for sow farms
  
  sow_farms <- farms$farmID[farms$FarmType == "sow"] #gives the index of sow farms
  
  print_with_title("sow farms", sow_farms)
  
  # Checking reinfected before update
  print_with_title("reinfected before update", reinfected)
  
  #update of the last outbreak time
  reinfected <- reinfected %>%
    mutate(last_outbreak_time = ifelse(farm_id %in% sow_farms & Status == "S" & (Results[,i-1] == "A" | Results[,i-1] == "B" | Results[,i-1] == "AB"), 
                                          1, 
                                          ifelse(farm_id %in% sow_farms & Status == "S",
                                                 last_outbreak_time + 1, 
                                                 last_outbreak_time) ))
  
  # Checking reinfected after update
  print_with_title("reinfected after update", reinfected)
  
  # Join the filtered sow_reinfected with prob_survival
   sow_reinfected <- reinfected %>%
    left_join(prob_survival, by = c("last_outbreak_time" = "time"), relationship = "many-to-many") %>%
    mutate(Probability = replace_na(Probability, 0))
  
  # Checking the result of the join
  print_with_title("sow_reinfected", sow_reinfected)
  
  # Now use sow_reinfected for calculating re-break rates
  rateTransmission_ReBreak_A <- sow_reinfected$Probability * beta_A_Rebreak
  rateTransmission_ReBreak_B <- sow_reinfected$Probability * beta_B_Rebreak
  
  print_with_title("RT_rebreakA", rateTransmission_ReBreak_A)
  print_with_title("RT_rebreakB", rateTransmission_ReBreak_B)
  
  #sum of the transmission rates
  # Initial sum of the transmission rates without considering re-break rates
  rateTransmission_A <- rateTransmission_net_A + rateTransmission_local_A + rateTransmission_truck_A + rateTransmission_ReBreak_A
  rateTransmission_B <- rateTransmission_net_B + rateTransmission_local_B + rateTransmission_truck_B + rateTransmission_ReBreak_B
  rateTransmission_AB <- rateTransmission_net_AB + rateTransmission_local_AB + rateTransmission_truck_AB + rateTransmission_ReBreak_B
  rateTransmission_BA <- rateTransmission_net_BA + rateTransmission_local_BA + rateTransmission_truck_BA + rateTransmission_ReBreak_A
  print_with_title("ratetransmissionA",rateTransmission_A)
  print(c("ratetransmissionB",rateTransmission_B))
  print(c("ratetransmissionAB",rateTransmission_AB))
  print(c("ratetransmissionBA",rateTransmission_BA))
  
 
  
  # Update status based on transmission rates
  calculate_infected <- function(rateTransmission, data , dt) {
    random_num <- runif(length(data))
    rateTransmission_Sus <- rateTransmission[data]
    inf_prob <- 1 - exp(-rateTransmission_Sus * dt)
    infected <- data[random_num < inf_prob]
    return(infected)
  }
  # Calculate the infected farms for each strain
  infA <- calculate_infected(rateTransmission_A, Sus, dt)
  infB <- calculate_infected(rateTransmission_B, Sus, dt)
  ## Status A who get B
  AinfB <- calculate_infected(rateTransmission_AB, statusA, dt)
  ## Status B who get A
  BinfA <- calculate_infected(rateTransmission_BA, statusB, dt)
  
  #checking the results
  print(c("which farm get A",infA))
  print(c("which farm get B",infB))
  print(c("which A farm get B", AinfB))
  print(c("which B farm get A", BinfA))
  
  
  
  #update status based on transmission rates
  
  Status[which(tmpS=="S")[infA]] <- "A"
  Status[which(tmpS=="S")[infB]] <- "B"
  Status[which(tmpS=="S")[which(infA %in% infB)]] <- "AB"
  Status[which(tmpS=="A")[AinfB]] <- "AB"
  Status[which(tmpS=="B")[BinfA]] <- "AB"
  
  print_with_title("update_status", Status)
  
  ##detection  
  # Calculate detection probabilities
  # Calculate detection probabilities
  detection_probabilities <- sapply(1:n_farms, function(j) {
    if (Status[j] == "infected" & DetectionStatus[j, timestep-1] == "ND") {
      return(detect_prob(farms$FarmType[j], Status[j], time_infected[j]))
    } else {
      # Return 0 for farms that are not infected or have already been detected
      return(0)
    }
  })
  print(c("detectionstatusupdated", DetectionStatus))
  
  
  # Transition to susceptible based on recovery time
  recovery_thresholds <- c(nursery = 8, finisher = 26, gdu = 23, sow= rpois(1, lambda = 41))
  recovery_time <- sapply(farms$FarmType, function(ft) recovery_thresholds[ft])
  
  print(c("check recovery thresholds", recovery_thresholds))
  
  print(c("recoveries", recovery_time))
  
  
  # Transition to susceptible based on recovery time
  Status[time_infected >= recovery_time] <- "S"
  
  return(Status)
  
}


# Initialize the Results matrix again
Results <- matrix(NA, nrow = N, ncol = ntimesteps)
# Define initial status
Status <- c(rep("A", 5), "B", rep("S", N-6)) ## S, A, B, AB)
Results[, 1] <- Status   # Store the initial status in the Results matrix  
n_farms<- 50
n_infected_A <- matrix(0, nrow = n_farms, ncol = timesteps)
n_infected_B <- matrix(0, nrow = n_farms, ncol = timesteps)
n_infected_AB <- matrix(0, nrow = n_farms, ncol = timesteps)


# Loop over timesteps
timesteps <- 100
for (i in 2:timesteps) {
  print(c("Step", i))
  Status <- stepF(Status, Results, movement_matrix[[i]], gravity_matrix,
                  truck_pig_matrix[[i]], prob_survival, farms, beta_A,
                  beta_B, beta_A_local, beta_B_local, beta_A_truckp, 
                  beta_B_truckp, beta_A_Rebreak, beta_B_Rebreak, 
                  DetectionStatus, time_infected, i)
  
  Results[,i] <- Status
  #count the number of infected farms
  n_infected_A[,i] <- sum(Status == "A")
  n_infected_B[,i] <- sum(Status == "B")
  n_infected_AB[,i] <- sum(Status == "AB")
}

Results

# Create a vector of dates
dates <- seq(as.Date("2024-01-01"), by = "week", length.out = timesteps)

plot


