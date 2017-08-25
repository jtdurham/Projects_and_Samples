# Note: None of the below process information is proprietary. It is a prototype of a general
# front of hospital to triage transition.

# Tier A simulation randomly generated a patient population with attributes based on historical data

# Tier B - Queueing

# Our objective: Simulate arrivals to the hospital and their movement into triage. Collect statistics
# on this process.

# Establish system properties and constraint parameters

# Note that a population must be simulated from Tier A before running Tier B. The number of patients simulated shoud be based on expected number of arrivals in Tier B
# The below CSV files are where decision parameters are kept and should be edited to reflect reality or test a hypothesis

#hos_info = read.csv("Hospital ID Simulation Info.csv") #Uncomment to simulate for each hospital. Make sure hospital level for Tier A was set to "HospitalID"
hos_info = read.csv("Hospital Size Simulation Info.csv") #Uncomment to simulate for each hospital size. Make sure hospital level for Tier A was set to "HospitalSize"

time_steps = 75
time_block = 1 #minute

hos_queue_sim = list()
components = c("Patients", "Front_Servers", "Triage_Rooms", "Front_Capacities", "Arrival_History")

# Simulate movement through the radiology department for each hopspital

for(h in 1:nrow(hos_info))
{
  # Instantiate servers and tracking lists
  
  patients = list()
  patient_id = 0
  
  front = 0
  capacities = integer(time_steps)
  arrival_track = integer(time_steps)
  
  front_servers = list()
  for(fs in 1:hos_info$Front_Servers[h])
  {
    front_servers[[fs]] = list(Server_ID = fs, Serving = NA, Time_Serving = 0, Total_Time_Served = 0)
  }
  
  to_front_serve = integer(0)
  
  triage_rooms = list()
  for(ts in 1:hos_info$Triage_Rooms[h])
  {
    triage_rooms[[ts]] = list(Room_ID = ts, Serving = NA, Time_Serving = 0, Total_Time_Served = 0)
  }
  
  for(block in 1:time_steps)
  {
    ################################# Front Office #################################
    
    to_triage = integer()
    
    # Simulate arrivals based on a Poisson distribution
    
    arrivals = rpois(1, hos_info$Arrival_Rate[h])
    front = front + arrivals
    if(front > hos_info$Capacity[h])
    {
      arrivals = arrivals - (front - hos_info$Capacity[h])
      front = hos_info$Capacity[h]
    }
    capacities[block] = front
    arrival_track[block] = arrivals
    
    # Instantiate patient movement values
    
    if(arrivals > 0)
    {
      for(a in 1:arrivals)
      {
        patient_id = patient_id + 1
        patients[[patient_id]] = list(Patient_ID = patient_id, Waiting_Time = 0)
        patients[[patient_id]]$ATO = sim_results[[h]]$ATO[patient_id] #ATO is Arrival to Order
        to_front_serve = c(to_front_serve, patient_id)
      }
    }
    
    time_advanced = 0
    advance = F
    to_adjust = numeric(length(front_servers))
    
    # Until no more servers are available, match arriving/wating patients to servers
    
    while(!advance)
    {
      if(length(to_front_serve) > 0)
      {
        for(p in to_front_serve)
        {
          patients[[p]]$Front_Service_Time = rexp(1, hos_info$Service_Rate[h]/time_block)
          found = F
          for(fs in 1:length(front_servers))
          {
            if(front_servers[[fs]]$Time_Serving == 0)
            {
              front_servers[[fs]]$Serving = p
              front_servers[[fs]]$Time_Serving = patients[[p]]$Front_Service_Time
              front_servers[[fs]]$Total_Time_Served = front_servers[[fs]]$Total_Time_Served + front_servers[[fs]]$Time_Serving
              found = T
              break
            }
          }
          if(found)
          {
            to_front_serve = to_front_serve[-which(to_front_serve == p)]
          }
        }
      
        total_time_served = numeric(length(front_servers))
        servers = integer(length(front_servers))
        time_left = numeric(length(front_servers))
        
        for(i in 1:length(front_servers))
        {
          servers[i] = i
          time_left[i] = front_servers[[i]]$Time_Serving
        }
        
        time_remaining = data.frame(Servers = servers, Time_Left = time_left)
        time_remaining = time_remaining[order(time_remaining$Time_Left, decreasing=F),]
        
        if((time_remaining$Time_Left[1] < time_block) & (total_time_served[time_remaining$Servers[1]] + time_remaining$Time_Left[1]) < time_block & (time_remaining$Time_Left[1] > 0))
        {
          for(wait in to_front_serve)
          {
            patients[[wait]]$Waiting_Time = patients[[wait]]$Waiting_Time + time_remaining$Time_Left[1]
          }
          front_servers[[time_remaining$Servers[1]]]$Time_Serving = 0
          to_triage = c(to_triage, front_servers[[time_remaining$Servers[1]]]$Serving)
          front_servers[[time_remaining$Servers[1]]]$Serving = NA
          time_advanced = time_advanced + time_remaining$Time_Left[1]
          total_time_served[time_remaining$Servers[1]] = total_time_served[time_remaining$Servers[1]] + time_advanced
          to_adjust[time_remaining$Servers[1]] = time_block - time_advanced
        } else
        {
          advance = T
        }
      } else
      {
        advance = T
      }
    }
    
    # Add waiting time to any patients still waiting for a server
    
    if(length(to_front_serve) > 0)
    {
      for(tfs in to_front_serve)
      {
        patients[[tfs]]$Waiting_Time = patients[[tfs]]$Waiting_Time + 1 - time_advanced
      }
    }
    
    # Adjust the service time remaining on each server
    
    for(fp in 1:length(front_servers))
    {
      if(to_adjust[fp] > 0)
      {
        front_servers[[fp]]$Time_Serving = max(0, front_servers[[fp]]$Time_Serving-to_adjust[fp])
      } else
      {
        front_servers[[fp]]$Time_Serving = max(0, front_servers[[fp]]$Time_Serving-1)
        if(front_servers[[fp]]$Time_Serving == 0)
        {
          if(!is.na(front_servers[[fp]]$Serving)) to_triage = c(to_triage, front_servers[[fp]]$Serving)
          front_servers[[fp]]$Serving = NA
        }
      }
    }
   
    ################################# Triage #################################
    
    to_exams = integer()
    
    # Simulate whether a patient will leave the system at this stage in the process
    
    if(length(to_triage)>0)
    {
      for(p in to_triage)
      {
        if(runif(1,0,1) < hos_info$Decay_Rate[h])
        {
          patients[[p]]$Discharge_After_Front = TRUE
          to_triage = to_triage[-which(to_exams == p)]
        }
      }
    }
    
    time_advanced = 0
    advance = F
    to_adjust = numeric(length(triage_rooms))
    for(p in to_triage)
    {
      patients[[p]]$Triage_Waiting_Time = 0
    }
    
    # Until no more servers are available, match arriving/wating patients to servers
    
    while(!advance)
    {
      if(length(to_triage) > 0)
      {
        for(p in to_triage)
        {
          patients[[p]]$Triage_Service_Time = ifelse(is.na(patients[[p]]$ATO), mean(visit_data$ArrivalToOrder_Avg, na.rm=T), patients[[p]]$ATO)
          found = F
          for(ts in 1:length(triage_rooms))
          {
            if(triage_rooms[[ts]]$Time_Serving == 0)
            {
              triage_rooms[[ts]]$Serving = p
              triage_rooms[[ts]]$Time_Serving = patients[[p]]$Triage_Service_Time
              triage_rooms[[ts]]$Total_Time_Served = triage_rooms[[ts]]$Total_Time_Served + triage_rooms[[ts]]$Time_Serving
              found = T
              break
            }
          }
          if(found)
          {
            to_triage = to_triage[-which(to_triage == p)]
            front = front - 1
          }
        }
        
        total_time_served = numeric(length(triage_rooms))
        servers = integer(length(triage_rooms))
        time_left = numeric(length(triage_rooms))
        
        for(i in 1:length(triage_rooms))
        {
          servers[i] = i
          time_left[i] = triage_rooms[[i]]$Time_Serving
        }
        
        time_remaining = data.frame(Servers = servers, Time_Left = time_left)
        time_remaining = time_remaining[order(time_remaining$Time_Left, decreasing=F),]
        
        if((time_remaining$Time_Left[1] < time_block) & (total_time_served[time_remaining$Servers[1]] + time_remaining$Time_Left[1]) < time_block & (time_remaining$Time_Left[1] > 0))
        {
          for(wait in to_triage)
          {
            patients[[wait]]$Triage_Waiting_Time = patients[[wait]]$Triage_Waiting_Time + time_remaining$Time_Left[1]
          }
          to_exams = c(to_exams, triage_rooms[[time_remaining$Servers[1]]]$Serving)
          triage_rooms[[time_remaining$Servers[1]]]$Time_Serving = 0
          triage_rooms[[time_remaining$Servers[1]]]$Serving = NA
          time_advanced = time_advanced + time_remaining$Time_Left[1]
          total_time_served[time_remaining$Servers[1]] = total_time_served[time_remaining$Servers[1]] + time_advanced
          to_adjust[time_remaining$Servers[1]] = time_block - time_advanced
        } else
        {
          advance = T
        }
      } else
      {
        advance = T
      }
    }
    
    # Add waiting time to any patients still waiting for a server
    
    if(length(to_triage) > 0)
    {
      for(ttr in to_triage)
      {
        patients[[ttr]]$Triage_Waiting_Time = patients[[ttr]]$Triage_Waiting_Time + 1 - time_advanced
      }
    }
    
    # Adjust the service time remaining on each server
    
    for(tp in 1:length(triage_rooms))
    {
      if(to_adjust[tp] > 0)
      {
        triage_rooms[[tp]]$Time_Serving = max(0, triage_rooms[[tp]]$Time_Serving-to_adjust[tp])
      } else
      {
        triage_rooms[[tp]]$Time_Serving = max(0, triage_rooms[[tp]]$Time_Serving-1)
        if(triage_rooms[[tp]]$Time_Serving == 0)
        {
          if(!is.na(triage_rooms[[tp]]$Serving)) to_exams = c(to_exams, triage_rooms[[tp]]$Serving)
          triage_rooms[[tp]]$Serving = NA
        }
      }
    }
  }
  hos_queue_sim[[h]] = list(patients, front_servers, triage_rooms, capacities, arrival_track)
  names(hos_queue_sim[[h]]) = components
}

names(hos_queue_sim) = hos_info$Hospital

# Pull queue statistics from the simulation

wait_history = list()
wait_history_labels = c("Front_Wait_Times", "Triage_Wait_Times")

for(h in 1:nrow(hos_info))
{
  wait_times = c(length(hos_queue_sim[[h]]$Patients))
  triage_wait_times = c(length(hos_queue_sim[[h]]$Patients))
  
  for(i in 1:length(hos_queue_sim[[h]]$Patients))
  {
    wait_times[i] = hos_queue_sim[[h]]$Patients[[i]]$Waiting_Time
    triage_wait_times[i] = ifelse(is.null(hos_queue_sim[[h]]$Patients[[i]]$Triage_Waiting_Time), NA, hos_queue_sim[[h]]$Patients[[i]]$Triage_Waiting_Time)
  }
  
  for(ts in 1:length(hos_queue_sim[[h]]$Triage_Rooms))
  {
    hos_queue_sim[[h]]$Triage_Rooms[[ts]]$Total_Time_Served = hos_queue_sim[[h]]$Triage_Rooms[[ts]]$Total_Time_Served - hos_queue_sim[[h]]$Triage_Rooms[[ts]]$Time_Serving
  }
  
  wait_history[[h]] = list(wait_times, triage_wait_times, radiology_wait_times, admit_wait_times)
  names(wait_history[[h]]) = wait_history_labels
}

names(wait_history) = hos_info$Hospital

##################################################
