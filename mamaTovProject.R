##----------------------------------------- 1.  all functions ------------------------------------------------

addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}

ktzitzState <- function(){          #attribute for ktzitza state: 1 - proper, 2 - defected.
  state = rdiscrete(n=1, c(0.85, 0.15), c(1, 2))
  return(state)
}




##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime<-2*8*60 #daily model

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mamara <- simmer("mamara")%>%
  add_resource("station1", capacity=1, queue_size=Inf)%>%
  add_resource("station2", capacity=1, queue_size=Inf)%>%
  add_resource("station3", capacity=1, queue_size=Inf)%>%
  add_resource("station4", capacity=1, queue_size=Inf)%>%
  add_resource("station5", capacity=1, queue_size=Inf)%>%
  add_resource("oven", capacity = 10, queue_size = Inf)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

basicLine <-                       #Stations 1-5 + distribution times for each station + giving the state feature at the end of the line
  trajectory("basicLine")%>%
  log_("Started basicLine")%>%
  addService("station1", function() rnorm(1, 3/60, 1/60) + rnorm(1, 3/60, 1/60))%>%
  log_("finished s1")%>%
  addService("station2", function() rnorm(1, 2/60, 0.1/60) + rnorm(1, 5/60, 0.25/60))%>%
  log_("finished s2")%>%
  addService("station3", function() rnorm(1, 1/60, 0.25/60) + rnorm(1, 5/60, 1.5/60) + rnorm(1, 2/60, 0.65/60))%>%
  log_("finished s3")%>%
  addService("station4", function() rnorm(1, 5/60, 0.5/60))%>%
  log_("finished s4")%>%
  addService("station5", function() rnorm(1, 2/60, 0.4/60) + rnorm(1, 3/60, 0.5/60))%>%
  log_("finished s5")%>%
  set_attribute(key = "state", value = function() ktzitzState)


baking <- 
  trajectory("baking")%>%
  batch(n=5, timeout=0, permanent = FALSE)%>%
  addService("oven", function() 10)


packingTray <- 
  trajectory("packingTray")%>% 
  batch(n=20, timeout=0, permanent = FALSE)
timeout = (function() rnorm(1, 2, 5))

packingBox <-
  trajectory("packingBox")%>%
  batch(n=10, timeout=0, permanent = FALSE)


needBreak <-                  #after 4 hours of work the workers go to rest
  trajectory("needBreak")%>%
  deactivate("main")%>%
  while (get_queue_count(mamara, "station1") > 0 &
         get_queue_count(mamara, "station2") > 0 &
         get_queue_count(mamara, "station3") > 0 &
         get_queue_count(mamara, "station4") > 0 &
         get_queue_count(mamara, "station5") > 0 
  )
  {
    b = 1
  }
timeout = 20


set_capacity("station1", 0)

breakTimeOverGoToWork <-      #rest time is over
  trajectory("finishRest")%>%
  set_capacity("station1", 1)


qTest <- 
  
  
  
  
  Main <- 
  trajectory("main")%>%
  simmer::join(basicLine,
               baking,
               packing
  )





##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

add_generator(mamara, "main", distribution=function() 8/60,mon=2)
add_generator(mamara, "needBreak", at(320))

##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
set.seed(456)
reset(Matnas)%>%
  simmer::run(simulationTime)



MatnasData<-get_mon_arrivals(Matnas)%>%
  mutate(waiting_time = end_time - start_time - activity_time)

ResourcesData <- get_mon_resources(Matnas)
attributeData<-get_mon_attributes(Matnas)

#plots 
plot(Gym)

arrivalsPerResource <- get_mon_arrivals(Matnas, per_resource=T)
