##----------------------------------------- 1.  all functions ------------------------------------------------

addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}

ktzitzState <- function(){          #attribute for the ktzitza's state: 1 - proper, 2 - defected.
  state = rdiscrete(n=1, c(0.85, 0.15), c(1, 2))
  return(state)
}

waitForLaneFinish <- function(){   
  while (get_queue_count(mamara, "station1") > 0 &
         get_queue_count(mamara, "station2") > 0 &
         get_queue_count(mamara, "station3") > 0 &
         get_queue_count(mamara, "station4") > 0 &
         get_queue_count(mamara, "station5") > 0 
  )
  {
   # log_("im waiting...")
  }
  timeout(function() 20)
}

breakWithSeize <- function(){  #optioenal
  #we can generate break that seize all he stations and then send the workers to 
  #break and after release the resources
 
  deactivate("main")%>%
  seize("station1", 1)%>%
  seize("station2", 1)%>%
  seize("station3", 1)%>%
  seize("station4", 1)%>%
  seize("station5", 1)%>%
  timeout(function() 20)%>%
  release("station1", 1)%>%
  release("station2", 1)%>%
  release("station3", 1)%>%
  release("station4", 1)%>%
  release("station5", 1)%>%
  activate("main")
}

randomSample <- function() {
  
}

fixInAD <- function() {
  deactivate("main")%>%
  function() payFixBytime(now(mamara))  
  function() rexp(1, 20)%>%     # diagnosis time fo the problem  & check if the rate is 20 or 20/60
    
  seize("station1", 1)%>%
  seize("station2", 1)%>%
  function() runif(10,30)%>%    # time to fix the machine
    
  release("station1", 1)%>%
  release("station2", 1)%>%
    
  activate("main")
}

fixInEJ <- function() {
  deactivate("main")%>%
  function() payFixBytime(now(mamara))%>% 
  function() rexp(1, 20)%>%     # diagnosis time fo the problem  & check if the rate is 20 or 20/60
  
  seize("station3", 1)%>%
  seize("station4", 1)%>%
  seize("station5", 1)%>%
  
  function() runif(10,30)%>%    # time to fix the machine
  
  release("station3", 1)%>%
  release("station4", 1)%>%
  release("station5", 1)%>%
  
  activate("main")
}

payFixByTime <- function(tempTime) {      #update the amount of money we pay for fixing the machine
  if(dayWorkTime - tempTime > 840 ){
    payForFixAmount = payForFixAmount + 1000
  }if(dayWorkTime - tempTime < 420){
    payForFixAmount = payForFixAmount + 1000
  }else{
    payForFixAmount = payForFixAmount + 500
  }
}

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime<-2*8*60 #daily model - how much does the sim need to run
# how we separate the times of the sim to know in witch hour of the day we are??
# cus we need to know how much to pay for mullfunction 500 or 1000?

dayWorkTime <- 960
QAinventory <- 0
payForFixAmount <- 0
toxicTime <-  10*60  #in seconds

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mamara <- simmer("mamara")%>%
  add_resource("station1", capacity=1, queue_size=Inf)%>%
  add_resource("station2", capacity=1, queue_size=Inf)%>%
  add_resource("station3", capacity=1, queue_size=Inf)%>%
  add_resource("station4", capacity=1, queue_size=Inf)%>%
  add_resource("station5", capacity=1, queue_size=Inf)%>%
  add_resource("oven", capacity = 10, queue_size = Inf)%>%
  add_resource("workerBpacking", capacity = 1, queue_size = Inf)%>%  #the resource worker b take whille he pack
  add_resource("workerBdeliver", capacity = 1, queue_size = Inf)%>%  #the resource worker b take when worker A need inventory refill
  add_resource("workerA", capacity = 1, queue_size = Inf)


##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

mullfunctionAD <- 
  trajectory("mullAD")%>%
  function() fixInAD


firstNeedBreak <-             #maybe we can just put the function in the generator??
  trajectory("firstBreak")%>%
  function() breakWithSeize

seconedNeedBreak <- 
  trajectory("seconedBreak")%>%
  function() breakWithSeize



basicLine <-                       #Stations 1-5 + distribution times for each station + giving the state feature at the end of the line
  trajectory("basicLine")%>%
  imAlive <-  proc.time()%>%          #sample the current time
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
  if(proc.time() - imAlive > toxicTime){      # if (finsh a-j time) -(stat time) > 10 then leave
    leave(1)
  }%>%
  set_attribute(key = "state", value = function() ktzitzState)


baking <- 
  trajectory("baking")%>%
  batch(n=5, timeout=0, permanent = FALSE)%>%
  addService("oven", function() 10)


packingTrayAndBox <- 
  trajectory("packingTray")%>% 
  if(QAinventory < 100) {
    set_capacity("workerBpacking", 0)%>%
    set_capacity("workerBdeliver", 1)      # need logic of priority
  }else{
    batch(n=20, timeout=0, permanent = FALSE)%>%
    timeout(function() rnorm(1, 2/60, 0.001/60))%>%
    batch(n=10, timeout=0, permanent = FALSE)
    }



# needBreak <-                  #after 4 hours of work the workers go to rest for 20 min and after return to work all together
#   trajectory("needBreak")%>%
#   deactivate("main")%>%
#   function()waitForLaneFinish()%>%
#   activate("main")

QAworkerNeedRefill <-         #worker b leave his station and bring a plat to qa inventory
  trajectory("needRefill")%>%
  set_capacity("workerBpacking",0)%>%
  set_capacity("workerBdeliver",1)%>%
  QAinventory = (QAworkerNeedRefill + 100) %>%
  timeout(function() 4)
  
  


# breakTimeOverGoToWork <-      #rest time is over
#   trajectory("finishRest")%>%
#   set_capacity("station1", 1)


QAtest <-                     #traj for testing the condition of the ktzitzot
  trajectory("QAtest")%>%
  separate()%>%
  
  randomSample100 <- function() randomSample
  

  
  
  
  
  Main <- 
  trajectory("main")%>%
  simmer::join(basicLine,
               baking,
               packing,
               QAtest
  )





##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
#main generator for ktzitz
add_generator(mamara, "main", distribution=function() 8/60,mon=2)
#generator for the first break
add_generator(mamara, "firstNeedBreak", at(320))
#generator for the seconed break
add_generator(mamara, "seconedNeedBreak", at(720))
#2 gennearators for mullfunctions with the acording distribution from fitDist_project file
add_generator(mamara, "mullfunctionAD", distrubution = rnorm(1,normFit$estimate[1], normFit$estimate[2]), mon = 2)
add_generator(mamara, "mullfunctionEJ", distrubution = rexp(1, expFit$estimate), mon = 2) 

##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
set.seed(456)
reset()%>%
  simmer::run(simulationTime)


MatnasData<-get_mon_arrivals()%>%
  mutate(waiting_time = end_time - start_time - activity_time)

ResourcesData <- get_mon_resources()
attributeData<-get_mon_attributes()

#plots 
plot()

arrivalsPerResource <- get_mon_arrivals()
