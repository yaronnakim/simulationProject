##----------------------------------------- 1.  all functions ------------------------------------------------
addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}

qualityCheck <- function(ktzitzState){
  if(ktzitzState == 1)
    QCState = rdiscrete(1, c(0.85,0.15), c(1,2))
  if(ktzitzState == 2)
    QCState = rdiscrete(n=1, c(0.3,0.7), c(1,2))
  return(QCState)
}

ktzitzState <- function(){          #attribute for the ktzitza's state: 1 - proper, 2 - defected.
  state = rdiscrete(n=1, c(0.85, 0.15), c(1, 2))
  return(state)
}


##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime<-2*8*60 #daily model - how much does the sim need to run

dayWorkTime <- 3*60
QAinventory <- 0
# payForFixAmount <- 0
# testCost <- 0.75 
# numOfDefects <- 0 
boxRejected <- 0
trayCounter <- 0


##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mamara <- simmer("mamara")%>%
  add_resource("station1", capacity=1, queue_size=Inf)%>%
  add_resource("station2", capacity=1, queue_size=Inf)%>%
  add_resource("station3", capacity=1, queue_size=Inf)%>%
  add_resource("station4", capacity=1, queue_size=Inf)%>%
  add_resource("station5", capacity=1, queue_size=Inf)%>%
  add_resource("oven", capacity = 10, queue_size = Inf)%>%
  add_resource("workerB", capacity = 1, queue_size = Inf , preemptive = F)%>%  #the resource worker b take whille he pack
  add_resource("workerA", capacity = 1, queue_size = Inf)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------



throw <- 
  trajectory("throw")%>%
  log_("ooooooooooooooout")

basicLine <-                       #Stations 1-5 + distribution times for each station + giving the state feature at the end of the line
  trajectory("basicLine")%>%
  renege_in(10, throw)%>%
  log_("Started basicLine")%>%
  addService("station1", function() rnorm(1, 3/60, 1/60) + function() rnorm(1, 3/60, 1/60))%>%
  log_("finished s1")%>%
  addService("station2", function() rnorm(1, 2/60, 0.1/60) + function() rnorm(1, 5/60, 0.25/60))%>%
  log_("finished s2")%>%
  addService("station3", function() rnorm(1, 1/60, 0.25/60) + function() rnorm(1, 5/60, 1.5/60) + rnorm(1, 2/60, 0.65/60))%>%
  log_("finished s3")%>%
  addService("station4", function() rnorm(1, 5/60, 0.5/60))%>%
  log_("finished s4")%>%
  addService("station5", function() rnorm(1, 2/60, 0.4/60) + function() rnorm(1, 3/60, 0.5/60))%>%
  log_("finished s5")%>%
  set_attribute(key = "ktzitzState", value = function() ktzitzState)


baking <- 
  trajectory("baking")%>%
  batch(n=5, timeout=0, permanent = FALSE)%>%
  renege_abort()%>%
  addService("oven", function() 10)%>%
  log_("baking done")


packingTray <- 
  trajectory("packingTray")%>%
  if(QAinventory == 0){          #start of the day... will deliver 2 trays to QA inventory
    batch(n=20, timeout=0, permanent = FALSE)%>%    #tray
      addService("workerB", function() rnorm(1, 2/60, 0.001/60))%>%
      QAinventory = (QAinventory + 100) %>%
      timeout(function() 4)%>%
      rollback(amount = 4, times = 1) 
  }%>%
  batch(n=20, timeout=0, permanent = FALSE)%>%    #tray
  timeout(function() rnorm(1, 2/60, 0.001/60))%>%
  trayCounter = trayCounter + 1 %>%
  log_("im a tray")%>%
  batch(n=10, timeout=0, permanent = FALSE)%>% #BOX
  log_("im a box")


needRefill <-         #worker b leave his station and bring a plat to qa inventory
  trajectory("needRefill")%>%
  deactivate("reFillInventoryA")%>%
  addService("workerB",function() 4 )%>%
  QAinventory = (QAinventory + 100)

prepareBox <- 
  trajectory("prepareBox")%>%
  trayNum <- 0
  separate()%>%
  set_attribute(key = "trayNum", value = trayNum)%>%
  trayNum = trayNum+1 %>%
  batch(n=10, timeout=0, permanent = FALSE)

QAprocess <- 
  trajectory("QAprocess")%>%
  seperate()%>%
  seperate()%>%
  seize("workerA") %>%
  timeout = function() runif(1, 10, 30)%>%
  set_attribute(key = "QAState", value = qualityCheck(function() get_attribute("ktzitzState")))%>%
  if(function() get_attribute("QAState") == 2)
    timeout = rnorm(1, 30/60, 10/60)%>%
  QAinventory = QAinventory-1 %>%
  set_attribute(key = "ktzitzState", value = 1)%>%
  set_attribute(key = "QAState", value = 1)
  
    
  
QAtest <-                     #traj for testing the condition of the ktzitzot
  trajectory("QAtest")%>%
  if(QAinventory < 100){
    activate(generator = "reFillInventoryA")  
  }%>%
  trayToCheck <- sample(1:10, 1)
  separate()%>%
  if(function() get_attribute("trayNum") == trayToCheck)
    branch(1, c(TRUE), QAprocess)%>%
  batch(n=10, timeout=0, permanent = FALSE)
  


main <- 
  trajectory("main")%>%
  simmer::join(basicLine,
               baking,
               packingTray,
               prepareBox,
               QAtest
  )


##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
#main generator for ktzitz
add_generator(mamara,"ktzitza", main, distribution=function() 8/60,mon=2)
#generator for the first break
add_generator(mamara, "firstNeedBreak", at(320))
#generator for the seconed break
add_generator(mamara, "seconedNeedBreak", at(720))
#2 gennearators for mualfunctions with the acording distribution from fitDist_project file
add_generator(mamara, "malfunctionAD", distrubution = rnorm(1,normFit$estimate[1], normFit$estimate[2]), mon = 2)
add_generator(mamara, "malfunctionEJ", distrubution = rexp(1, expFit$estimate), mon = 2) 
add_generator(mamara, "reFillInventoryA", needRefill, at(now(mamara)), mon = 2, priority = 1 , preemptible = 1)

add_generator(mamara, "init", at(0), mon = 2) 


##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
set.seed(456)
reset(mamara)%>%
  simmer::run(simulationTime)