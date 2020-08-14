
library(simmer)
library(simmer.plot)
"simulation of hospital admissions for surgery, 
to experiment with sending home patients or not"
hos<-simmer()
g.sendhome<-FALSE
g.patients<-1000
g.admit.duration<-c(100,10)
g.theatre.duration<-c(100,10)
g.postop.duration<-c(200,10)

daypatient<-trajectory() %>%
  log_("sent home") %>%
  timeout(300) %>%
  seize("admin",1) %>%
  timeout(rnorm(1,g.admit.duration[1],g.admit.duration[2])) %>%
  release("admin",1) %>%
  seize("bed",1)

immediate<-trajectory()%>%
  log_("immediate admit")%>%
  seize("bed",1)

patient <-trajectory() %>%
  log_("start pat") %>%
  seize("admin",1) %>%
  timeout(rnorm(1,g.admit.duration[1],g.admit.duration[2])) %>%
  release("admin",1) %>%
  log_(function()paste("getting bed:",toString(get_queue_count(hos,"bed")))) %>%
  branch(
    function () {
      crit<-get_attribute(hos,"crit")
      avail<-get_queue_count(hos,"bed")
      if( g.sendhome && avail==0 && crit == 1 ){
        return (TRUE)
      } else {
        return(FALSE)
        
        
        
      }
     } ,
     # actually want to inspect arrival and queues 
     continue= c(TRUE,FALSE),
     daypatient,
     immediate
   ) %>%
  log_("carrying on") %>%
  seize("bed",1) %>%
  seize("theatre",1) %>%
  timeout(rnorm(1,g.theatre.duration[1],g.theatre.duration[2])) %>%
  release("theatre",1) %>%
  timeout(rnorm(1,g.postop.duration[1],g.postop.duration[2])) %>%
  release("bed",1)
  
# some patients need treatment pdq...
serious<-data.frame(time=rpois(g.patients,30),crit=2,priority=TRUE)
# ... other less so
wounded<-data.frame(time=rpois(g.patients,30),crit=1,priority=FALSE)

hos  %>%
  add_resource("bed",30) %>%
  add_resource("admin",40) %>%
  add_resource("theatre",1) %>%
  add_dataframe("serious",
                patient,
                serious) %>%
  add_dataframe("wounded",
                patient,
                wounded)


hos %>% run(until = Inf)
arr<-get_mon_arrivals(hos)
arr$ill<-substr(arr$name,1,7)
arr$wait<-arr$end_time-arr$start_time
res<-get_mon_resources(hos)
#plot(get_mon_resources(hos), metric = "usage")


  
ggplot(arr, aes(end_time - start_time, fill = ill, color = ill)) +
  geom_histogram(position = "dodge") +
  scale_color_brewer(palette = "Set1")
# run 100 times
# mhos<-lapply(1:100,function(i){
#   run(hos,until = Inf)
# })
# 
# plot(get_mon_resources(mhos),metric="usage")

       