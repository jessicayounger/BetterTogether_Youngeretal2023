##k_calc for difficulty-weighted K values
#Takes in the trial level data, then gives each trial a weight depending on the degree of change (difficulty) 
#and whether the answer is correct or not. 
#While harder correct answers are worth more, harder incorrect answers are not penalized as much
#Jessica Younger and Buddy Lorentz 04-01-2019

k_calc = function(df){
  
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  
  df = df %>%
    select(c("pid","module","Time.Point","grade","condition","difficulty_level","degree_of_change","blue_distractors","trial_id","half","correct_button","cue_rotated","red_rectangles")) %>%
    filter(module=="FILTER") %>%
    droplevels()
  df$degree_of_change=as.numeric(df$degree_of_change)
  df$red_rectangles=as.numeric(df$red_rectangles)
  
  df$degrees = round(rad2deg(df$degree_of_change),digits=0)
 
  df$correct_button = ifelse(df$correct_button=="correct", 1, 0)
  
  df = df %>% 
    mutate(weight = case_when(
      correct_button==1 & abs(degrees)==90 ~.16,
      correct_button==1 &abs(degrees)==abs(75) ~.33,
      correct_button==1 & abs(degrees)==abs(60) ~.5,
      correct_button==1 &abs(degrees)==abs(45) ~.66,
      correct_button==1 &abs(degrees)==abs(30) ~.83,
      correct_button==1 &abs(degrees)==abs(15) ~1,
      abs(degrees)==abs(180) ~0,
      abs(degrees)==abs(0) ~0,
      correct_button==0 & abs(degrees)==90 ~1,
      correct_button==0 &abs(degrees)==abs(75) ~.83,
      correct_button==0 & abs(degrees)==abs(60) ~.66,
      correct_button==0 &abs(degrees)==abs(45) ~.5,
      correct_button==0 &abs(degrees)==abs(30) ~.33,
      correct_button==0 &abs(degrees)==abs(15) ~.16
    ))
  
  df = df %>% 
    group_by(pid,trial_id) %>% 
    mutate(adjusted_correct = correct_button*weight) %>% 
    ungroup()
  
  k2 = df %>% 
    group_by(pid,condition, red_rectangles) %>% 
    summarise(hitrate = sum(adjusted_correct) / sum(weight), correct_rejection_rate = mean(correct_button[cue_rotated==0])) %>% 
    ungroup()
  
  k= k2 %>% 
    group_by(pid,condition) %>% 
    mutate(k = ((hitrate - (1 - correct_rejection_rate))*red_rectangles)) %>% 
    select(-c(red_rectangles, hitrate,correct_rejection_rate))
  
  k = as.data.frame(k)
  
  k.spread = k %>%
    spread(condition,k) %>%
    as.data.frame()
  names(k.spread)=c("pid", "FILTER.k.r2b0", "FILTER.k.r2b2", "FILTER.k.r2b4", "FILTER.k.r4b0", "FILTER.k.r4b2")
  return(k.spread)
}
