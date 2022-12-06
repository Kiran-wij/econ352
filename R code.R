
require(dplyr)

require(readstata13)

require(bunching)





loans = read_csv("~/Econ 352/HW2/HW3/all_llma_nc.csv")

loanLimits = read_csv("~/Econ 352/HW2/HW3/conformingloanlimits.csv")



# Preview the data (important for checking datatypes)

str(loans)

BINSIZE=function(){
  
  #multiply by BINSIZE cos bin size is 1/0.01
  
  return(100)
  
}



#Clean data

tmp = loans %>% 
  
  mutate(year = as.numeric(substr(as.character(loans$year), 1, 4))) %>% 
  
  # filter(1992 <= year, year <= 2015, product_type == 10, original_term == 360| original_term == 180, loan_type == 1, loan_purpose == 1|loan_purpose == 2|loan_purpose == 3|loan_purpose == 5) %>%
  
  inner_join(loanLimits, by="year") %>%
  
  mutate(deviation = log(original_balance/conformingloanlimit)) %>%
  
  filter(!is.na(deviation)) %>%
  
  mutate(bin = floor(deviation * BINSIZE())) %>%
  
  filter(abs(bin) <= 100) %>%
  
  filter(!is.na(initial_interest_rate)) %>%
  
  na.omit()



mean_initial_rates = tmp %>% group_by(bin) %>% summarise(mean_initial_i = mean(initial_interest_rate, na.rm=T))

freq = as.data.frame(table(tmp$bin))

names(freq) =c("bin", "deviation")

freq$bin = as.numeric(as.character(freq$bin))

fillEmpty= data.frame(bin=c(-BINSIZE():BINSIZE()), fillDev = 0)

binDat = inner_join(mean_initial_rates, freq, by='bin') %>%
  
  full_join(fillEmpty, by="bin") %>%
  
  mutate(deviation = abs(deviation)+fillDev) %>%
  
  select(-fillDev)



#Plot Fig 1A

plot(binDat$bin, binDat$mean_initial_i)

abline(v=0, lty=3)

#Before break

preBin = binDat %>% filter(bin<0)

preBinReg = lm(mean_initial_i ~bin, preBin)

preBinX = c(-BINSIZE():-1)

preBinY = predict(preBinReg)

lines(preBinX, preBinY, lty=3)

#After break

postBin = binDat %>% filter(bin>0)

postBinReg = lm(mean_initial_i ~bin, postBin) 

postBinX = c(1:BINSIZE())

postBinY = predict(postBinReg)

lines(postBinX, postBinY, lty=3)

#Plot Fig 1B

totalDev = sum(binDat$deviation)

binDat = binDat %>% 
  
  mutate(share=deviation/totalDev)

plot(binDat$bin, binDat$share, "h")



#Estimate bunching for deviation: just below and just above

preRates = predict(preBinReg, data.frame(bin=c(0)))/100

postRates = predict(postBinReg, data.frame(bin=c(0)))/100

options(warn=-1)

bunchit(tmp$deviation+4, zstar=4, t0=preRates, t1=postRates, notch=T, binwidth = 1/2/BINSIZE(), bins_l=200, bins_r=200, poly=13)
