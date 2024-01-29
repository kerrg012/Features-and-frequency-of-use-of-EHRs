
library(tidyverse)
library(gtsummary)
library(corrplot)
library(rcompanion)
set.seed(3)


#### setup #### 
data <- read_csv("insight1605.csv")

data <- data %>%
  rename(consent = QID3,
         gender = Q1,
         age = Q2,
         GPduration = Q5,
         GPhours = Q6,
         teaching = Q7,
         country = Q9,
         setting = Q10,
         EHR = Q11,
         EHRduration = Q12,
         EHRfreq = Q14,
         # uses:
         EHR_tests = Q15_1,
         EHR_orders = Q15_2,
         EHR_alerts = Q15_3,
         EHR_providers = Q15_4,
         EHR_patients = Q15_5,
         EHR_patientEHR = Q15_6,
         EHR_patientEd = Q15_7,
         EHR_monitoring = Q15_8,
         EHR_admin = Q15_9,
         EHR_reports = Q15_10,
         # digital maturity
         DM_systemuse = Q16_1,
         DM_systemready = Q16_2,
         DM_ability = Q16_3,
         DM_communicate = Q16_4,
         DM_eval= Q16_5,
         DM_positive = Q16_6,
         # impacts:
         iEHR_harm = Q17_1,
         iEHR_ppref = Q17_2,
         iEHR_services = Q17_3,
         iEHR_waste = Q17_4,
         iEHR_delays = Q17_5,
         iEHR_quality = Q17_6,
         # technologies:
         tech_records = Q18_1,
         tech_phone = Q18_2,
         tech_chat = Q18_3,
         tech_video = Q18_4,
         tech_triage = Q18_5,
         tech_monitoring = Q18_6,
         tech_patient = Q18_7,
         tech_msg = Q18_8,
         tech_other = Q18_9,
         tech_other_text = Q18_9_TEXT,
         # hours per week
         hours_records_B = Q20_1_1,
         hours_records_A = Q20_1_2,
         hours_phone_B = Q20_2_1,
         hours_phone_A = Q20_2_2,
         hours_chat_B = Q20_3_1,
         hours_chat_A = Q20_3_2,
         hours_video_B = Q20_4_1,
         hours_video_A = Q20_4_2,
         hours_triage_B = Q20_5_1,
         hours_triage_A = Q20_5_2,
         hours_monitoring_B = Q20_6_1,
         hours_monitoring_A = Q20_6_2,
         hours_patient_B = Q20_7_1,
         hours_patient_A = Q20_7_2,
         hours_msg_B = Q20_8_1,
         hours_msg_A = Q20_8_2,
         # Training and guidance
         train_offer = Q21_1,
         train_do = Q21_2,
         guide_local = Q21_3,
         guide_national = Q21_4,
         guide_cybersecurity = Q21_5,
         train_trainees = Q21_6,
         # recommendations
         rec_symptom = Q31_1,
         rec_info = Q31_2,
         rec_trackers = Q31_3,
         rec_mindfulness = Q31_4,
         rec_counselling = Q31_5,
         rec_crisis = Q31_6,
         # overall experience
         experience = Q22,
         # patient care impacts:
         pimpact_records = Q23_1,
         pimpact_phone = Q23_2,
         pimpact_chat = Q23_3,
         pimpact_video = Q23_4,
         pimpact_triage = Q23_5,
         pimpact_monitoring = Q23_6,
         pimpact_patient = Q23_7,
         pimpact_msg = Q23_8,
         # clinical practice impacts
         cimpact_covid = Q24_1,
         cimapct_monitoring = Q24_2,
         cimpact_preventative = Q24_3,
         cimpact_chronic = Q24_4,
         cimpact_acute = Q24_5,
         cimpact_patient = Q24_6,
         cimpact_provider = Q24_7,
         cimpact_emotion = Q24_8,
         cimpact_satisfaction = Q24_9,
         cimpact_delays = Q24_10,
         cimpact_input = Q24_11,
         cimpact_escalation = Q24_12,
         cimpact_equity = Q24_13,
         cimapct_waste = Q24_14,
         benefits = Q30,
         challenges = Q25,
         # wants for evolution
         evolve_records = Q27_1,
         evolve_phone = Q27_2,
         evolve_chat = Q27_3,
         evolve_video = Q27_4,
         evolve_triage = Q27_5,
         evolve_monitoring = Q27_6,
         evolve_patient = Q27_7,
         evolve_msg = Q27_8,
         evolve_other = Q27_9,
         evolve_other_text = Q27_9_TEXT,
         barriers3 = Q28,
         qualities3 = Q29,
         qualities3_text = Q29_9_TEXT,
         parent_topics = `Q30...Parent.Topics`,
         topics = `Q30...Topics`) %>%
  filter(EHR=="Yes")

data$everyday <- ifelse(data$EHRfreq == "Everyday", 1, 0)
data<-data %>% filter(everyday =="1" | everyday=="0")


# format columns 
data$country[data$country=="United States of America"] <-"USA"

data$teaching <- factor(data$teaching, levels = c("Yes", "No", "Prefer not to answer"))
data$setting <- factor(data$setting, levels = c("Urban", "Mixed", "Rural"))
data$GPduration <- factor(data$GPduration, 
                          levels = c("< 5 years", "5 - 10 years", "10 - 15 years", 
                                     "15 - 20 years", "> 20 years"))

data$age <- factor(data$age, levels = c("Under 30", "30-39", "40-49", "50-59", 
                                        "60-69", "70+", "Prefer not to answer"))
data$GPhours <- as.numeric(data$GPhours)
data$GPhours[data$GPhours > 100] <- NA
data$GPhourscat <- cut(as.numeric(data$GPhours), breaks = c(-1, 28,36,40,100))
table(data$GPhourscat, exclude=F)
data$EHRduration <- factor(data$EHRduration, 
                           levels = c("Only after COVID-19 outbreak", 
                           "Before COVID-19 outbreak, but for less than 2 years",
                            "2-5 years", "5-10 years", "More than 10 years"))


data$country <- relevel(factor(data$country), ref = "Israel")

# digital literacy score
data$DM_ability <- ifelse(data$DM_ability =="Agree", 1, 0)
data$DM_systemuse <- ifelse(data$DM_systemuse =="Agree", 1, 0)
data$DM_systemready<- ifelse(data$DM_systemready =="Agree", 1, 0)
data$DM_communicate <- ifelse(data$DM_communicate =="Agree", 1, 0)
data$DM_eval <- ifelse(data$DM_eval =="Agree", 1, 0)
data$DM_positive <- ifelse(data$DM_positive =="Agree", 1, 0)

b <- colnames(b <- data %>% select(starts_with("DM_")))
data$DMscore <- factor(rowSums(data[,c(b)], na.rm=T))

#### 1. Inspect data ####

# missing data
miss_data <- data %>% 
  select(starts_with("EHR"),
         age, gender, setting, GPduration,teaching, GPhours, GPhourscat,  
         country, EHRduration)%>%
  gather(key, value) %>% 
  group_by(key) %>% 
  count(na = is.na(value)) %>% 
  pivot_wider(names_from = na, values_from = n, values_fill = 0) %>% 
  mutate(pct_missing = (`TRUE`/sum(`TRUE`, `FALSE`))*100) %>% 
  ungroup()

miss_data %>% 
  gt::gt()

miss_data %>%
  mutate(Present = 100 - pct_missing) %>%
  gather(Key, value, 4:5) %>%   
  mutate(Key = recode(Key, pct_missing = "Missing")) %>%
  ggplot(aes(x = reorder(key, `TRUE`), y = value, fill = Key)) +       
  geom_col(alpha = 0.85) +
  scale_fill_manual(name = "",
                    values = c('tomato3', 'steelblue'),
                    labels = c("Missing", "Present")) +
  coord_flip() +
  labs(x = NULL, y = "Missing (%)")


# Duration of EHRs use by years of clinical experience 
t <- data.frame(prop.table(table(data$GPduration, data$EHRduration), margin=2)*100)
t %>%
  rename(`Years of clinical experience` = Var1) %>%
  mutate(`Duration of EHR use` = Var2,
         Var2 = as.numeric(Var2)) %>%
  ggplot(aes(reorder(str_wrap(`Duration of EHR use`, width=16), Var2), Freq, fill=`Years of clinical experience`)) +
  geom_bar(stat="identity", position=position_dodge2()) +
  theme_bw(base_size=24) +
  xlab("Duration of EHRs use") +
  ylab("Percentage (%)") +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(0,100,10))

#### 2. Summary tables ####

# overall
out <- data %>%
  select(age, gender, setting, GPduration,teaching, GPhours, GPhourscat,  
         EHRduration, train_do, DMscore, country) %>%
  tbl_summary(
    label = list(age ~ "Age category",
                 gender ~ "Gender",
                 setting ~ "Setting",
                 GPduration ~ "Experience",
                 teaching ~ "Teaching activities",
                 GPhours ~ "Average hours of clinical work per week", 
                 GPhourscat ~ "Hours of clinical work per week",
                 EHRduration ~ "Duration of use",
                 train_do ~"Training in digital-first technologies",
                 DMscore~"Digital maturity Score"),
    missing="ifany",
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{median} ({p25} - {p75})"),
      all_categorical() ~ c("{n}"))
  )
out

# by frequency of use
out <- data %>%
  select(everyday, age, gender, setting, GPduration,teaching, GPhours, GPhourscat,  
         EHRduration, train_do, DMscore, country) %>%
  tbl_summary(by = everyday, 
              label = list(age ~ "Age category",
                           gender ~ "Gender",
                           setting ~ "Setting",
                           GPduration ~ "Experience",
                           teaching ~ "Teaching activities",
                           GPhours ~ "Average hours of clinical work per week", 
                           GPhourscat ~ "Hours of clinical work per week",
                           EHRduration ~ "Duration of use",
                           train_do ~"Training in digital-first technologies",
                           DMscore~"Digital maturity Score"),
              missing="ifany",
              type = all_continuous() ~ "continuous2",
              statistic = list(
                all_continuous() ~ c("{mean} ({sd})", "{median} ({p25} - {p75})"),
                all_categorical() ~ c("{n}"))
  )
out


#### 3. factors associated with everyday EHRs use ####

##### 3.1 univariable #####
cols <- data %>%
  select(gender, age, GPhours, GPhourscat, GPduration, setting, teaching, 
         EHRduration, train_do, DMscore, country) 
cols <- colnames(cols)

# set reference categories
data$train_do<-factor(data$train_do)
data$age <- relevel(data$age, ref = c("30-39"))
data$GPduration <- relevel(data$GPduration, ref = c("5 - 10 years"))
data$setting <- relevel(data$setting, ref = c("Urban"))
data$teaching <- relevel(data$teaching, ref = c("No"))
data$EHRduration <- relevel(data$EHRduration, ref = c("More than 10 years"))
data$DMscore <- relevel(factor(as.character(data$DMscore)), ref="3")

# loop over predictors
empty <- list()
for(a in 1:length(cols)) {
  var <- cols[a]
  dat <- data %>%
    filter(gender == "Female" | gender =="Male", age != "Prefer not to answer", 
           teaching != "Prefer not to answer") %>% 
    select(everyday, starts_with(var)) 
  colnames(dat) <- c("everyday", "var")
  mod <- glm(everyday ~ var, data = dat, family = binomial(link="logit"))
  out <- broom::tidy(mod, exponentiate=T, conf.int=T)
  out$var <- var
  empty[[a]] <- out
}
tab5 <- do.call("rbind", empty)
tab5$term <- str_replace(tab5$term, pattern="var", replace="")

# write_csv(tab5, "tab5.csv")


# null model vs univariable model
dat <- data %>%
  filter(gender == "Female" | gender =="Male", age != "Prefer not to answer", 
         teaching != "Prefer not to answer", !is.na(GPhourscat))
null <- glm(everyday ~ 1, data = dat, family = binomial(link="logit"))
mod <- glm(everyday ~ DMscore, data = dat, family = binomial(link="logit")) # swap out for variable of interest
anova(null, mod, test="LRT")

##### 3.2 multivariable #####

# select data
dat <- data %>%
  filter(gender == "Female" | gender =="Male", age != "Prefer not to answer", 
         teaching != "Prefer not to answer", !is.na(GPhours)) 

# set referebce categories
dat$EHRduration <- relevel(dat$EHRduration, 
                           ref = c("More than 10 years"))
dat$DMscore <- relevel(dat$DMscore, ref="4")
dat$country <- relevel(factor(dat$country), ref="Ireland")

# Run model
mod <- glm(everyday ~ GPhourscat + setting +EHRduration +country +DMscore, data = dat, family = binomial(link="logit"))

tab6 <- broom::tidy(mod, exponentiate=T, conf.int=T)
# write_csv(tab6, "tab6.csv")

# visualise effects plots
sjPlot::plot_model(mod, type = "eff")


#### 4. Fig 1: every day users by country ####

# percent everyday users
b1 <- data.frame(table(data$country, data$everyday)) %>%
  filter(Var2=="1") %>%
  rename(everyday = Var2, fevery = Freq)
b <- data.frame(table(data$country, data$everyday)) %>%
  filter(Var2=="0") %>%
  rename(less = Var2, fless = Freq) %>%
  left_join(b1) %>%
  mutate(total = fevery+fless, 
         perc = fevery/total*100)

# Plot specs
b <- b[order(b$perc, decreasing=T),]
b$order = 1:nrow(b)
b$country <- paste0(b$Var1, " (", b$total, ")")
b$country <- reorder(b$country, b$order, FUN = median)
# b$labs <- paste0(b$fevery, "/", b$total, " = ", format(round(b$perc, 1), nsmall=1), "%")
b$labs <- paste0(format(round(b$perc, 1), nsmall=1), "%")

# save plot
png(filename="everyday users by country.png", res=300, height=3000, width=6500)
b %>%
  ggplot(aes(perc, country)) + 
  geom_bar(stat="identity", fill = "steelblue3") +
  theme_classic(base_size=20) +
  ylab("") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  coord_cartesian(xlim=c(0,105)) +
  xlab("Everyday EHRs users (%)")  +
  geom_text(label=b$labs, nudge_x=4.5, size=6.5)
dev.off()

#### 5. EHRs feature availability ####
# EHRs features table
fun <- data %>% select(starts_with("EHR_"))

empty <- list()
for(i in 1:ncol(fun)){
  fun[[i]] <- factor(fun[[i]], levels = c("Yes", "Partially", "No"))
  empty[[i]] <- table(fun[[i]], exclude=F)
  
}
tab4 <- data.frame(do.call("rbind", empty))

# write_csv(tab4, "tab4.csv")

##### reference table #####
# reference table for plots - needed for this section
fun <- data %>%
  filter(!(is.na(EHR_reports))) %>%
  select(starts_with("EHR_"), everyday)

rm(i, empty)
empty <- list()
for(i in 1:(ncol(fun)-1)){
  fun[[i]] <- factor(ifelse(fun[[i]] == "No", 0, 1))
  tab <- data.frame(table(fun[[i]],  exclude=F)) %>%
    mutate(feature=colnames(fun)[i])

  empty[[i]] <- tab
}
tab <- data.frame(do.call("rbind", empty))

tab <- tab %>% filter(Var1==1) %>%
  mutate(perc = Freq/nrow(fun)*100) %>%
  arrange(perc) %>%
  select(feature) %>%
  rename(variable=feature)
tab$order <- 1:nrow(tab)


#### 5.1 Cramer's V #### 
cols <- c("gender", "age", "GPhours", "GPhourscat", "GPduration", 
                 "setting", "teaching", "EHRduration", "train_do", "DMscore")

dat <- data %>%
  filter(!is.na(EHR_reports))
fun <- dat %>%
  select(starts_with("EHR_"))

fun <- colnames(fun)
vars <- c("country", "DMscore", "setting")

empty2 <- list()
l=1
for(v in vars){
  print(v)
  tests <- list()
  
  for(i in 1:length(fun)){
    
    print(fun[[i]])
    dat$feature <- dat[[which(colnames(dat) == fun[[i]])]]
    
    dat$feature <- ifelse(as.character(dat$feature) == "No", 0, 1)
    
    tb <- table(dat[[v]], dat$feature)
    
    test <- cramerV(tb, ci=T)
    test$pvalue <- chisq.test(tb)$p.value
    test$feature <- fun[[i]]
    
    tests[[i]] <- test
    
  }
  
  df <- do.call("rbind", tests)
  df$var <- v
  
  empty2[[l]] <- df
  
  l=l+1
}


cram <- do.call("rbind", empty2)
cram$pvalue <- p.adjust(cram$pvalue) # adjust for multiple testing


cram <- cram %>%
  left_join(tab, by=c("feature" = "variable")) %>%
  mutate(padj = round(pvalue,3)) %>%
  mutate(cramers_txt = ifelse(padj<0.05,"sig","nosig")) %>%
  mutate(cramers = round(Cramer.V,3))
cram$feature<-reorder(cram$feature, cram$order)

cram$feature <- case_match(cram$feature, 
                           "EHR_tests" ~ "Access to test results",
                           "EHR_orders" ~ "Entering orders",
                           "EHR_alerts" ~ "Alerts and reminders",
                           "EHR_providers" ~ "Inter-provider communication",
                           "EHR_patients" ~ "Patient-provider communication",
                           "EHR_patientEHR" ~ "Patient EHRs access",
                           "EHR_patientEd" ~ "Patient education",
                           "EHR_monitoring" ~ "Home monitoring",
                           "EHR_admin" ~ "Admin tools",
                           "EHR_reports" ~ "Data standardisation")

cram$var[cram$var == "DMscore"] <-"Digital Maturity Score"
cram$feature <- str_wrap(cram$feature, 15)
cram$var <- str_wrap(cram$var, 15)

cram$var[cram$var=="country"] <- "Country"
cram$var[cram$var=="setting"] <- "Urbanicity"

# p value data frame
p <- cram %>%
  select(var, pvalue, feature) %>%
  pivot_wider(names_from = var, values_from = c("pvalue")) 
p <-data.frame( p[,-1])
p <- t(p)
colnames(p) <- unique(cram$feature)
rownames(p) <- unique(cram$var)

# lower 95% confidence interval
l <- cram %>%
  select(var, lower.ci, feature) %>%
  pivot_wider(names_from = var, values_from = c("lower.ci")) 
l <-data.frame( l[,-1])
l <- t(l)
colnames(l) <- unique(cram$feature)
rownames(l) <- unique(cram$var)

# upper 95% confidence interval
u <- cram %>%
  select(var, upper.ci , feature) %>%
  pivot_wider(names_from = var, values_from = c("upper.ci")) 
u <-data.frame( u[,-1])
u <- t(u)
colnames(u) <- unique(cram$feature)
rownames(u) <- unique(cram$var)

# data frame for plotting
b <- cram %>%
  select(var, Cramer.V, feature) %>%
  pivot_wider(names_from = var, values_from = c("Cramer.V")) 
b <-data.frame( b[,-1])
b <- t(b)
colnames(b) <- unique(cram$feature)
rownames(b) <- unique(cram$var)



png(filename="cramersV.png", res=300, height=2300, width=3400)
mar=c(0,0,1,0)
corrplot(b, method="shade",         
         is.corr=F,
         col.lim=c(-0.0,0.6),
         cl.pos = 'b', 
         cl.length=7, 
         cl.ratio = 0.5, #legend thickness
         col = COL1('Blues'), 
         tl.cex=1.5, tl.col='black', 
         cl.cex=1.3,
         p.mat=p, 
         addgrid.col = "grey50",
         pch.cex=5, pch.col='grey60',
         sig.level=0.05, insig='pch',  mar=c(1,1,1,1) # b, l, t, r
         # lowCI.mat=l, uppCI.mat = u # change plot type to reveal 95% CI
         )
text(5, -0.4,"Effect Size", cex=1.6)
dev.off()

#### 5.2 features vs country ####

# table of % with feature available by country 
data$country<-as.character(data$country)
data$country[data$country=="United States of America"] <- "USA"
data$key <- 1:nrow(data)
dat <- data %>% 
  filter(!(is.na(EHR_reports))) %>%
  select(starts_with("EHR_"), country, key)
rem <- dat %>% filter_at(vars(starts_with("EHR_")), all_vars(is.na(.)))
dat <- subset(dat, !(dat$key %in% rem$key))

ttl <- dat %>%  count(country)

for(i in 1:10){
  dat[[i]] <- ifelse(dat[[i]]=="No", 0, 1) # Partially or Yes counted as having feature available
}
dat[1:10] <- lapply(dat[1:10], as.numeric)

for(i in 1:10){
  b <- data.frame(table(dat$country, dat[[i]])) %>%
    filter(Var2==1) %>%
    select(-Var2) 
  colnames(b) <- c("country", colnames(dat)[[i]])
  ttl <-left_join(ttl, b)
  ttl[[i+2]] <- ttl[[i+2]]/ttl$n*100
}


# get country order
countries <- ttl$country
ttl <- data.frame(ttl[-c(1:2)])
rownames(ttl) <- countries
htq181mat <- as.matrix(ttl)

ord_countries <- hclust(dist(htq181mat),method = 'ward.D2')

# #dendrogram
# png(filename="dendrogram.png", res=1000, height=16, width=15.92, units="cm")
# plot(ord_countries, col = "#487AA1", col.main = "#45ADA8", col.lab = "black",
#      col.axis = "#F38630", lwd = 2,  sub = '', hang = -1, axes = FALSE,
#      main="", xlab="", ylab="", cex=1,mar=c(1,1,1,1))
# axis(side = 2, at = seq(0, 200,20), col = "#F38630",
#      labels = FALSE, lwd = 2, cex.axis=1)
# mtext(seq(0, 200,20), side = 2, at = seq(0, 200,20),
#       line = 1, col = "#A38630", las = 2, cex=1)
# mtext("Country",side=1, las = 1, cex=1)
# mtext("Height",side=2, cex=1,line=2.5, at=100)
# dev.off()

ord_countries <- ord_countries$labels[ord_countries$order]
ord_qs <- hclust(dist(t(htq181mat)),method = 'ward.D2')
ord_qs <- ord_qs$labels[ord_qs$order]

ttl$Country = countries
ttl <- reshape2::melt(ttl,id_vars = "Country")

# plot
labs<- c("Access to test results", "Entering orders", "Alerts and reminders",
            "Inter-provider communication","Patient-provider communication", 
            "Patient EHR access", "Patient education", "Home monitoring", "Admin tools",
            "Data standardisation")
dict <- data.frame(q = colnames(dat[1:10]), labs=labs)
ttl$lab <- plyr::mapvalues(ttl$variable,dict$q,str_wrap(dict$labs,15))
ttl$Country <- factor(ttl$Country,
                      levels = ord_countries, 
                      labels = str_wrap(ord_countries, 15))
ttl$variable <- factor(ttl$variable,levels = ord_qs)
ttl<-ttl %>%left_join(tab)
ttl$lab <- reorder(ttl$lab, as.numeric(ttl$order))
ttl$lab <- fct_rev(ttl$lab)
ttl.country <- ttl
# write_csv(ttl.country, "country features.csv")

rm(ttl, ord_countries, htq181mat, countries,b)

#### 5.3 features vs digital maturity ####
data$DMscore <- factor(as.character(data$DMscore))
data$key <- 1:nrow(data)
dat <- data %>% 
  filter(!(is.na(EHR_reports))) %>%
  select(starts_with("EHR_"), DMscore, key)
rem <- dat %>% filter_at(vars(starts_with("EHR_")), all_vars(is.na(.)))
dat <- subset(dat, !(dat$key %in% rem$key))

ttl <- dat  %>% count(DMscore)

for(i in 1:10){
  dat[[i]] <- ifelse(dat[[i]]=="No", 0, 1)
}
dat[1:10] <- lapply(dat[1:10], as.numeric)

for(i in 1:10){
  b <- data.frame(table(dat$DMscore, dat[[i]])) %>%
    filter(Var2==1) %>%
    select(-Var2) 
  colnames(b) <- c("DMscore", colnames(dat)[[i]])
  ttl <-left_join(ttl, b)
  ttl[[i+2]] <- ttl[[i+2]]/ttl$n*100
}



countries <- ttl$DMscore
ttl <- data.frame(ttl[-c(1:2)])
rownames(ttl) <- countries
htq181mat <- as.matrix(ttl)

# DM score dendrogram
ord_countries <- hclust(dist(htq181mat),method = 'ward.D2')
plot(ord_countries, col = "#487AA1", col.main = "#45ADA8", col.lab = "black",
     col.axis = "#F38630", lwd = 3,  sub = '', hang = -1, axes = FALSE, 
     main="", xlab="", ylab="")
axis(side = 2, at = seq(0, 80,10), col = "#F38630",
     labels = FALSE, lwd = 2, cex.axis=40)
mtext(seq(0, 80,10), side = 2, at = seq(0, 80,10),
      line = 1, col = "#A38630", las = 1, cex=1)
mtext("Digital Maturity Score",       side=1, las = 1, cex=1.5)
mtext("Height",side=2, cex=1.5,line=2.5, at=40)

# get DM score order
ord_countries <- ord_countries$labels[ord_countries$order]
ord_qs <- hclust(dist(t(htq181mat)),method = 'ward.D2')
ord_qs <- ord_qs$labels[ord_qs$order]
ttl$Country = countries
ttl <- reshape2::melt(ttl,id_vars = "Country")

# plot
labs<- c("Access to test results", "Entering orders", "Alerts and reminders",
         "Inter-provider communication","Patient-provider communication", 
         "Patient EHR access", "Patient education", "Home monitoring", "Admin tools",
         "Data standardisation")
dict <- data.frame(q = colnames(dat[1:10]), labs=labs)
ttl$lab <- plyr::mapvalues(ttl$variable,dict$q,str_wrap(dict$labs,15))
ttl$Country <- factor(ttl$Country,
                      levels = ord_countries,
                      # levels=as.character(0:6),
                      labels = str_wrap(ord_countries, 15))
ttl$variable <- factor(ttl$variable,levels = ord_qs)
ttl<-ttl %>%left_join(tab)
ttl$lab <- reorder(ttl$lab, as.numeric(ttl$order))
ttl$lab <- fct_rev(ttl$lab)
ttl.dm <- ttl


#### 5.4 features vs setting ####
data$setting <- factor(as.character(data$setting))
data$key <- 1:nrow(data)
dat <- data %>% 
  filter(!(is.na(EHR_reports))) %>%
  select(starts_with("EHR_"), setting, key)
rem <- dat %>% filter_at(vars(starts_with("EHR_")), all_vars(is.na(.)))
dat <- subset(dat, !(dat$key %in% rem$key))

ttl <- dat %>% count(setting)

for(i in 1:10){
  dat[[i]] <- ifelse(dat[[i]]=="No", 0, 1)
}
dat[1:10] <- lapply(dat[1:10], as.numeric)

for(i in 1:10){
  b <- data.frame(table(dat$setting, dat[[i]])) %>%
    filter(Var2==1) %>%
    select(-Var2) 
  colnames(b) <- c("setting", colnames(dat)[[i]])
  ttl <-left_join(ttl, b)
  ttl[[i+2]] <- ttl[[i+2]]/ttl$n*100
}


#
countries <- ttl$setting
ttl <- data.frame(ttl[-c(1:2)])
rownames(ttl) <- countries
htq181mat <- as.matrix(ttl)

ord_countries <- hclust(dist(htq181mat),method = 'ward.D2')
ord_countries <- ord_countries$labels[ord_countries$order]

ord_qs <- hclust(dist(t(htq181mat)),method = 'ward.D2')
ord_qs <- ord_qs$labels[ord_qs$order]

ttl$Country = countries
ttl <- reshape2::melt(ttl,id_vars = "Country")

# plot
labs<- c("Access to test results", "Entering orders", "Alerts and reminders",
         "Inter-provider communication","Patient-provider communication", 
         "Patient EHR access", "Patient education", "Home monitoring", "Admin tools",
         "Data standardisation")
dict <- data.frame(q = colnames(dat[1:10]), labs=labs)
ttl$lab <- plyr::mapvalues(ttl$variable,dict$q,str_wrap(dict$labs,15))
ttl$Country <- factor(ttl$Country,
                      levels = ord_countries, 
                      labels = str_wrap(ord_countries, 15))
ttl$variable <- factor(ttl$variable,levels = ord_qs)
ttl<-ttl %>%left_join(tab)
ttl$lab <- reorder(ttl$lab, as.numeric(ttl$order))
ttl$lab <- fct_rev(ttl$lab)
ttl.setting <- ttl

# inspect - little differences seen; confirmed by Cramer's V
ggplot(ttl) + 
  geom_tile(aes(x=Country,y=lab,fill=value),color="grey20") + 
  labs(x="Practice Setting",y="",fill="Percent with EHR feature (%)") +
  scale_fill_steps2(low = "chartreuse4",mid = "lightyellow",high = "firebrick3",midpoint = 50,
                    breaks=seq(0,100,10)) + 
  theme_bw(base_size=24) +
  theme(legend.position = 'bottom',
        legend.key.size = unit(24,"mm"),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5)) 


#### 5.5 heatmap plots ####

# set colour scale
low="snow"
mid = "skyblue"
high = "steelblue4"

# country plot
ttl.country$Country <- factor(ttl.country$Country, levels=c(
  "Israel", "USA", "Croatia", "Finland", "Spain", "Turkey","Sweden", "UK",
  "Portugal", "Australia", "Germany", "Italy","Ireland", "Canada", "France", 
  "Brazil", "Colombia","Chile", "Poland", "Slovenia"))
p1 <- ggplot(ttl.country) +
  geom_tile(aes(x=Country,y=lab,fill=value),color="grey20") +
  labs(x="Country",y="",fill="Percent with EHRs feature (%)") +
  scale_fill_steps2(low = low,mid=mid, high = high, midpoint = 50,
                    breaks=seq(0,100,10),limits=c(0,100)) +
  theme_bw(base_size=20) +
  theme(legend.position = 'bottom',
        legend.key.size = unit(20,"mm"),
        axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1),
        ) #+coord_flip()
p1
legend=cowplot::get_legend(p1)
png(filename="Figure 3.png", res=600, height=8.2, width=11.69, 
    unit ="in")
p1
dev.off()

# DM score plot
ttl.dm$Country <- factor(ttl.dm$Country, levels=as.character(0:6))
p2 <- ggplot(ttl.dm) + 
  geom_tile(aes(x=Country,y=lab,fill=value),color="grey20") + 
  labs(x="Digital Maturity Score",y="",fill="Percent with EHRs feature (%)") +
  scale_fill_steps2(low = low,mid=mid, high = high,midpoint = 50,
                    breaks=seq(0,100,10),limits=c(0,100)) + 
  theme_bw(base_size=20) +
  theme(legend.position = 'bottom',
        legend.key.size = unit(20,"mm"),
        axis.text.x = element_text(angle=0, hjust = 1,vjust = 1)) #+coord_flip()
p2
png(filename="Figure 4.png", res=600, height=8.2, width=11.69, 
    unit ="in")
p2
dev.off()


