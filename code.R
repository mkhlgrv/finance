source('lib.R')
dt <- import('data.xlsx') %>% arrange(Date)
mean(dt$`PM US Equity  (L1)`)
mean(dt$`WFC US Equity  (R1)`)
var(dt$`PM US Equity  (L1)`)
var(dt$`WFC US Equity  (R1)`)
cor(dt$`PM US Equity  (L1)`, dt$`WFC US Equity  (R1)`)

ER <- c(mean(dt$`PM US Equity  (L1)`),
        mean(dt$`WFC US Equity  (R1)`))
COVMAT <- cov(dt[, c('PM US Equity  (L1)', 'WFC US Equity  (R1)')])

ef.frontier <- function(er = ER, cov.mat = COVMAT, n = 1000, alpha.min = -1, alpha.max = 2){
  alpha <- seq(alpha.min, alpha.max, length.out = n)
  alpha %>%
    map_dfr(function(alphai){
    eri <-  alphai*er[1] + (1-alphai)*er[2]
    stdevi <- sqrt(alphai^2*cov.mat[1,1] + (1-alphai)^2*cov.mat[2,2] + 2* abs(alphai*(1-alphai))*cov.mat[1,2])
    
    data.frame(alpha = alphai,
               er = eri,
               stdev = stdevi)
  })
}
ef1 <- ef.frontier()
ggplot(ef1)+
  geom_path(aes(stdev, er, color=alpha), size =1)+
  geom_point(aes(sd(dt$`PM US Equity  (L1)`), mean(dt$`PM US Equity  (L1)`)))+
  #geom_label(aes(sd(dt$`PM US Equity  (L1)`), mean(dt$`PM US Equity  (L1)`), label = 'PM US'))+
  geom_point(aes(sd(dt$`WFC US Equity  (R1)`), mean(dt$`WFC US Equity  (R1)`)))#+
  #geom_label(aes(sd(dt$`WFC US Equity  (R1)`), mean(dt$`WFC US Equity  (R1)`), label = 'WFC US'))
