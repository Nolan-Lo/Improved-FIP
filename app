library(tidyverse)
library(Lahman)
library(retrosheet)
library(shiny)
library(DT)
library(Metrics)

## 2017 ##

adv2017 <- read.csv("AdvPitching2017.csv")
std2017 <- read.csv("StdPitching2017.csv")
bip2017 <- read.csv("BipPitching2017.csv")

df2017 <- inner_join(adv2017, std2017, by = "Name") %>%
  inner_join(bip2017, by = "Name")

lgBIP <- 185295 - 40104 - 15829 - 970 - 1763
lgIFFB <- lgBIP*0.355*0.096
lgLD <- lgBIP*0.203
lgGB <- lgBIP*0.442

df2017 <- df2017 %>%
  mutate(BIP = TBF - SO - BB - IBB - HBP,
         LD = BIP*LDpct,
         GB = BIP*GBpct,
         FB = BIP*FBpct,
         IFFB = FB*IFFBpct,
         aFIP = (((13*(FB*0.136) + 3*(BB + HBP + 0.35*LD) - 
                     2*(SO + IFFB + 0.05*GB)) / IP) + 
                   (4.36 - (((13*6105) + (3*(15829 + 1763 + 0.35*lgLD)) - 
                               2*(40104 + lgIFFB + 0.05*lgGB)) / 43257.0))))

df2018ERA <- read.csv("ERA2018.csv")
df2017 <- df2017 %>%
  inner_join(df2018ERA, by = "Name")

## 2021 ##

adv2021 <- read.csv("AdvPitching2021.csv")
std2021 <- read.csv("StdPitching2021.csv")
bip2021 <- read.csv("BipPitching2021.csv")

df2021 <- inner_join(adv2021, std2021, by = "Name") %>%
  inner_join(bip2021, by = "Name")

lgBIP0 <- 181817 - 42145 - 15794 - 703 - 2112
lgIFFB0 <- lgBIP0*0.365*0.10
lgLD0 <- lgBIP0*0.207
lgGB0 <- lgBIP0*0.429

df2021 <- df2021 %>%
  mutate(BIP = TBF - SO - BB - IBB - HBP,
         Soft = BIP*Softpct,
         LD = BIP*LDpct,
         GB = BIP*GBpct,
         FB = BIP*FBpct,
         IFFB = FB*IFFBpct,
         aFIP = (((13*(FB*0.136) + 3*(BB + HBP + 0.35*LD) - 
                     2*(SO + IFFB + 0.05*GB)) / IP) + 
                   (4.27 - (((13*5944) + (3*(15794 + 2112 + 0.35*lgLD0)) - 
                               2*(42145 + lgIFFB0 + 0.05*lgGB0)) / 42615.0))))

## 2022 ##

adv2022 <- read.csv("AdvPitching2022.csv")
std2022 <- read.csv("StdPitching2022.csv")
bip2022 <- read.csv("BipPitching2022.csv")

df2022 <- inner_join(adv2022, std2022, by = "Name") %>%
  inner_join(bip2022, by = "Name")

lgBIP1 <- 182052 - 40812 - 14853 - 475 - 2046
lgIFFB1 <- lgBIP1*0.372*0.102
lgLD1 <- lgBIP1*0.199
lgGB1 <- lgBIP1*0.429

df2022 <- df2022 %>%
  mutate(BIP = TBF - SO - BB - IBB - HBP,
         Soft = BIP*Softpct,
         LD = BIP*LDpct,
         GB = BIP*GBpct,
         FB = BIP*FBpct,
         IFFB = FB*IFFBpct,
         aFIP = (((13*(FB*0.114) + 3*(BB + HBP + 0.35*LD) - 
                     2*(SO + IFFB + 0.05*GB)) / IP) + 
                   (3.97 - (((13*5215) + (3*(14853 + 2046 + 0.35*lgLD1)) - 
                               2*(40812 + lgIFFB1 + 0.05*lgGB1)) / 43075.1))))

## 2023 ##

adv2023 <- read.csv("AdvPitching2023.csv")
std2023 <- read.csv("StdPitching2023.csv")
bip2023 <- read.csv("BipPitching2023.csv")

df2023 <- inner_join(adv2023, std2023, by = "Name") %>%
  inner_join(bip2023, by = "Name")

lgBIP2 <- 184104 - 41843 - 15819 - 474 - 2112
lgIFFB2 <- lgBIP2*0.375*0.098
lgGB2 <- lgBIP2*0.425
lgLD2 <- lgBIP2*0.2

df2023 <- df2023 %>%
  mutate(BIP = TBF - SO - BB - IBB - HBP,
         GB = BIP*GBpct,
         LD = BIP*LDpct,
         FB = BIP*FBpct,
         IFFB = FB*IFFBpct,
         aFIP = (((13*(FB*0.127) + 3*(BB + HBP + 0.35*LD) - 
                     2*(SO + IFFB + 0.05*GB)) / IP) + 
                   (4.33 - (((13*5868) + (3*(15819+2112 + 0.35*lgLD2)) - 
                               2*(41843 + lgIFFB2 + 0.05*lgGB2)) / 43087.1))))

## Getting Projected ERA, Correlation, RMSE ##

df2022ERA <- df2022 %>%
  select(Name, ERA2022)
df2021 <- df2021 %>%
  inner_join(df2022ERA, by = "Name")

df2023ERA <- df2023 %>%
  select(Name, ERA2023)
df2022 <- df2022 %>%
  inner_join(df2023ERA, by = "Name")

lm_ERA0 <- lm(df2017$ERA2018 ~ df2017$ERA2017, df2017)
lm_FIP0 <- lm(df2017$ERA2018 ~ df2017$FIP, df2017)
lm_xFIP0 <- lm(df2017$ERA2018 ~ df2017$xFIP, df2017)
lm_SIERA0 <- lm(df2017$ERA2018 ~ df2017$SIERA, df2017)
lm_aFIP0 <- lm(df2017$ERA2018 ~ df2017$aFIP, df2017)

lm_ERA1 <- lm(df2021$ERA2022 ~ df2021$ERA2021, df2021)
lm_FIP1 <- lm(df2021$ERA2022 ~ df2021$FIP, df2021)
lm_xFIP1 <- lm(df2021$ERA2022 ~ df2021$xFIP, df2021)
lm_SIERA1 <- lm(df2021$ERA2022 ~ df2021$SIERA, df2021)
lm_aFIP1 <- lm(df2021$ERA2022 ~ df2021$aFIP, df2021)

lm_ERA2 <- lm(df2022$ERA2023 ~ df2022$ERA2022, df2022)
lm_FIP2 <- lm(df2022$ERA2023 ~ df2022$FIP, df2022)
lm_xFIP2 <- lm(df2022$ERA2023 ~ df2022$xFIP, df2022)
lm_SIERA2 <- lm(df2022$ERA2023 ~ df2022$SIERA, df2022)
lm_aFIP2 <- lm(df2022$ERA2023 ~ df2022$aFIP, df2022)

df2017 <- df2017 %>%
  mutate(proj_ERA2018_ERA = lm_ERA0$coefficients[1] + ERA2017*lm_ERA0$coefficients[2])
df2017 <- df2017 %>%
  mutate(proj_ERA2018_FIP = lm_FIP0$coefficients[1] + FIP*lm_FIP0$coefficients[2])
df2017 <- df2017 %>%
  mutate(proj_ERA2018_xFIP = lm_xFIP0$coefficients[1] + xFIP*lm_xFIP0$coefficients[2])
df2017 <- df2017 %>%
  mutate(proj_ERA2018_SIERA = lm_SIERA0$coefficients[1] + SIERA*lm_SIERA0$coefficients[2])
df2017 <- df2017 %>%
  mutate(proj_ERA2018_aFIP = lm_aFIP0$coefficients[1] + aFIP*lm_aFIP0$coefficients[2])

df2021 <- df2021 %>%
  mutate(proj_ERA2022_ERA = lm_ERA1$coefficients[1] + ERA2021*lm_ERA1$coefficients[2])
df2021 <- df2021 %>%
  mutate(proj_ERA2022_FIP = lm_FIP1$coefficients[1] + FIP*lm_FIP1$coefficients[2])
df2021 <- df2021 %>%
  mutate(proj_ERA2022_xFIP = lm_xFIP1$coefficients[1] + xFIP*lm_xFIP1$coefficients[2])
df2021 <- df2021 %>%
  mutate(proj_ERA2022_SIERA = lm_SIERA1$coefficients[1] + SIERA*lm_SIERA1$coefficients[2])
df2021 <- df2021 %>%
  mutate(proj_ERA2022_aFIP = lm_aFIP1$coefficients[1] + aFIP*lm_aFIP1$coefficients[2])

df2022 <- df2022 %>%
  mutate(proj_ERA2023_ERA = lm_ERA2$coefficients[1] + ERA2022*lm_ERA2$coefficients[2])
df2022 <- df2022 %>%
  mutate(proj_ERA2023_FIP = lm_FIP2$coefficients[1] + FIP*lm_FIP2$coefficients[2])
df2022 <- df2022 %>%
  mutate(proj_ERA2023_xFIP = lm_xFIP2$coefficients[1] + xFIP*lm_xFIP2$coefficients[2])
df2022 <- df2022 %>%
  mutate(proj_ERA2023_SIERA = lm_SIERA2$coefficients[1] + SIERA*lm_SIERA2$coefficients[2])
df2022 <- df2022 %>%
  mutate(proj_ERA2023_aFIP = lm_aFIP2$coefficients[1] + aFIP*lm_aFIP2$coefficients[2])

df2023 <- df2023 %>%
  mutate(proj_ERA2024_ERA = lm_ERA2$coefficients[1] + ERA2023*lm_ERA2$coefficients[2],)
df2023 <- df2023 %>%
  mutate(proj_ERA2024_FIP = lm_FIP2$coefficients[1] + FIP*lm_FIP2$coefficients[2])
df2023 <- df2023 %>%
  mutate(proj_ERA2024_xFIP = lm_xFIP2$coefficients[1] + xFIP*lm_xFIP2$coefficients[2])
df2023 <- df2023 %>%
  mutate(proj_ERA2024_SIERA = lm_SIERA2$coefficients[1] + SIERA*lm_SIERA2$coefficients[2])
df2023 <- df2023 %>%
  mutate(proj_ERA2024_aFIP = lm_aFIP2$coefficients[1] + aFIP*lm_aFIP2$coefficients[2])

  ## Tables of Results ##

x2017 <- c("ERA", "FIP", "xFIP", "aFIP", "SIERA")
y2017 <- c(cor(df2017$ERA2018, df2017$ERA2017), 
           cor(df2017$ERA2018, df2017$FIP),
           cor(df2017$ERA2018, df2017$xFIP),
           cor(df2017$ERA2018, df2017$aFIP),
           cor(df2017$ERA2018, df2017$SIERA))
z2017 <- c(rmse(df2017$ERA2018, df2017$proj_ERA2018_ERA),
           rmse(df2017$ERA2018, df2017$proj_ERA2018_FIP),
           rmse(df2017$ERA2018, df2017$proj_ERA2018_xFIP),
           rmse(df2017$ERA2018, df2017$proj_ERA2018_aFIP),
           rmse(df2017$ERA2018, df2017$proj_ERA2018_SIERA))
table2017 <- data.frame(x2017, y2017, z2017)
table2017 <- table2017 %>% mutate(across(where(is.numeric), round, 4))
colnames(table2017) <- c("2017 Estimators", "Correlation with 2018 ERA", "RMSE with 2018 ERA")

x2021 <- c("ERA", "FIP", "xFIP", "aFIP", "SIERA")
y2021 <- c(cor(df2021$ERA2022, df2021$ERA2021), 
           cor(df2021$ERA2022, df2021$FIP),
           cor(df2021$ERA2022, df2021$xFIP),
           cor(df2021$ERA2022, df2021$aFIP),
           cor(df2021$ERA2022, df2021$SIERA))
z2021 <- c(rmse(df2021$ERA2022, df2021$proj_ERA2022_ERA),
           rmse(df2021$ERA2022, df2021$proj_ERA2022_FIP),
           rmse(df2021$ERA2022, df2021$proj_ERA2022_xFIP),
           rmse(df2021$ERA2022, df2021$proj_ERA2022_aFIP),
           rmse(df2021$ERA2022, df2021$proj_ERA2022_SIERA))
table2021 <- data.frame(x2021, y2021, z2021)
table2021 <- table2021 %>% mutate(across(where(is.numeric), round, 4))
colnames(table2021) <- c("2021 Estimators", "Correlation with 2022 ERA", "RMSE with 2022 ERA")

x2022 <- c("ERA", "FIP", "xFIP", "aFIP", "SIERA")
y2022 <- c(cor(df2022$ERA2023, df2022$ERA2022), 
           cor(df2022$ERA2023, df2022$FIP),
           cor(df2022$ERA2023, df2022$xFIP),
           cor(df2022$ERA2023, df2022$aFIP),
           cor(df2022$ERA2023, df2022$SIERA))
z2022 <- c(rmse(df2022$ERA2023, df2022$proj_ERA2023_ERA),
           rmse(df2022$ERA2023, df2022$proj_ERA2023_FIP),
           rmse(df2022$ERA2023, df2022$proj_ERA2023_xFIP),
           rmse(df2022$ERA2023, df2022$proj_ERA2023_aFIP),
           rmse(df2022$ERA2023, df2022$proj_ERA2023_SIERA))
table2022 <- data.frame(x2022, y2022, z2022)
table2022 <- table2022 %>% mutate(across(where(is.numeric), round, 4))
colnames(table2022) <- c("2022 Estimators", "Correlation with 2023 ERA", "RMSE with 2023 ERA")


df2023_proj <- df2023 %>% select(Names, ERA2023, FIP, xFIP, SIERA, aFIP, 
                                 proj_ERA2024_FIP, proj_ERA2024_xFIP, proj_ERA2024_SIERA, proj_ERA2024_aFIP) %>%
  arrange(proj_ERA2024_aFIP) %>% head(100)
df2023_proj <- df2023_proj %>% mutate(across(c('aFIP','proj_ERA2024_FIP', 'proj_ERA2024_xFIP', 
                                               'proj_ERA2024_SIERA', 'proj_ERA2024_aFIP'), round, 4))

ui <- fluidPage(
  
  mainPanel(
    div(
      style = "margin-left: auto; margin-right: auto; text-align: left; max-width: 800px",
      
      HTML("<div style='font-size: 40px;'><b><br />Improving FIP:
      Creating a New Stat Based on FIP to Analyze and Value Pitchers</b></div>
      <div style='font-size: 20px;'>Nolan Lo</div>
      <div style='font-size: 20px;'>January 11th, 2024</div>
          <br />
          <div style='font-size: 26px;'><b>Introduction</b></div>
          <div style='font-size: 16px;'>
           Baseball is a story told by stats, and the way fans have approached 
           valuing certain stats has changed over the years. For analyzing pitchers, 
            fans have since evolved from valuing Wins and Losses, and even ERA. 
             The new stat gaining immense popularity in analyzing pitchers is FIP, 
             or Fielding Independent Pitching.</div>
        <br />
        <div style='font-size: 18px; text-align: center;'>
           FIP = ((13*HR)+(3*(BB+HBP))-(2*K))/IP + FIP Constant
        <br /><br />
           FIP Constant = lgERA – (((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIP)</div>
        <br />
        <div style='font-size: 16px;'>
           However, even FIP was optimized and thus a new stat was born, xFIP (Expected FIP). 
           xFIP is essentially the same as FIP but instead of accounting for the amount 
           of home runs a pitcher has allowed, it calculates the expected home runs based on 
           the amount of flyballs given up multiplied by the league average home run per 
           flyball percent. My goal is to optimize FIP and xFIP even further, prove my 
           stat's improvement in predicting future ERA, and use my improved stat to 
           predict MLB pitchers' ERA for the 2024 season.
        <br /></div>"),
      
      HTML("<br />
        <div style='font-size: 26px;'><b>Calculating the New Stat: aFIP (adjusted FIP)</b></div>
           <div style='font-size: 16px;'>
           The new stat I created, aFIP (adjusted FIP), utilizes the basis of xFIP while
           still maintaining the most important aspect, staying independent from fielding. 
           xFIP accounts for flyballs in its formula, and so does aFIP. However, aFIP 
           also accounts for the other hit types, being infield fly balls, line drives, 
           and groundballs. Infield flyballs are converted to outs at a near-perfect rate, 
           similar to strikeouts, so I treated them the same in the formula. Line drives 
           have a very high batting average and thus were treated like a walk but with 
           less weight. Groundballs have a below-average batting average and thus were  
           treated like a strikeout with a very low weight. The formula is as follows:
        <br /><br />
        <div style='font-size: 16px; text-align: center;'>
           aFIP = ((13*(Fly balls * lgHR/FB%))+(3*(BB+HBP+0.35*LD))-(2*(K+IFFB+0.05*GB))/IP + aFIP Constant
        <br /><br />
           aFIP Constant = lgERA – (((13*lgHR)+(3*(lgBB+lgHBP+0.35*lgLD))-(2*lgK+lgIFFB+0.05*lgGB))/lgIP)</div>
        <br /></div>"), 
      
      HTML("<br />
      <div style='font-size: 26px;'><b>Comparing Correlations of Stats</b></div>
          <div style='font-size: 16px;'>
           To analyze whether aFIP is an improvement when it comes to predicting future 
           ERAs, we can look at both the correlation and RMSE of each of the stats. 
           The stats we will analyze are current year ERA, FIP, xFIP, aFIP, and SIERA 
           as estimators to predict next year's  ERA. The goal is to have a higher correlation 
           and a lower RMSE. Let us take a look at how the 2022 season's estimator stats 
           did in predicting 2023 ERA. (Note: Only players who pitched at least 40 innings 
           in both years are being accounted for. 265 pitchers qualified for this span.)
           <br /></div>"),
      
      DTOutput("CorrTable2022"),
      
      HTML("<br />
          <div style='font-size: 16px;'>
           What we can see from this table is that ERA is by far the worst in predicting 
           future ERA. FIP is an improvement, but xFIP is considerably better. This is 
           anticipated since it removes park factors which do play a significant role 
           in a pitcher's production. This is why I had aFIP utilize the changes made by xFIP 
           and it ends up having the best Correlation and RMSE of the FIP stats. Lastly, 
           is one of the newer stats, SIERA, slightly edging out aFIP. Next, we can take a look at 
           the previous season, 2021, which did have some unique results to analyze.
           <br />"),
      
      DTOutput("CorrTable2021"),
      
      HTML("<br />
          <div style='font-size: 16px;'>
           Compared to 2022, 2021 had s much lower overall correlation. 2021 ERA did 
           roughly the same in predicting the following year as 2022 ERA did, but each 
           of the other stats did considerably worse. Uniquely enough as well, FIP 
           performed than both xFIP and aFIP. Since aFIP is based on xFIP, naturally 
           since xFIP did worse this season it follows that so will aFIP. It is interesting 
           to note though that aFIP still outperformed xFIP. However, the 2021 season 
           can be seen as a minority when it comes to FIP outperforming xFIP, so to confirm 
           and analyze aFIP further, let's look at a season a few years prior, 2017.
           <br />"),
      
      DTOutput("CorrTable2017"),
      
      HTML("<br />
          <div style='font-size: 16px;'>
           After seeing the analysis from 2017 estimators vs. 2018 ERA, we can see a drastic 
           increase in correlation for aFIP over FIP and xFIP, with SIERA again slighltly edging 
           out aFIP. This supports my theory that aFIP outperforms xFIP. We also see that the correlation 
           numbers overall are much closer to 2021's numbers bringing up the question if 
           2022's numbers are the outlier. Given datasets of every year and repeating the process, 
           these assumptions can be further supported. In conclusion, it seems that my 
           new stat aFIP does seem to outperform xFIP, sometimes by a significant margin as 
           well. Aside from minority seasons where FIP outperforms xFIP, aFIP seems to be much 
           more accurate in predicting future ERA, getting very close to SIERA. This brings to 
           question, what is SIERA and why use aFIP when SIERA is slightly better. Well, 
           SIERA is also like aFIP in that it accounts for balls in play while still staying 
           independent from fielding. However, the reason that aFIP is still relevant is for the 
           same simple reason FIP and xFIP are; calculating SIERA is extremely complicated for even 
           dedicated baseball analytics fans. aFIP takes the form of xFIP and still maintains 
           simplicity to the level where fans can look at the formula and understand 
           how aFIP is calculated. With all that being said, below is a table featuring the 
           top 100 pitchers' projected 2024 ERAs using aFIP, as well as each of their projected 
           ERAs using the other stats as predictors. 
           <br />"),
      
      DTOutput("ProjTable2023"),
      
      HTML("<br />
      <div style='font-size: 26px;'><b>Conclusion / Appendix</b></div>
           <div style='font-size: 16px;'>
           Thank you for reading my attempt at creating an optimized FIP and my analysis 
           on using each of them as predictors. It was my goal to create a stat that 
           came as close as possible to SIERA, while still maintaning an equation simple 
           enough to be interpreted by the common fan. 
           <br /><br />
           All data sets used were courtesy of fangraphs using this link here: 
           <br />
           https://www.fangraphs.com/leaders/major-league
           <br /><br /><br />")
    )
  )
)

# Define server
server <- function(input, output, session) {

  output$CorrTable2022 <- renderDT({
    
    datatable(
      table2022,
      options = list(dom = 't', paging = FALSE))
  })
  
  output$CorrTable2021 <- renderDT({
    
    datatable(
      table2021,
      options = list(dom = 't', paging = FALSE))
  })
  
  output$CorrTable2017 <- renderDT({
    
    datatable(
      table2017,
      options = list(dom = 't', paging = FALSE))
  })
  
  output$ProjTable2023 <- renderDT({
    
    datatable(
      df2023_proj,
      options = list(dom = 't', paging = FALSE))
  })
  
} 

# Run the Shiny app
shinyApp(ui, server)
