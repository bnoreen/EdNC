#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(stringr)
library(tidyr)
library(plotly)
library(ggthemes)
library(gsheet)
url <- 'https://docs.google.com/spreadsheets/d/14iJjVuDdidxHEbv2rlfIyxUFOdx0ud1Nq-hImcxGVhU'
votes <- as.data.frame(gsheet2tbl(url))
url <- 'https://docs.google.com/spreadsheets/d/1TNhXGWyc7wpL-Mnr_72yxLi0d_qyNkUuNCXrhmZHXVI'
article <- as.data.frame(gsheet2tbl(url))
emails <- read.csv('~/data/emails.csv')
names(emails) <- c('UserId','Event','Time','Email','Subject')
article$StartTime <- as.Date(as.character(substr(article$StartTime,0,10)))
article[which(article$PublishDate=='NULL'),]$PublishDate = NA
article$PublishDate <- as.Date(as.character(substr(article$PublishDate,0,10)))
emails$Time <- as.Date(as.character(substr(emails$Time,0,10)))
names(votes)=c('Name','ResponseDate','UserIdString')
votes$ResponseDate = str_sub(votes$ResponseDate,1,10)
votes$ResponseDate = as.Date(votes$ResponseDate,'%Y-%m-%d')
temp_votes = votes[which(votes$ResponseDate>=as.Date('2018-07-01')),]
temp_votes = temp_votes[!duplicated(temp_votes[,c(1,3)]),]
temp_votes = as.data.table(temp_votes)
temp_votes = as.data.frame(temp_votes[, .N, by=.(week(ResponseDate))])
temp_votes = temp_votes[order(temp_votes$week),]
temp_votes$Total = cumsum(temp_votes$N)
temp_votes$Target = seq(0,(nrow(temp_votes)-1)*137*7,137*7)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  ####for the last 30 days
  articles1 <- article[which(article$StartTime >= Sys.Date()-30),]
  surveys1 <- votes[which(votes$ResponseDate >= Sys.Date()-30),]
  emails1 <- emails[which(emails$Time >= Sys.Date()-30),]
  
  articles1 <- as.data.frame(table(articles1$UserIdString))
  names(articles1) <- c('UserId','Articles')
  
  surveys1 <- surveys1[!duplicated(surveys1),]
  surveys1 <- as.data.frame(table(surveys1$UserIdString))
  names(surveys1) <- c('UserId','Surveys')
  
  emails1 <- emails1[which(emails1$Event==3),]
  emails1 <- as.data.frame(table(emails1$UserId))
  names(emails1) <- c('UserId','EmailOpens')
  
  loyalty <- merge(articles1,surveys1,by='UserId',all=T)
  loyalty <- merge(loyalty,emails1,by='UserId',all=T)
  
  loyalty[is.na(loyalty)] <- 0
  ####for the 30 days before the last 30 days
  articles2 <- article[which(article$StartTime >= Sys.Date()-60 & article$StartTime < Sys.Date()-30),]
  surveys2<- votes[which(votes$ResponseDate >= Sys.Date()-60 & votes$ResponseDate < Sys.Date()-30),]
  emails2<- emails[which(emails$Time >= Sys.Date()-60 & emails$Time < Sys.Date()-30),]
  
  articles2 <- as.data.frame(table(articles2$UserIdString))
  names(articles2) <- c('UserId','Articles')
  
  surveys2 <- surveys2[!duplicated(surveys2),]
  surveys2 <- as.data.frame(table(surveys2$UserIdString))
  names(surveys2) <- c('UserId','Surveys')
  
  emails2 <- emails2[which(emails2$Event==3),]
  emails2 <- as.data.frame(table(emails2$UserId))
  names(emails2) <- c('UserId','EmailOpens')
  
  loyalty2 <- merge(articles2,surveys2,by='UserId',all=T)
  loyalty2 <- merge(loyalty2,emails2,by='UserId',all=T)
  
  loyalty2[is.na(loyalty2)] <- 0

  output$reachvotes <- renderPlotly({

    plot_ly(temp_votes,x=~week,y=~Total,type='scatter',mode='lines',name='Current') %>%
      add_trace(y=~Target,type='scatter',name='Target')


  })

  output$text_reachvotes <- renderText({
    paste0('Goal:50000, Current:',temp_votes[nrow(temp_votes),]$Total,
           ', Expected:',temp_votes[nrow(temp_votes),]$Target)

  })

  output$subscribers <- renderPlotly({
    #SUBSCRIBER
    subs <- read.csv('~/subscribers.csv',stringsAsFactors = F)
    subs$SignupDate <- substr(subs$SignupDate,1,10)
    subs$SignupDate <- as.Date(as.character(subs$SignupDate))
    subs <- as.data.frame(table(subs$SignupDate))
    subs <- subs[order(subs$Var1),]
    subs$Total <- cumsum(subs$Freq)
    subs$Var1 <- as.Date(as.character(subs$Var1))
    subs <- subs[which(subs$Var1 >= '2018-07-01'),]
    subs$Target = seq(4398,4398+(nrow(subs)-1)*29.04658,29.04658)
    names(subs) = c('Date','Signups','Total','Target')
    plot_ly(subs,x=~Date,y=~Total,type='scatter',mode='lines',name='Current') %>%
    add_trace(y=~Target,type='scatter',name='Target')


  })

  output$article <- renderPlotly({

    article$Date =substr(article$StartTime,1,10)
    article <- as.data.frame(table(article$Date))
    article$Target = 1750
    plot_ly(article,x=~Var1,y=~Freq,type='bar',mode='bars',name='Current') %>%
      add_trace(y=~Target,type='scatter',name='Target')
  })

  output$loyaltyplot <- renderPlot({
    loyalty$Loyalty = as.numeric(loyalty$Articles*input$article +
      loyalty$Surveys*input$survey +
      loyalty$EmailOpens*input$email)
    loyalty[which(loyalty$Loyalty >=input$maxscore),]$Loyalty=input$maxscore
    ggplot(loyalty, aes(x=Loyalty)) +  geom_histogram(binwidth=2,color="white", fill="#6c134f") +
    theme_light() + ggtitle('Last 30 days of Loyalty')
  })

  output$loyaltyscore <- renderText({#this needs to update when the plot updates
    loyalty$Loyalty = as.numeric(loyalty$Articles*input$article +
                                   loyalty$Surveys*input$survey +
                                   loyalty$EmailOpens*input$email)
    loyalty2$Loyalty = as.numeric(loyalty2$Articles*input$article +
                                    loyalty2$Surveys*input$survey +
                                    loyalty2$EmailOpens*input$email)
    loyaltyscore_df <- merge(loyalty,loyalty2,by='UserId')
    nrow(loyaltyscore_df[which(loyaltyscore_df$Loyalty.x>=input$loyaltylevel & loyaltyscore_df$Loyalty.y>=input$loyaltylevel),])
    })
  
  output$loyaltydf <- renderDataTable({
    loyalty$Loyalty = as.numeric(loyalty$Articles*input$article +
                                   loyalty$Surveys*input$survey +
                                   loyalty$EmailOpens*input$email)
    loyalty2$Loyalty = as.numeric(loyalty2$Articles*input$article +
                                    loyalty2$Surveys*input$survey +
                                    loyalty2$EmailOpens*input$email)
    loyaltyscore_df <- merge(loyalty,loyalty2,by='UserId')
    length(names(loyaltyscore_df))
    names(loyaltyscore_df) = c('UserId','Article Previous','Surveys Previous','Email Opens Previous',
                               'Loyalty Previous','Articles Current','Surveys Current',
                               'Email Opens Current','Loyalty Current')
    loyaltyscore_df <- merge(loyaltyscore_df,emails[!duplicated(emails$UserId),c(1,4)],by='UserId')
    loyaltyscore_df <- loyaltyscore_df[order(loyaltyscore_df$'Loyalty Current',decreasing = T),]
    loyaltyscore_df[1:200,]
    
  })
  
  output$votechart <- renderPlotly({
    
    z1 = temp_votes
    z1$topsurvey1 = NA
    z1$topsurvey2 = NA
    z1$topsurvey3 = NA
    z1$topsurvey1num = NA
    z1$topsurvey2num = NA
    z1$topsurvey3num = NA
    votes$week = week(votes$ResponseDate)
    for(i in 1:nrow(z1)){
    temp = votes[which(votes$week==z1$week[i]),]
    temp[,1] <- as.character(temp[,1])
    temp = as.data.frame(table(temp[,1]))
    temp = temp[order(temp$Freq,decreasing = T),]
      z1$topsurvey1[i] = as.character(temp$Var1[1])
      z1$topsurvey2[i] = as.character(temp$Var1[2])
      z1$topsurvey3[i] = as.character(temp$Var1[3])
      z1$topsurvey1num[i] = as.character(temp$Freq[1])
      z1$topsurvey2num[i] = as.character(temp$Freq[2])
      z1$topsurvey3num[i] = as.character(temp$Freq[3])
    }
    z1$week <- lubridate::ymd( "2018-01-01" ) + lubridate::weeks( z1$week - 1 )
    z1 <- z1[which(z1$week >= input$votedate),]
     plot_ly(data=z1) %>%
      add_trace(x = ~week, y = ~N, type = 'bar', hover='text',name='Responses per week',text = ~paste('Top Surveys<br>',
      z1$topsurvey1,'-',z1$topsurvey1num,'votes<br>',z1$topsurvey2,'-',z1$topsurvey2num,'votes<br>',z1$topsurvey3,
      '-',z1$topsurvey3num,' votes'),marker = list(color = 'rgb(115,20,84)'),textfont = 
      list(color = '#000000', size =20),mode='bar') %>% layout(xaxis=list(title='Month'),yaxis=list(title='Responses'))
      
  })
  
  observeEvent({
    input$articledate
    input$timemin},{
      
      temp_article <- article[which(article$StartTime >= input$articledate & article$ActiveTime>=input$timemin),]
      
      
    output$articlereads <- renderPlotly({
      z1 <- data.frame(table(temp_article$StartTime))
      z1$toparticle1 = NA
      z1$toparticle2 = NA
      z1$toparticle3 = NA
      z1$toparticle1num = NA
      z1$toparticle2num = NA
      z1$toparticle3num = NA
      names(z1)[1] = 'Day'
      z1$Day <- as.Date(as.character(z1$Day))
      z1$Week = week(z1$Day)
      z1$Week <- lubridate::ymd( "2018-01-01" ) + lubridate::weeks( z1$Week - 1 )
      
      for(i in 1:nrow(z1)){
        temp = temp_article[which(temp_article$StartTime==z1$Day[i]),]
        temp[,6] <- as.character(temp[,6])
        temp = as.data.frame(table(temp[,6]))
        temp = temp[order(temp$Freq,decreasing = T),]
        z1$toparticle1[i] = as.character(temp$Var1[1])
        z1$toparticle2[i] = as.character(temp$Var1[2])
        z1$toparticle3[i] = as.character(temp$Var1[3])
        z1$toparticle1num[i] = as.character(temp$Freq[1])
        z1$toparticle2num[i] = as.character(temp$Freq[2])
        z1$toparticle3num[i] = as.character(temp$Freq[3])
      }
      
      
      plot_ly(data=z1) %>%
        add_trace(x = ~Day, y = ~Freq, type = 'bar', hover='text',name='Reads',text = ~paste('Top articles<br>',
                                                                                             z1$toparticle1,'-',z1$toparticle1num,'reads<br>',z1$toparticle2,'-',z1$toparticle2num,'reads<br>',z1$toparticle3,
                                                                                             '-',z1$toparticle3num,' reads'),marker = list(color = 'rgb(115,20,84)'),textfont = 
                    list(color = '#000000', size =20),mode='bar') %>% layout(xaxis=list(title='Month'),yaxis=list(title='Reads'))
    })
    
    output$article_df <- renderDataTable({
      article_df = data.frame('Article'=c(),'Reads in timeframe'=c(),
                              '1min+ Reads'=c(),'2min+ Reads'=c(),'PublishDate'=c(),check.names=F)
      for(i in unique(temp_article$Title)){
        article_df <- rbind(article_df,
                            data.frame('Article'=c(i),
                                       'Reads in timeframe'=c(nrow(temp_article[which(temp_article$Title==i),])),
                                       '1min+ Reads'=c(nrow(temp_article[which(temp_article$Title==i & temp_article$ActiveTime >= 60),])),
                                       '2min+ Reads'=c(nrow(temp_article[which(temp_article$Title==i & temp_article$ActiveTime >= 120),])),
                                       'PublishDate'=c(substr(temp_article[which(temp_article$Title==i),]$PublishDate[1],1,10)),
                                       check.names=F
                            )
        )
        
      }
      article_df <- article_df[order(article_df[,2],decreasing = T),]
      article_df[which(article_df$Article!='NULL'),]
    })
    
    output$publishdate <- renderPlotly({
      publish_temp <- article[which(article$PublishDate >= input$articledate & article$PublishDate <= Sys.Date()),]#input$articledate),]
      publish_temp <- publish_temp[which(publish_temp$Title != 'NULL'),]
      bytitle <- as.data.frame(table(publish_temp$Title))
      byday <- publish_temp[!duplicated(publish_temp$Title),]
      byday <- as.data.frame(table(byday$PublishDate))
      byday$Var1 <- as.Date(as.character(byday$Var1))
      byday$Top <- NA
      for(i in 1:nrow(byday)){
        publish_temp4 <- bytitle[which(bytitle$Var1 %in%
                                         publish_temp[which(publish_temp$PublishDate == byday$Var1[i]),]$Title),]
        byday$Top[i] = as.character(publish_temp4[which.max(publish_temp4$Freq),]$Var1)
      }
      
      plot_ly(data=byday) %>%
        add_trace(x = ~Var1, y = ~Freq, type = 'bar', hover='text',name='Responses per week',
                  text = ~paste('Top Article Published Today:',byday$Top),marker = list(color = 'rgb(115,20,84)'),textfont = 
                    list(color = '#000000', size =20),mode='bar') %>% layout(xaxis=list(title='Month'),
                                                                             yaxis=list(title='Published Articles'))
    })
  })
  

  observeEvent(input$emailsegment,{
  
    emails$count = 1
    email_temp <- aggregate(x=emails[,c('count')],by=list(emails$Subject,emails$Event),FUN=sum)
    email_temp <- merge(email_temp, aggregate(x=emails[,c('Time')],by=list(emails$Subject),FUN=min),
                        by='Group.1',all.x=T)
    email_temp$Opened = NA
    for(i in unique(email_temp$Group.1)){
      if (nrow(email_temp[which(email_temp$Group.1 == i),])==2){
        email_temp[which(email_temp$Group.1 == i & email_temp$Group.2==1),]$Opened[1] = email_temp[which(email_temp$Group.1 == i & email_temp$Group.2==3),]$x.x[1]
      }
    }
    email_temp = email_temp[which(email_temp$Group.2==1),]
    names(email_temp) = c('Subject','Type','Count','Date','Opened')
    email_temp[is.na(email_temp)]=0
    email_temp$Sent = email_temp$Count; email_temp$Count = NULL;
    email_temp$Percentage = round(email_temp$Opened*100/email_temp$Sent,1)
    email_temp <- email_temp[order(email_temp$Date,decreasing = T),]
    email_temp$Group <- 'Other'
    email_temp[which(email_temp$Subject %like% 'Reach Roundup'),]$Group = 'Reach'
    email_temp[which(email_temp$Subject %like% 'Awake58'),]$Group = 'Awake58'
    email_temp$Group <- as.character(email_temp$Group)
    email_temp$Subject <- as.character(email_temp$Subject)
    if(input$emailsegment == 'Reach Roundup'){
      data = email_temp[which(email_temp$Group == 'Reach'),]
    } else if (input$emailsegment == 'Awake58'){
      data = email_temp[which(email_temp$Group == 'Awake58'),]
    } else if (input$emailsegment == 'Other'){
      data = email_temp[which(email_temp$Group == 'Other'),]
    } else {
      data =email_temp
    }
    
  
  
  output$email_df <- renderDataTable({
     data[,c('Subject','Date','Opened','Sent','Percentage')]
  })
  
  output$email_plot <- renderPlotly({
    dat2 = data[1:10,]
    plot_ly(data=dat2) %>%
      add_trace(x = ~Subject, y = ~Percentage, type = 'bar', hover='text',name='Open Percentage',text = ~paste(dat2$Subject),marker = list(color = 'rgb(115,20,84)'),textfont = 
                  list(color = '#000000', size =20),mode='bar') %>% layout(xaxis=list(title='Subject'),yaxis=list(title='Open Percentage'))
    
  })
  
  })#emailobserveeventclose

})
