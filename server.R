#For weekly DNet data
#Drug Trends Programme - DNeT/NIDIP team
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(shiny)

library(dplyr) #for Gantt chart-needed for case_when()
library(forcats) #for area plot-need to reverse factor order

library(ggplot2)
#https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
library(scales) #for date scale

#reading RDS file format is apparently more efficient
#https://appsilon.com/fast-data-loading-from-files-to-r/
weekly <- readRDS("Weekly2.rds")
allmkt <- readRDS("AllMkt.rds")
alldrg <- readRDS("AllDrg.rds")
DrgAZ <- readRDS("DrgAZ.rds") #Alphabetical order for drugs
DrgNL <- readRDS("DrgNL.rds") #Total number of listings order for drugs
MktAZ <- readRDS("MktAZ.rds") #Alphabetical order for markets
MktNL <- readRDS("MktNL.rds") #Total number of listings order for markets
MktST <- readRDS("MktST.rds") #Chronological order for markets
Mktalph <- readRDS("Mktalph.rds")

# https://stackoverflow.com/questions/30443625/how-do-i-build-a-reactive-dataframe-in-r-shiny
rd <- weekly
makeReactiveBinding("rd")

Drgalph <- c(0.2,0.2,seq(1,1,length=length(DrgNL)+1))
names(Drgalph) <- c(is.na,  " Total",  "All",  DrgNL)
# Mktalph <- c(0.2,0.2,seq(1,1,length=length(MktAZ)+1))
# names(Mktalph) <- c(is.na,  " Total",  "All",  MktAz)

gg_rainbow <- function(m) {
  if (m<=6) {
    gco <- rainbow(n=m,s=1,v=0.95)
  }
  else if (m<=18) {
    val <- seq(1,.4,length=3)
    gco <- cbind(rainbow(n=6,s=1,v=val[1]),rainbow(n=6,s=1,v=val[2]),rainbow(n=6,s=1,v=val[3]))
    gco <- gco[1:m]
  }
  else if (m<=24) {
    gco <- rainbow(n=6,s=1,v=1)
    sat <- c(.4, 1,.25)
    val <- c( 1,.5,.65)
    for(i in 1:3) {
      gco <- cbind(gco,rainbow(n=6,s=sat[i],v=val[i]))
    }
    gco <- gco[1:m]
  }
  else {
    M <- ceiling(m/6)
    gco <- rainbow(n=M,s=1,v=1)
    sat <- seq(0.2,1,length=5)
    val <- seq(1,0.4,length=5)
    for(i in 1:5) {
      gco <- cbind(gco,rainbow(n=M,s=sat[i],v=val[i]))
    }
    gco <- gco[1:m]
  }
   return(gco)
}

enableBookmarking("url")

server <- function(input, output, session) {

# Allow direct linking to specific tabs (with default configs)
  observe({
  # Trigger this observer every time an input changes
    reactiveValuesToList(input)
#Shorten URL - https://shiny.rstudio.com/reference/shiny/latest/setBookmarkExclude.html
    setBookmarkExclude(c(".clientValue-default-plotlyCrosstalkOpts","plotly_relayout-A",
      "plotly_afterplot-A","plotly_hover-A","dimension","AMarket","ADrug"))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

#https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click
  observeEvent( eventExpr=input$ADrug, ignoreInit=T, handlerExpr={
      if (input$ADrug==TRUE) {
        updateCheckboxGroupInput(session,"BDrug",NULL,
          choices=eval(parse(text=paste0("Drg",input$Sort))), selected=DrgNL)
      }
      else if (input$ADrug==FALSE) {
        updateCheckboxGroupInput(session,"BDrug",NULL,
          choices=eval(parse(text=paste0("Drg",input$Sort))),selected="")
      }
  })

  observeEvent( eventExpr=input$AMarket, ignoreInit=T, handlerExpr={
    df <- as.data.frame(table(rdf()[paste0("Market",input$Sort)]))
    df <- select(subset(df,Freq>1), Var1) # Single data points look weird so subset Freq>1 rather than Freq>0
    m <- df$Var1

    if (input$AMarket==TRUE) {
      updateCheckboxGroupInput(session,"BMarket",NULL,choices=m,selected=m)
    }
    else if (input$AMarket==FALSE) {
      updateCheckboxGroupInput(session,"BMarket",NULL,choices=m)
    }
  })

  observeEvent( eventExpr=input$Sort, ignoreInit=TRUE , handlerExpr={
    if (input$Drop=="Drug") {
      DrgSel=input$DDrug
      updateSelectInput(session,"DDrug",NULL,
        choices=c("All drugs"="All",eval(parse(text=paste0("Drg",input$Sort)))),
        selected=DrgSel)
    }
    else if (input$Drop=="Market") {
      DrgSel=input$BDrug
      updateCheckboxGroupInput(session,"BDrug",NULL,
        choices=eval(parse(text=paste0("Drg",input$Sort))), selected=DrgSel)
    }
  })

#https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
  rdf <- eventReactive(eventExpr=c(input$Drop, input$BDrug, input$DMarket, input$DDrug, input$date), {
    if (input$Drop != "Gant") {
      if (input$Drop=="Market") {
        Bvar <- paste0("Drug",input$Sort)
        Sel <- input$BDrug
        Dsel <- input$DMarket
      }
      else if (input$Drop != "Market") {
        Bvar <- paste0("Market",input$Sort)
        Sel <- input$DDrug
        Dsel <- input$DDrug
      }
      if (Dsel=="All" ) { # & input$Yax=="listing" can edit when number of vendors is updated ######
        if (input$Drop=="Market") {
          rd <- subset(allmkt, subset=DrugAZ %in% Sel &
            (Week >= input$date[[1]] & Week <= input$date[[2]]) ) %>%
            mutate(
              listing=listing_lin) %>%
            arrange( eval(parse(text=paste0("Drug",input$Sort))) ) # needed for area plot
        }
        else if (input$Drop=="Drug") {
          rd <- subset(alldrg, subset=(Week >= input$date[[1]] & Week <= input$date[[2]]) ) %>%
            select(-Start,-End,-Length,-nWeek,-Listing,-nDrug,-status)
        }
      }
      else if (Dsel != "All") {
        rd <- subset(weekly,subset=DrugAZ %in% Sel &
          (Week >= input$date[[1]] & Week <= input$date[[2]]) ) %>%
          select(-Start,-End,-Length,-nWeek,-Listing,-status) %>%
          mutate(
          ) %>%
          arrange( eval(parse(text=Bvar)) )
      }
    }
  })

  observeEvent( eventExpr={
    rdf()
    input$Sort
    }, handlerExpr={
    if (input$Drop!="Gant") {
      df <- rdf()
      if (input$Drop=="Market" & input$DMarket=="All") {
        df <-  subset(weekly, subset=
            (Week >= input$date[[1]] & Week <= input$date[[2]] & DrugAZ %in% input$BDrug) )
      }
      if (nrow(df)>0) {
        df <- as.data.frame(table(df[paste0("Market",input$Sort)]))
        df <- subset(df,Freq>1) # Single data points look weird so subset Freq>1 rather than Freq>0
        m <- df$Var1

        if (input$DMarket=="All") {
          msel <- "All"
        }
        else {
          msel <- subset(df,Var1==input$DMarket)
          if (is.null(msel) | nrow(msel)==0) {
            msel <- subset(df,Freq==max(Freq))
          }
          msel <- msel$Var1
        }
        updateSelectInput(session,"DMarket", label=NULL,
          choices=c("All markets"="All",as.character(m)),
          selected=msel
        )

        msel <- subset(df,Var1 %in% input$BMarket)
        if (is.null(msel) | nrow(msel)==0) {
          msel <- df[order(df$Freq,decreasing=T),]
          msel <- msel[1:min(nrow(msel),6),]
        }
        msel <- msel$Var1
        updateCheckboxGroupInput(session,"BMarket", label=NULL,
          choices=m,
          selected=msel
        )
      }
    }
  })

# Visualisation tab

# Gantt chart ----------------------------------------------------------
  output$GantPlot <- renderPlotly({
# https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny - for scaling
    pax <- difftime(input$date[[2]],input$date[[1]],units="weeks")/(input$dimension/50)
    if (length(as.numeric(pax))==0) { pax=2 }
    xax <- ceiling(pax)
      if (input$DDrug=="All") {
        df <- subset(alldrg, is.na(listing)==F & Week <= input$date[[2]] & Week >= input$date[[1]]) %>%
          mutate(
            Market=factor(Market,MktST)
          )
      }
      else {
        df <- subset(weekly, is.na(listing)==F &
            (Week <= input$date[[2]] & Week >= input$date[[1]]) )
        if (input$Detail==T) {
          df <- group_by(df, Week,Market) %>%
            mutate(
              pct=listing / sum(listing)*100
            )
        }
        df <- ungroup(df) %>%
          subset(DrugAZ==input$DDrug) %>%
          mutate(
            Market=factor(Market,MktST)
          )
      }
      m <- as.data.frame(table(df$Market))
      m <- subset(m,Freq>0)
      m <- m$Var1
      N <- ceiling(length(m)/3)
    df <- group_by(df,Market) %>%
      mutate(
        start= as.Date(ifelse(input$date[[1]]>Start,input$date[[1]],Start),origin="1970-01-01"),
        end= as.Date(ifelse(input$date[[2]]<End,input$date[[2]],End),origin="1970-01-01"),
        pos= case_when(
          as.POSIXct(start)-as.POSIXct(input$date[[1]]) > as.POSIXct(input$date[[2]])-as.POSIXct(end) ~
            start-(nchar(as.character(Market))*.9+6)*as.numeric(pax),
          TRUE ~ end+(nchar(as.character(Market))*.9+6)*as.numeric(pax) )
      ) %>%
      ungroup()

    Title <- input$DDrug

# https://rdrr.io/r/base/Log.html - log10 and log2 might be more efficient
    minL <- min(df$listing_log10)
    widL <- 5/(max(df$listing_log10)-minL)
    ht <- 15*length(m)+350

    p <- ggplot(df) + aes(y=factor(Market), yend=factor(Market)) +
      geom_text(aes(label=Market,x=pos))

    if (input$DDrug=="All" | input$Detail==F | input$Heat==F) {
      val <- seq(0.95,0.5,length=3)
      gco <- rbind(rainbow(n=N,s=1,v=val[1]),rainbow(n=N,s=1,v=val[2]),rainbow(n=N,s=1,v=val[3]))
      gco <- as.vector(gco)
        gco <- gco[1:length(m)]
        names(gco) <- m
      p <- p + aes(colour=Market) +
        geom_segment( size=(df$listing_log10-minL)*widL+0.2) +
        scale_colour_manual(values=gco)
    }
    else {
      p <- p +
        geom_segment(size=(df$listing_log10-minL)*widL+0.2, aes(colour=pct))
      if (input$Colour=="rainbow") {
        p <- p + scale_colour_gradientn(colours=rainbow(6))
      }
      else if (input$Colour=="3-colour") {
        p <- p + scale_colour_gradient2(midpoint=mean(df$pct),low="red",mid="yellow",high="blue")
      }
    }

    if (input$Detail==T) {
      if (input$DDrug=="All" | input$Heat==F) {
        p <- p + aes(x=Week, xend=Week+6.99, text=paste0(
            "Market (status): ",Market," (",status,")",
            "<br>Start date: ",Start,
            "<br>End date: ",End,
            "<br>Duration of market in weeks: ",Length,
            "<br>Number of weeks with data available: ",nWeek,
            "<br>Mean number of listings per week (with data available): ",Listing,"<br>",
            "<br>Date: ",Week,
            "<br>Number of listings on date: ",listing
          ))
      }
      else {
        p <- p + aes(x=Week, xend=Week+6.99, text=paste0(
            "Market (status): ",Market," (",status,")",
            "<br>Start date: ",Start,
            "<br>End date: ",End,
            "<br>Duration of market in weeks: ",Length,
            "<br>Number of weeks with data available: ",nWeek,
            "<br>Mean number of listings per week (with data available): ",Listing,"<br>",
            "<br>Date: ",Week,
            "<br>Number of listings on date: ",listing,
            "<br>Percentage of listings on date: ",round(pct,1),"%"
          )) + labs(colour="% listings")
      }

      icons <- list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian","hoverCompareCartesian","toggleSpikelines")
    }
    else {
        p <- p + aes(x=start, xend=end, text=paste0(
            "Market (status): ",Market," (",status,")",
            "<br>Start date: ", Start,
            "<br>End date: ", End,
            "<br>Duration of market in weeks: ", Length,
            "<br>Number of weeks with data available: ", nWeek,
            "<br>Mean number of listings per week (with data available): ",Listing
          ))

      icons <- list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines")
    }
    p <- p + labs(y="Market name", x="Date", title=Title) +
      scale_x_date(breaks=seq(input$date[[1]], input$date[[2]], xax),
        labels=date_format("%d/%m/%Y")) +
      theme_light() + theme(legend.title=element_blank(),
                            panel.grid.minor.x=element_blank(),
                            panel.grid.minor.y=element_blank(),
                            panel.grid.major.y=element_blank(),
                            axis.text.x=element_text(angle=90),
                            axis.text.y=element_blank())

    ggplotly(p, tooltip="text", height=ht ) %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-online-drug-market">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.02, yanchor="top",
        showarrow=F, font=list(size=10, color="grey")
      ) %>%
#Based on: https://plotly-r.com/embedding-images.html
      layout(
        images=list(
          source="DrugTrends-Logo.png",
          xref="paper", yref="paper",
          x=0.01, xanchor="left",
          y=.99, yanchor="top",
          sizex=0.07, sizey=0.15)
      )  %>%
      layout(showlegend=FALSE, margin=list(b=100)) %>%
# https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js - list of button names
      config(displaylogo=F, modeBarButtonsToRemove=icons)
  })

# Main plot
  output$DNetPlot <- renderPlotly({
# This if statement is needed to stop the warnings
    if (input$Drop != "Gant") {
      df <- rdf()
      xax <- ceiling(difftime(input$date[[2]],input$date[[1]],units="weeks")/(input$dimension/50))
    if (length(as.numeric(xax))==0) { xax=2 }
      if (input$Drop=="Market") {
        Bvar <- paste0("Drug",input$Sort)
        Dsel <- input$DMarket
        Msel <- input$DMarket
        Bsel <- input$BDrug
        Balp <- Drgalph
        plot <- input$plot
        if (input$DMarket=="All") {
          validate(need(nrow(df) > 0, "No data selected or data unavailable"))
        }
      }
      else if (input$Drop=="Drug") {
        Bvar <- paste0("Market",input$Sort)
        Dsel <- input$DDrug
        Msel <- input$BMarket
        Bsel <- input$BMarket
        Balp <- Mktalph
        plot <- "line" #input$plot
        if (input$DDrug=="All") {
            df <- subset(df, subset=Market %in% Bsel)
          validate(need(nrow(df) > 0, "No data selected or data unavailable"))
        }
      }
      if (Dsel!="All") {
        df <- subset(df, subset=Market %in% Msel) %>%
          arrange( eval(parse(text=Bvar)) )
      }
      if (nrow(df) > 0) {
        if (plot=="area") {
          df <- group_by(df,Week) %>%
            mutate(
              pct=round( listing_lin / #eval(parse( text=paste0(input$Yax,"_lin") )) /
                  sum(listing_lin)*100,1 ), #eval(parse(text= paste0(input$Yax,"_lin") ))
              R2=fct_rev( eval(parse(text=Bvar)) )
            )
          p <- ggplot(df) + aes(text=paste0(
              "Date: ",Week,
              "<br>Drug: ",DrugAZ,
              "<br>Market: ",Market,
              "<br>Number of listings: ",round(listing,0),#", input$Yax,"s: ", round( eval(parse(text=input$Yax)),0),
              "<br>Number of listings (interpolated): ",round(listing_lin,1), #", input$Yax,"s (interpolated): ", round( eval(parse(text= paste0(input$Yax,"_lin"))),1),
              "<br>Percentage of listings: ",pct,"%" )) + #input$Yax,"s: ", pct )) +
            geom_area(aes(fill=R2, y=pct, group=1)) +
            scale_fill_manual(values=rev(gg_rainbow(length(Bsel))) )
        }
        else {
          if (input$Tot==TRUE & is.null(df)==FALSE) { # & input$Yax=="listing"
            df_Tot <- subset(df,is.na(listing_lin)== F) %>%
              group_by(Week) %>%
              mutate(
                listing=NA,
                listing_lin=round(sum(listing_lin),1)
              )
            df_Tot[,Bvar] <- " Total"
            df_Tot[,paste0(input$Drop,input$Sort)] <- Dsel
            df_Tot <- df_Tot[,c("Week",paste0(input$Drop,input$Sort),Bvar,"listing","listing_lin")]
            df_Tot <- ungroup(df_Tot) %>%
              distinct()
            df <- df[,c("Week",paste0(input$Drop,input$Sort),Bvar,"listing","listing_lin")]
            df <- mutate(df,
                # vendor_lin=NA,
                listing_lin=NA
              )
            df <- rbind(df,df_Tot)
          }
    #to solve issue with a series becoming dotted line when all data points are present
          df <- group_by(df,eval(parse(text=Bvar))) %>%
            mutate(
              # vendor_chk=sum(is.na(vendor)),
              # vendor_chk=ifelse(Week==min(Week),vendor_chk,1),
              listing_chk=sum(is.na(listing)),
              listing_chk=ifelse(Week==min(Week),listing_chk,1)
            ) %>%
            ungroup()
    #https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/aes_string
          p <- ggplot(df) + aes_string(color=Bvar)# still run with same warning
          if (input$Tot==TRUE) { # & input$Yax=="listing"
            Bcol <- c("#666666", gg_rainbow(length(Bsel)))
            names(Bcol) <- c(" Total", Bsel)
# Triggers Warning: Removed XXX rows containing missing values (position_stack). (because of subsetting BUT still works ok)
            p <- p + geom_area(aes( y=listing_lin, #eval(parse(text =paste0(input$Yax,"_lin"))),
# Triggers Warning: Ignoring unknown aesthetics: text (because geom_area doesn't know how to evaluate text but need to do this for subsetting - STILL WORKS OKAY)
              text=paste0(
                "Date: ", Week,
                "<br>",substring(Bvar,1,nchar(Bvar)-2),": ", eval(parse(text=Bvar)),
                "<br>Number of listings (interpolated): ", listing_lin #input$Yax,"s (interpolated): ", eval(parse(text =paste0(input$Yax,"_lin")))
              ) )) +
              aes_string(alpha=Bvar) + scale_alpha_manual(values=Balp)
          }
          else {
            Bcol <- c(gg_rainbow(length(Bsel)))
            names(Bcol) <- c(Bsel)
          }
          p <- p + scale_colour_manual(values=Bcol)
        }
        p <- p + aes(x=Week) +
          labs(title=paste0(input$Drop,": ",Dsel))
        if (plot=="area") {
          p <- p + labs(y="Percentage of listings") #paste0("Percentage of ",input$Yax,"s"))
        }
        else {
# https://stackoverflow.com/questions/35815845/r-ggplot-change-linetype-within-a-series
          p <- p + geom_line(linetype="solid") +
            geom_point(size=1,
# Triggers Warning: Ignoring unknown aesthetics: text (because geom_point doesn't know how to evaluate text but need to do this for subsetting - STILL WORKS OKAY)
              aes(text=paste0(
                "Date: ", Week,
                "<br>",substring(Bvar,1,nchar(Bvar)-2),": ", eval(parse(text=Bvar)),
                "<br>Number of listings: ", listing ))) + #input$Yax,"s: ", eval(parse(text =input$Yax)) ))) +
            geom_line(linetype="dotted",
              data=subset( df, is.na(listing)==FALSE & #eval(parse(text=input$Yax))
                  listing_chk>0 )) + # eval(parse(text=paste0(input$Yax,"_chk")))
              scale_y_continuous(limits=c(0, max(df$listing, 1000))) + #max(df[,input$Yax], 1000))) +
              # aes_string(y=input$Yax , group=1) +
              aes(y=listing, group=1) +
              labs(y="Number of listings")#paste0("Number of ", input$Yax,"s"))
        }
      }
      validate(need(nrow(df) > 0, "No data selected or data unavailable"))

      p <- p + labs(x="Date") +
        scale_x_date(breaks=seq(input$date[[1]], input$date[[2]], xax),labels=date_format("%d/%m/%Y")) +
        theme_light() + theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          legend.title=element_blank(),
          axis.text.x=element_text(angle=90))

      ggplotly(p, tooltip="text", height=800 ) %>%
        add_annotations(
          text='Source: <a href="https://ndarc.med.unsw.edu.au/resource/trends-online-drug-market">DrugTrends</a>, NDARC',
          xref="paper", yref="paper",
          x=0, xanchor="left",
          y=1.02, yanchor="top",
          showarrow=F, font=list(size=10, color="grey")
        ) %>%
#Based on: https://plotly-r.com/embedding-images.html
        layout(
          images=list(
            source="DrugTrends-Logo.png",
            x=0.01, xanchor="left", y=.99, yanchor="top",
            sizex=0.07, sizey=0.15,
            xref="paper", yref="paper",
            xanchor="left", yanchor="bottom"
          ))  %>%
        add_annotations(
          text="Drug", xref="paper", yref="paper",
          x=1.02, xanchor="left",
          y=1, yanchor="bottom",
          legendtitle=TRUE, showarrow=FALSE
        ) %>%
        layout(legend=list(y=0.95, yanchor="top"), margin=list(b=80)) %>%
        config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d", "lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
    }
  })
}
