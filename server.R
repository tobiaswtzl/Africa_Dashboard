################################################################################
#BMZ Africa Dashboard
################################################################################

#checks if pacman package exists, if not install
if (!require("pacman")) install.packages("pacman")

#install/load packages
pacman::p_load("shiny",
               "dplyr",
               "here",
               "leaflet",
               "shinyWidgets",
               "plotly",
               "shinydashboard"
)

server <- function(input, output, session) {
  
  
  ################################################################################
  ###Functions
  ################################################################################
  
  #Plot aktuelle Lage
  plot_aktuelle_lage <- function(data, variable, custom_title, hover_suffix= "", text_caption) {
    
    #remove 0 values
    data[data==0] <- NA
    
    #plot
    plotly::plot_ly(data = data,
                    x = ~.data[[variable]],
                    y = ~reorder(country_de, .data[[variable]]),
                    text = ~.data[[variable]],
                    texttemplate= paste("%{x}", hover_suffix),
                    textposition = 'auto',
                    type = 'bar',
                    hoverinfo = 'y',          
                    orientation = 'h') %>% 
      
      #customise layout
      plotly::layout(
        title = list(
          text = custom_title,
          size = 20
        ),
        xaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        ),
        yaxis = list(
          title = ""
        ),
        annotations = 
          list(x = 1,
               y = 0,
               text = paste0("Quelle: ", text_caption), 
               showarrow = FALSE,
               xref='paper',
               yref='paper', 
               xanchor='right',
               yanchor='auto',
               xshift=0, 
               yshift=0
          )
      ) %>% 
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "resetScale2d", "pan2d", "zoom2d", "autoScale2d",
                                                "select2d", "lasso2d", "hoverCompareCartesian", "hoverClosestCartesian"), displaylogo = FALSE)
    
  }
  map_africa <- function(data, bins, displayed_var, labels, title, lab_form = " %") {
    
    #prepare colours
    pal <- leaflet::colorBin(
      "YlOrRd",
      domain = displayed_var,
      bins = bins
    )
    
    #create leaflet object
    leaflet::leaflet(
      data = data,
      options = leafletOptions(
        minZoom = 3.4,
        maxZoom = 7,
        dragging = TRUE
      )
    ) %>%
      
      #define view
      leaflet::setView(
        lng = 13.1021,
        lat = 0.2812,
        zoom = 3.4
      ) %>%
      
      #polygon data
      leaflet::addProviderTiles(
        "MapBox",
        options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
        )
      ) %>% 
      
      #create coloured ploygons
      leaflet::addPolygons(
        fillColor = ~pal(displayed_var),
        layerId = ~ADMIN,
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        
        #define what happens when mouse hovers over polygons
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        
        #add labels
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")      
      ) %>% 
      
      #add legend
      leaflet::addLegend(
        pal = pal,
        values = ~displayed_var,
        opacity = 0.7,
        title = title,
        position = "bottomright",
        na.label = "Keine aktuellen Daten",
        labFormat = labelFormat(suffix = lab_form)
      )
    
  }
  
  #function to prepare data for plotting
  pie_chart_data_prep <- function(data, group_by_var) {
    
    data %>%
      dplyr::group_by({{group_by_var}}) %>%
      dplyr::summarise(financial_commitment_in_mio_per_variable = sum(financial_commitment_in_mio))
  }
  
  #function to plot prepared data with plotly
  pie_chart_plot <- function(data, group_by_var) {
    
    plotly::plot_ly(data = data,
                    labels = ~.data[[group_by_var]],
                    values = ~financial_commitment_in_mio_per_variable,
                    marker = list(colors = c('#1f77b4', '#ff7f0e','#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')),
                    sort = FALSE,
                    text = ~.data[[group_by_var]],
                    textposition = "outside",
                    textinfo = 'label + value',
                    hovertemplate = "<b>EZ im Bereich %{text}: </b><br>%{value} Mio. Euro <extra></extra>"
    ) %>%
      plotly::add_pie(hole = 0.6) %>%
      plotly::layout(title = list(text = chosen_country(), xanchor = "left", x = 0.1, yanchor = "top"),
                     showlegend = FALSE, margin = mrg) %>%
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "hoverClosestPie"), displaylogo = FALSE)
    
  }
  
  #Area Chart data prep
  area_chart_data_prep <- function(data, group_by_var) {
    
    data %>%
      dplyr::group_by({{group_by_var}}, year) %>% 
      
      #sum
      dplyr::summarise(financial_commitment_in_mio_per_variable = sum(financial_commitment_in_mio)) %>% 
      
      # #pivot for plotting
      tidyr::pivot_wider(names_from = {{group_by_var}},
                         values_from = financial_commitment_in_mio_per_variable) 
  }
  
  
  #Render menu, passed to Sidebar 
  output$menu <- renderMenu({
    sidebarMenu(
      id = "tabs",
      #different menu tabs
      menuItem(" Aktuelle Lage", tabName = "aktuelle_daten", icon = icon("earth-africa")),
      menuItem(" Bilaterale EZ (BMZ)", tabName = "ez_daten", icon = icon("handshake-simple"))
    )
  })
  
  #Reset all filters
  observeEvent(input$reset_all, {
    updatePickerInput(session, "select_do",
                      selected = c(
                        "Kreditanstalt für Wiederaufbau (KFW)",
                        "Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ) GmbH",
                        "Physikalisch-Technische Bundesanstalt (PTB)",
                        "Bundesanstalt für Geowissenschaften und Rohstoffe (BGR)"
                      )
    )
    updateSliderInput(session,
                      "date_range",
                      value = c(2005, max(data_memphis$year))
    )
    updateSwitchInput(session,
                      "switch_money",
                      value = FALSE)
    updateSwitchInput(session,
                      "per_capita",
                      value = FALSE)
    
  })
  
  ################################################################################
  #Prepare Data AKtuelle Situation
  ################################################################################
  
  output$africa_map_food <- renderLeaflet({
    
    if (input$food_radio == "Ernährungssicherheit") {
      #prepare labels
      labels_food <- sprintf(
        "<strong>%s: %g &#37 der analysierten Haushalte in Phase 3-5</strong><br/>
  Phase 1 (Keine/minimal):&nbsp; %g &#37 <br/>
  Phase 2 (Strapaziert): &nbsp;&nbsp;&nbsp;&nbsp;  %g &#37<br/>
  Phase 3 (Krise):   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp; &nbsp; &nbsp; %g &#37 <br/>
  Phase 4 (Notsituation):   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  %g &#37  <br/>
  Phase 5 (Hungersnot):     &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; %g &#37 <br/>
  Einschätzung gültig für: % s  <br/>
  Anteil der analysierten Bevölkerung: %g &#37",
  map_data$country_de,
  map_data$phase35_percent,
  map_data$phase1_percent,
  map_data$phase2_percent,
  map_data$phase3_percent,
  map_data$phase4_percent,
  map_data$phase5_percent,
  map_data$period,
  map_data$analysed_population_percent
      ) %>% lapply(htmltools::HTML)
      
      
      map_africa(data = map_data,
                 bins = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70),
                 map_data$phase35_percent,
                 labels = labels_food,
                 title = "Haushalte in Phase 3-5")
      
    } else  {
      
      #prepare for leaflet
      africa_data <- leaflet::leaflet(map_data,
                                      options = leafletOptions(
                                        minZoom = 3.4,
                                        maxZoom = 7,
                                        dragging = TRUE
                                      )
      ) %>%
        leaflet::setView(lng = 13.1021 ,
                         lat = 0.2812,
                         zoom = 3.4
        ) %>%
        leaflet::addProviderTiles("MapBox",
                                  options = providerTileOptions(
                                    id = "mapbox.light",
                                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
                                  )
        )
      
      #prepare colours
      bins <- c(-5, 0, 3, 11, 25, Inf)
      pal <- leaflet::colorBin(c("#228B22",  "#ffffcc", "#fd8d3c", "#e31a1c", "#800026"),
                               domain = map_data$change_foodbasket_percent,
                               bins = bins
      )
      
      #prepare labels
      labels_food_basket <- sprintf(
        "<strong>%s: %g &#37 Kostenveränderung Food Basket</strong><br/>",
        map_data$country_de,
        map_data$change_foodbasket_percent) %>%
        lapply(htmltools::HTML)
      
      
      africa_data %>% 
        leaflet::addPolygons(
          fillColor = ~pal(change_foodbasket_percent),
          layerId = ~ADMIN,
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels_food_basket,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
        leaflet::addLegend(
          colors = c("#228B22",  "#ffffcc", "#fd8d3c", "#e31a1c", "#800026", "#71797E"),
          values = ~change_foodbasket_percent,
          opacity = 0.7,
          title = "Preissteigerung Food Basket <br/>(im Vergleich zum vorherigen Quartal)",
          position = "bottomright",
          na.label = "Keine aktuellen Daten",
          labFormat = labelFormat(suffix = " %"),
          labels = c("Negativ (<0 %)", "Normal (0-3 %)", "Moderat (3-10 %)", "Hoch (10-25 %)", "Massiv (>25 %)", "Keine Daten verfügbar")) 
      
      
      
    }
    
    
  })
  
  #render Plot aktuelle Lage
  output$plot_aktuelle_lage <- renderPlotly({
    
    if (input$food_radio == "Ernährungssicherheit") {
      
      plot_aktuelle_lage(
        data = data_boxes, 
        variable = "phase35_percent",
        custom_title = "Anteil der (analysierten) afrikanischen Bevölkerung in Phase 3-5 (Krise-Hungersnot) nach Ländern in %",
        hover_suffix = "%",
        text_caption = "Cadre Harmonisé und Integrated Food Security Phase Classification"
      )
    }
    else {
      
      if (input$food_radio == "Veränderung Food Basket Preis") {
        
        plot_aktuelle_lage(
          data = data_boxes, 
          variable = "change_foodbasket_percent",
          custom_title = "Veränderung der Food Basket Kosten nach Ländern in % ",
          hover_suffix = "%",
          text_caption = "World Food Programme")
        
      } else {
        #currently not in use because debt classifications cannot be received via api
        
        # data_boxes %>% 
        #   dplyr::mutate(risk_debt_distress = ifelse(is.na(risk_debt_distress), "Keine Daten verfügbar", risk_debt_distress)) %>% 
        #   dplyr::group_by(risk_debt_distress) %>% 
        #   
        #   #count number of countries per group
        #   count() %>% 
        #   
        #   #add row for countries with low risk in case such a ountry will exist at one point, otherwise colour order will be broken then
        #   ungroup() %>% 
        #   add_row(risk_debt_distress = "niedrig", n = 0) %>% 
        #   
        #   #create numeric variable to define custom order later - I'm sure there is a better solution, but it works..
        #   mutate(risk_debt_distress_numeric = case_when(
        #     risk_debt_distress == "Keine Daten verfügbar" ~ 0,
        #     risk_debt_distress == "niedrig" ~ 1,
        #     risk_debt_distress == "moderat" ~ 2,
        #     risk_debt_distress == "hoch" ~3,
        #     risk_debt_distress == "Staatsschuldenkrise vorhanden" ~ 4
        #   )) %>% 
        #   
        #   #sort
        #   arrange(risk_debt_distress_numeric) %>% 
        # 
        # 
        # #plot
        # plotly::plot_ly(labels = ~risk_debt_distress,
        #                 values = ~n,
        #                 sort = FALSE,
        #                 marker = list(colors = c("#708090", "#ffffcc","#fed976", "#fd8d3c", "#e31a1c")),
        #                 text = ~risk_debt_distress,
        #                 textposition = "outside",
        #                 textinfo = 'label + value',
        #                 hovertemplate = "<b>EZ im Bereich %{text}: </b><br>%{value} Mio. Euro <extra></extra>"
        # ) %>%
        #   plotly::add_pie(hole = 0.6) %>%
        #   plotly::layout(title = list(
        #     text = "Risiko einer Staatsschuldenkrise in afrikanischen Ländern",
        #     xanchor = "left",
        #     x = 0.03,
        #     yanchor = "top"
        #     ),
        #     showlegend = FALSE,
        #     margin = mrg,
        #     annotations = list(x = 1, y = 0, text = "Quelle: World Bank & IMF", 
        #            showarrow = F, xref='paper', yref='paper', 
        #                         xanchor='right', yanchor='auto', xshift=0, yshift=0)) %>%
        #   plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "hoverClosestPie"), displaylogo = FALSE)
      }
      
    }
    
  }
  )
  
  
  
  #render Glossar
  output$ipc_glossar <- renderTable(ipc_glossar, na = "", spacing = "l")
  
  #render Quellen
  output$quellen <- renderUI({
    HTML(
      "<h2>Ernährungssicherheit </h2>",
      "<h3>Organisation </h3>",
      "Cadre Harmonisé (CH) und Integrated Food Security Phase Classification (Inititative von:
        Action Against Hunger, CARE, CILSS, FAO, FEWS NET, FSC, GNC, IGAD, EC JRC, Oxfam, Save the
        Children, SICA, SADC, UNICEF, WFP)",
      "<h3>Datenerhebung </h3>",
      "Haushaltsbefragung",
      "<h3>Erläuterung Indikator </h3>",
      "siehe Glossar <br>",
      '<a href="https://www.ipcinfo.org/ipcinfo-website/ipc-overview-and-classification-system/en/" target="_blank">Quelle</a>',
      
      
      "<h2>Kostensteigerung Food Basket </h2>",
      "<h3>Organisation </h3>",
      "World Food Programme",
      "<h3>Datenerhebung </h3>",
      "",
      "<h3>Erläuterung Indikator </h3>",
      "This indicator contains the change in the cost of a food basket in relation to a previous period
        (Periods of 3 months). The change of the cost of basic food basket is calculated by comparing the
        seasonally adjusted cost of the food basket with the cost in the reference period (previous quarter
        or baseline), as percentage change. The change is considered normal when the percentage change is 
        between 0 and 3%, moderate when it is between 3 and 10%, high when it is between 10 and 25%, and
        severe above 25%. Note that the countries included here only include those monitored by WFP.<br>",
      '<a href="https://data.humdata.org/dataset/cost-of-the-food-basket" target="_blank">Quelle</a>',
      
      
      "<h2>Betroffene von Naturkatastrophen </h2>",
      "<h3>Organisation</h3>",
      "Centre for Research on the Epidemiology of Disasters (CRED) ",
      "<h3>Datenerhebung </h3>",
      "Informationen von NGOs, UN Organisationen, Versicherungen, Forschungsinstituten und Presseagenturen. ",
      "<h3>Erläuterung Indikator </h3>",
      "People requiring immediate assistance during a period of emergency, i.e.
        requiring basic survival needs such as food, water, shelter, sanitation and
        immediate medical assistance.<br>",
      
      '<a href="https://data.humdata.org/dataset/emdat-country-profiles" target="_blank">Quelle</a>',
      
      
      "<h2>Binnenvertriebene </h2>",
      "<h3>Organisation</h3>",
      "Internal Displacement Monitoring Centre",
      "<h3>Datenerhebung </h3>",
      "Informationen von (sub)nationalen Regierungen, UN Agenturen, NGOs, Medien und der Zivilgesellschaft.",
      "<h3>Erläuterung Indikator </h3>",
      "Genutzt wird die UN Defintion von Binnenflücchtlingen (UN,
        1998): Personen oder Personengruppen, die zur Flucht gezwungen oder verpflichtet wurden
        oder ihre Häuser oder üblichen Wohnsitze verlassen mussten, insbesondere infolge von oder
        zum Zwecke der Vermeidung der Auswirkungen von bewaffneten Konflikten, Situationen allgemeiner Gewalt,
        Menschenrechtsverletzungen oder natürlichen oder von Menschen verursachten Katastrophen, und die keine
        international anerkannte Staatsgrenze überquert haben",
      '<br><a href="https://www.internal-displacement.org/database/methodology" target="_blank">Quelle</a>',
      
      
      "<h2>Opfer von Gewalttaten </h2>",
      "<h3>Organisation</h3>",
      "Armed Conflict Location and Event Data Project",
      "<h3>Datenerhebung </h3>",
      "ACLED researchers systematically collect and review the latest reports from selected local,
        national and international sources, including media, vetted social media accounts, government
        and NGO reports, and partner organizations.  ACLED researchers work to triangulate reports when
        and where possible, but they do not independently verify events or gather first-hand information on the ground.
        ACLED’s local partners often verify and collect first hand information. ACLED employs a range of sourcing strategies
        to ensure the data are timely and reliable. <br>",
      "<h3>Erläuterung Indikator </h3>",
      "Summenindex aus den Opfern von Kämpfen, Explosionen und Gewalt gegen Zivilist*innen",
      '<br><a href="https://acleddata.com/download/2827/" target="_blank">Quelle</a>',
      
      
      "<h2>Proteste </h2>",
      "<h3>Organisation</h3>",
      "Armed Conflict Location and Event Data Project",
      "<h3>Datenerhebung </h3>",
      "ACLED researchers systematically collect and review the latest reports from selected local,
        national and international sources, including media, vetted social media accounts, government
        and NGO reports, and partner organizations.  ACLED researchers work to triangulate reports when
        and where possible, but they do not independently verify events or gather first-hand information on the ground.
        ACLED’s local partners often verify and collect first hand information. ACLED employs a range of sourcing strategies
        to ensure the data are timely and reliable. <br>",
      "<h3>Erläuterung Indikator </h3>",
      "Summenindex aus der Anzahl von Protesten und Aufständen.",
      '<br> <a href="https://acleddata.com/download/2827/" target="_blank">Quelle</a>'
      
    )
  }
  )
  
  #filter data_referate depending on clicked country
  data_referate_rec <- reactive({
    
    clicked_country_africa_food <- input$africa_map_food_shape_click$id
    
    if (is.null(input$africa_map_food_shape_click$id)) {
      text_referate <- data_referate%>% 
        dplyr::filter(country_en == "Africa")
      
      HTML(
        "<h3>Bitte wählen Sie ein Land aus, um die Einschätzung des zuständigen Regionalreferats zu lesen. </h3>"
      )
      #if country is chosen
    } else {
      text_referate <- data_referate %>% 
        dplyr::filter(country_en == clicked_country_africa_food) 
      
      HTML(
        paste0(
          
          paste0("<h2>Einschätzung des Ref. ", text_referate$Referat, " zum Land ", text_referate$Land,
                 "</h2>"),
          
          "<h3>Allgemein </h3>",
          
          paste(text_referate$einschaetzung),
          
          "<h3>Ernährungssicherheit </h3>",
          paste(text_referate$einschaetzung),
          
          "<h3>Politische Stabilität </h3>",
          paste(text_referate$einschaetzung),
          
          "<h3>Energie </h3>",
          
          paste0(text_referate$einschaetzung, "<br> <br>"),
          
          "ReferentIn: Max Musterfrau (0000)<br>",
          
          "Zuletzt aktualisiert am: 12. August 2022"
          
        )
      )
    }
  })
  
  #render 
  output$einschaetzung_referate <- renderUI({
    
    text_referate <- data_referate_rec() 
    
    
  })
  
  
  data_boxes_reac <- reactive({
    
    clicked_country_africa_food <- input$africa_map_food_shape_click$id
    
    
    
    if (is.null(input$africa_map_food_shape_click$id)) {
      data_boxes_reac <- data_boxes%>% 
        dplyr::filter(country_en == "Africa")
      
      #if country is chosen
    } else {
      data_boxes_reac <- data_boxes %>% 
        dplyr::filter(country_en == clicked_country_africa_food) 
    }
    
  })
  
  
  output$demoBox <- renderValueBox({
    
    data_box_acled_protest <- data_boxes_reac() 
    
    valueBox(
      paste(data_box_acled_protest$protest_riot_sum),
      HTML(
        paste0(
          "<strong>Proteste in den letzten 3 Monaten<br> pro 100.000 Einwohner</strong> <br> <br>", "Zeitraum: ",
          data_box_acled_protest$acled_oldest_date,
          " - ",
          data_box_acled_protest$acled_most_recent_date,
          "<br>Quelle: Armed Conflict Location & Event Data Project"
        )
      ),
      icon = icon("hand-fist"),
      color = "blue"
    )
  })
  
  output$violenceBox <- renderValueBox({
    
    data_box_acled_fatalities <- data_boxes_reac() 
    
    valueBox(
      paste(data_box_acled_fatalities$fatalities_sum), HTML(
        paste0(
          "<strong>Opfer Gewaltakte in den letzten 3 Monaten<br> pro 100.000 Einwohner</strong><br><br>",
          "Zeitraum: ",
          data_box_acled_fatalities$acled_oldest_date,
          " - ",
          data_box_acled_fatalities$acled_most_recent_date,
          "<br>Quelle: Armed Conflict Location & Event Data Project "
        )
      ),
      icon = icon("person-rifle"),
      color = "blue"
    )
    
    
  })
  
  output$displacementBox <- renderValueBox({
    
    data_box_int_displacement <- data_boxes_reac() 
    
    valueBox(
      data_box_int_displacement$int_displacement, 
      HTML(
        paste0(
          "<strong>Binnevertriebene in den letzten 180 Tagen<br> pro 100.000 Einwohner</strong><br><br>",
          "Zeitraum: ",
          data_box_int_displacement$int_displacement_oldest_date,
          " - ",
          data_box_int_displacement$int_displacement_most_recent_date),
        "<br>Quelle: Internal Displacement Monitoring Centre "),
      icon = icon("tents"),
      color = "blue"
    )
  })
  
  #disaster box
  output$disasterBox <- renderValueBox({
    
    data_box_disaster <- data_boxes_reac() 
    
    valueBox(
      paste(data_box_disaster$affected_sum),
      HTML(
        paste0(
          "<strong>Betroffene von Naturkatastrophen <br>pro 100.000 Einwohner</strong> <br> <br>", "Zeitraum: ",
          paste0(format(as.Date(Sys.Date(), format="%Y/%m/%d"),"%Y"), " (Aktualisierung alle drei Monate)"),
          "<br>Quelle: Centre for Research on the Epidemiology of Disasters"
        )
      ),
      icon = icon("house-tsunami"),
      color = "blue"
    )
  })
  
  output$foodbasketBox <- renderValueBox({
    
    data_box_foodbasket <- data_boxes_reac()
    
    valueBox(
      paste(data_box_foodbasket$change_foodbasket_percent, "%") , HTML(
        paste0(
          "<strong>Kostensteigerung Food Basket im Vergleich zum letzten Quartal</strong><br><br>",
          paste0("Zeitraum: aktuelles Quartal" ),
          "<br>Quelle: World Food Programme"
        )
      ),
      icon = icon("chart-line"),
      color = "blue"
    )
  }
  )
  
  output$hungerBox <- renderValueBox({
    
    data_box_hunger <- data_boxes_reac()
    
    valueBox(
      paste(data_box_hunger$phase35_percent, "%") , HTML(
        paste0(
          "<strong>der analysierten Haushalte sind in Phase 3-5</strong><br><br>",
          paste0("Zeitraum: ",data_box_hunger$period ),
          "<br>Quelle: Integrated Food Security Phase Classification"
        )
      ),
      icon = icon("wheat-awn-circle-exclamation"),
      color = "blue"
    )
  })
  
  
  ################################################################################
  #Prepare Memphis data
  ################################################################################
  
  #filtered data by DO and year
  data_filter_do_year <- reactive({
    data_memphis %>%
      
      #filter dos
      dplyr::filter(do %in% input$select_do) %>%
      
      #filter year
      dplyr::filter(
        year <= max(input$date_range) &
          year >= min(input$date_range)
      )      
  })
  
  
  #prepare data for plots
  data_plots <- reactive({
    
    #get clicked country
    clicked_country <- input$africa_map_shape_click$id
    
    #if no country is chosen
    if (is.null(input$africa_map_shape_click$id)) {
      
      data_filter_do_year() %>% 
        
        #planned or paid money
        {
          if (input$switch_money == FALSE) 
            mutate(., financial_commitment_in_mio = round(financial_commitment_planned_in_mio, 2))
          
          else 
            mutate(., financial_commitment_in_mio = round(financial_commitment_paid_in_mio, 2))
        } 
      
      #if country is chosen
    } else {
      
      data_filter_do_year() %>% 
        
        dplyr::filter(country_en == clicked_country)  %>% 
        
        #planned or paid money
        
        {
          if (input$switch_money == FALSE) 
            mutate(., financial_commitment_in_mio = round(financial_commitment_planned_in_mio, 2))
          
          else 
            mutate(., financial_commitment_in_mio = round(financial_commitment_paid_in_mio, 2))
        } 
    }
    
    
  })
  
  
  #filter data for map - planned or paid, absolut or per capita
  data_map <- reactive({
    
    data_filter_do_year() %>% 
      
      #chose financial commitment: planned or paid
      {
        if (input$switch_money == FALSE) 
          mutate(., financial_commitment_in_mio = round(financial_commitment_planned_in_mio, 2))
        
        else 
          mutate(., financial_commitment_in_mio = round(financial_commitment_paid_in_mio, 2))
      } %>% 
      
      #per capita or absolute
      {
        if (input$per_capita == TRUE)
          mutate(., financial_commitment_in_mio = round((financial_commitment_in_mio * 1000000) / population_wb, 2))
        else
          mutate(., financial_commitment_in_mio = round(financial_commitment_in_mio, 2))
        
      }
    
  })
  #get the name of the currently clicked country - no country clicked == africa
  chosen_country <- reactive({
    
    if (is.null(input$africa_map_shape_click$id)) {
      country <- "Afrika"
      
    } else {
      country <- countrycode::countrycode(input$africa_map_shape_click$id, origin = "country.name.en", destination = "country.name.de")
      
    }
  })
  
  
  ################################################################################
  #Plots
  ################################################################################
  
  ################################
  #Africa Map
  ################################
  output$africa_map <- renderLeaflet({
    
    #filter data
    data_c <- data_map() %>%
      dplyr::group_by(ISO_A3) %>% 
      dplyr::summarise(financial_commitment_in_mio_per_year = sum(financial_commitment_in_mio, na.rm = TRUE)) %>% 
      
      #add Somaliland because of missing ISO code
      tibble::add_row(ISO_A3 = "-99", financial_commitment_in_mio_per_year = 0) %>% 
      
      #NAs to 0
      dplyr::mutate(
        dplyr::across(
          everything(), 
          ~ ifelse(is.na(.x), 0, .x)
        )
      )
    
    
    #merge with spatial data
    world_map_data_africa_merged <- merge(map_data, data_c, by.x = 'ISO_A3', by.y = 'ISO_A3') 
    
    #prepare colours
    if(input$per_capita == FALSE) {
      bins_ez <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, Inf)
      
      #prepare labels
      labels_ez <- sprintf(
        "<strong>%s</strong><br/>%g Mio. EUR",
        world_map_data_africa_merged$country_de,
        world_map_data_africa_merged$financial_commitment_in_mio_per_year
      ) %>% lapply(htmltools::HTML)
      
      #define label
      lab_form = " Mio. Euro"
      
      
      #if money per capita
    } else {
      bins_ez <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf)
      
      #prepare labels
      labels_ez <- sprintf(
        "<strong>%s</strong><br/>%g EUR",
        world_map_data_africa_merged$country_de,
        world_map_data_africa_merged$financial_commitment_in_mio_per_year
      ) %>% lapply(htmltools::HTML)
      
      #define label
      lab_form = " Euro"
      
    }
    
    map_africa(data = map_data,
               bins = bins_ez,
               displayed_var = world_map_data_africa_merged$financial_commitment_in_mio_per_year,
               labels = labels_ez,
               title = "Bilaterale EZ (BMZ)",
               lab_form = lab_form)
    
    
  })
  
  # get name of clicked polygon
  observeEvent(input$africa_map_shape_click, {
    # click event
    clicked_country <- input$africa_map_shape_click$id
    
    #print name of clicked country
    print(clicked_country)
  })
  
  
  
  ################################
  #Gebuchte/Geplante Gelder
  ################################
  output$gebucht_bar <- renderPlotly({
    
    data_filter_do_year() %>% 
      
      #create variables for overview: absolute and in percent
      dplyr::mutate(planned_total = sum(financial_commitment_planned_in_mio)) %>% 
      dplyr::mutate(paid_total = sum(financial_commitment_paid_in_mio)) %>% 
      dplyr::mutate(paid_total_percent = round(100 / planned_total * paid_total, 2)) %>% 
      dplyr::mutate(planned_total_percent = round(100 - paid_total_percent, 2)) %>% 
      dplyr::mutate(y = "") %>% 
      
      #keep only top row (relevant columns are the same since mutate and not summarise is used above)
      dplyr::slice_head() %>% 
      
      #select only relevant columns
      dplyr::select(contains("total"), y) %>% 
      
      #plot
      plotly::plot_ly(x = ~planned_total_percent,
                      y = ~y,
                      type = 'bar',
                      orientation = 'h',
                      name = 'planned_total_per',
                      text = "Belegte Haushaltsmittel",
                      textposition = 'auto',
                      hovertemplate = "<b>Anteil der belegten Haushaltsmittel</b><br> %{value} % <extra></extra>",
                      marker = list(color = '#6495ED',
                                    line = list(color = '#6495ED',
                                                width = 1))) %>% 
      plotly::add_trace(x = ~paid_total_percent,
                        name = 'paid_total_percent',
                        text = "Nicht belegte Haushaltsmittel",
                        textposition = 'auto',
                        hovertemplate = "<b>Anteil der nicht belegten Haushaltsmittel</b><br> %{value} % <extra></extra>",
                        marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                      line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                  width = 1))) %>% 
      plotly::layout(barmode = 'stack',
                     xaxis = list(title = "", showgrid = FALSE, ticksuffix = "%"),
                     yaxis = list(title ="", showgrid = FALSE),
                     showlegend = FALSE) %>% 
      plotly::config(displayModeBar = FALSE)
  }
  )
  
  
  ################################
  #Kernthemen: absolut
  ################################
  
  #define margins so the pie chart fits the tabbox
  mrg <- list(l = 50, r = 50,
              b = 30, t = 62,
              pad = 20)
  
  
  output$kernthema_pie <- renderPlotly({
    
    #prep data
    data_kernthemen_pie <- pie_chart_data_prep(data_plots(), kernthema)
    
    #plot
    pie_chart_plot(data_kernthemen_pie, "kernthema")
  }
  )
  
  ################################
  #Referate: absolut
  ################################
  output$referate_pie <- renderPlotly({
    
    #prep data
    data_referate_pie <- pie_chart_data_prep(data_plots(), referat)
    
    #plot
    pie_chart_plot(data_referate_pie, "referat")
  }
  )
  
  
  ################################
  #Fem EZ: absolut
  ################################
  output$fem_ez_pie <- renderPlotly({
    
    #prep data
    data_fem_ez_pie <- pie_chart_data_prep(data_plots(), fem_ez)
    
    #plot
    pie_chart_plot(data_fem_ez_pie, "fem_ez")
  }
  )
  
  
  ################################
  #Kernthemen: Zeitverlauf
  ################################
  
  #Filled Line Chart Kernthema
  
  #create list of kernthemen to add missings later
  kernthemen_cols <-  c("Leben ohne Hunger" = NA, "Wirtschaft" = NA, "Frieden" = NA,
                        "Klima" = NA, "Umwelt" = NA, "Gesundheit" = NA, "Sonstiges" = NA)
  
  output$kernthema_area <- renderPlotly({
    
    data_plots() %>%
      area_chart_data_prep(., kernthema) %>% 
      
      #add missing cols
      tibble::add_column(!!!kernthemen_cols[!names(kernthemen_cols) %in% names(.)]) %>%
      
      #rounding, NAs to 0
      dplyr::mutate(across(everything(), ~ ifelse(is.na(.x), 0, round(.x, 2)))) %>% 
      
      
      #plot
      plotly::plot_ly(x = ~year,
                      y = ~`Leben ohne Hunger`,
                      name = 'Leben ohne Hunger',
                      type = 'scatter',
                      mode = 'none',
                      stackgroup = 'one' ,
                      hoverinfo = 'text',
                      hovertext = ~ paste0("<b>Leben ohne Hunger:</b> ", `Leben ohne Hunger`, " Mio. Euro"),
                      fillcolor = '#d62728'
      ) %>% 
      plotly::add_trace(y = ~Wirtschaft , name = 'Wirtschaft', hoverinfo = 'text', hovertext = ~ paste0("<b>Wirtschaft:</b> ", Wirtschaft, " Mio. Euro"),
                        fillcolor = '#e377c2') %>% 
      plotly::add_trace(y = ~Frieden , name = 'Frieden', hovertext = ~ paste0("<b>Frieden:</b> ", Frieden, " Mio. Euro"),
                        fillcolor = '#1f77b4 ') %>% 
      plotly::add_trace(y = ~Klima, name = 'Klima',  hovertext = ~ paste0("<b>Klima:</b> ", Klima, " Mio. Euro"),
                        fillcolor = '#2ca02c') %>% 
      plotly::add_trace(y = ~Umwelt, name = 'Umwelt', hovertext = ~ paste0("<b>Umwelt:</b> ", Umwelt, " Mio. Euro"),
                        fillcolor = '#8c564b') %>%  
      plotly::add_trace(y = ~Gesundheit, name = 'Gesundheit', hovertext = ~ paste0("<b>Gesundheit:</b> ", Gesundheit, " Mio. Euro"),
                        fillcolor = '#ff7f0e') %>% 
      plotly::add_trace(y = ~Sonstiges, name = 'Sonstiges', hovertext = ~ paste0("<b>Sonstiges:</b> ", Sonstiges, " Mio. Euro"),
                        fillcolor = '#9467bd') %>% 
      plotly::layout(title = list(text = chosen_country(), xanchor = "center", yanchor = "top"),
                     xaxis = list(title = "", showgrid = FALSE, hoverformat = "Mio Euro"),
                     yaxis = list(title = "EZ in Mio.", showgrid = FALSE, hoverinfo = 'text', hoverformat = "Mio Euro"),
                     hovermode = "x unified",
                     showlegend = FALSE
      ) %>% 
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "resetScale2d", "pan2d", "zoom2d", "autoScale2d"), displaylogo = FALSE)
    
  }
  )
  
  ################################
  #Referat: Zeitverlauf
  ################################
  
  #add list of referate to add missings later
  referat_cols <-  c("Ref. 201" = NA, "Ref. 202" = NA, "Ref. 203" = NA,
                     "Ref. 211" = NA, "Ref. 212" = NA, "Ref. 213" = NA, "Ref. 300" = NA)
  
  output$referat_area <- renderPlotly({
    
    data_plots() %>%
      area_chart_data_prep(referat) %>% 
      
      #fill empty columns
      tibble::add_column(!!!referat_cols[!names(referat_cols) %in% names(.)]) %>% 
      
      #rounding, NAs to 0
      dplyr::mutate(across(everything(), ~ ifelse(is.na(.x), 0, round(.x, 2)))) %>% 
      
      #plot
      plotly::plot_ly(x = ~year, y = ~`Ref. 300`, name = 'Ref. 300',
                      type = 'scatter', mode = 'none', stackgroup = 'one' , hoverinfo = 'text',
                      hovertext = ~ paste0("<b>Ref. 300:</b> ", `Ref. 300`, " Mio. Euro"),
                      fillcolor = '#d62728'
      ) %>% 
      plotly::add_trace(y = ~`Ref. 201` , name = 'Ref. 201', hoverinfo = 'text', hovertext = ~ paste0("<b>Ref. 201:</b> ", `Ref. 201`, " Mio. Euro"),
                        fillcolor = '#e377c2') %>% 
      plotly::add_trace(y = ~`Ref. 202` , name = 'Ref. 202', hovertext = ~ paste0("<b>Ref. 202:</b> ", `Ref. 202`, " Mio. Euro"),
                        fillcolor = '#1f77b4 ') %>% 
      plotly::add_trace(y = ~`Ref. 203`, name = 'Ref. 203',  hovertext = ~ paste0("<b>Ref. 203:</b> ", `Ref. 203`, " Mio. Euro"),
                        fillcolor = '#2ca02c') %>% 
      plotly::add_trace(y = ~`Ref. 211`, name = 'Ref. 211', hovertext = ~ paste0("<b>Ref. 211:</b> ", `Ref. 211`, " Mio. Euro"),
                        fillcolor = '#8c564b') %>%  
      plotly::add_trace(y = ~`Ref. 212`, name = 'Ref. 212', hovertext = ~ paste0("<b>Ref. 212:</b> ", `Ref. 212`, " Mio. Euro"),
                        fillcolor = '#ff7f0e') %>% 
      plotly::add_trace(y = ~`Ref. 213`, name = 'Ref. 213', hovertext = ~ paste0("<b>Ref. 213:</b> ", `Ref. 213`, " Mio. Euro"),
                        fillcolor = '#9467bd') %>% 
      plotly::layout(title = list(text = chosen_country(), xanchor = "center", yanchor = "top"),
                     xaxis = list(title = "", showgrid = FALSE, hoverformat = "Mio Euro"),
                     yaxis = list(title = "EZ in Mio.", showgrid = FALSE, hoverinfo = 'text', hoverformat = "Mio Euro"),
                     hovermode = "x unified",
                     showlegend = FALSE
      ) %>% 
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "resetScale2d", "pan2d", "zoom2d", "autoScale2d"), displaylogo = FALSE)
    
  }
  )
  
  ################################
  #Fem EZ: Zeitverlauf
  ################################
  
  #add list of referate to add missings later
  fem_ez_cols <-  c("Keine Genderkomponente" = NA, "Genderkomponente" = NA, "Gender Hauptkomponente" = NA,
                    "nicht verfügbar" = NA)
  
  output$fem_ez_area <- renderPlotly({
    
    data_plots() %>%
      area_chart_data_prep(fem_ez) %>% 
      
      #fill empty columns
      tibble::add_column(!!!fem_ez_cols[!names(fem_ez_cols) %in% names(.)]) %>% 
      
      #rounding, NAs to 0
      dplyr::mutate(across(everything(), ~ ifelse(is.na(.x), 0, round(.x, 2)))) %>% 
      
      
      #plot
      plotly::plot_ly(x = ~year, y = ~`Keine Genderkomponente`, name = 'Keine Genderkomponente',
                      type = 'scatter', mode = 'none', stackgroup = 'one' , hoverinfo = 'text',
                      hovertext = ~ paste0("<b>Keine Genderkomponente:</b> ", `Keine Genderkomponente`, " Mio. Euro"),
                      fillcolor = '#506C64'
      ) %>% 
      plotly::add_trace(y = ~`Genderkomponente` , name = 'Genderkomponente', hoverinfo = 'text', hovertext = ~ paste0("<b>Genderkomponente:</b> ", `Genderkomponente`, " Mio. Euro"),
                        fillcolor = '#77CBB9') %>% 
      plotly::add_trace(y = ~`Gender Hauptkomponente` , name = 'Gender Hauptkomponente', hovertext = ~ paste0("<b>Gender Hauptkomponente:</b> ", `Gender Hauptkomponente`, " Mio. Euro"),
                        fillcolor = '#75B8C8 ') %>% 
      plotly::layout(title = list(text = chosen_country(), xanchor = "center", yanchor = "top"),
                     xaxis = list(title = "", showgrid = FALSE, hoverformat = "Mio Euro"),
                     yaxis = list(title = "EZ in Mio.", showgrid = FALSE, hoverinfo = 'text', hoverformat = "Mio Euro"),
                     hovermode = "x unified",
                     showlegend = FALSE
      ) %>% 
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "resetScale2d", "pan2d", "zoom2d", "autoScale2d"), displaylogo = FALSE)
    
  }
  )
  
  
}
