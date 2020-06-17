library(shiny)
library(data.table)
library(tidyverse)
library(ggthemes)
library(MetricsWeighted)
library(rapport)
library(plotly)
library(stargazer)
library(gtools)
library(DT)
library(kableExtra)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(feather)
library(rstatix)

datalist_cvar <- fread("datalist_cvar.csv")

datalist_bal <- fread("datalist_bal.csv")

datalist_mvar <- fread("datalist_mvar.csv")

datalist_isv <- fread("datalist_isv.csv")

datalist_cfv <- fread("datalist_cfv.csv")

datalist_tax <- fread("datalist_tax.csv")

datalist_full <- fread("datalist_full.csv")

naics_def <- fread("naics_list.csv")

# 
# nu_compustat <-fread("C:/Users/Jackson_Mejia/Documents/ERM_Practice/10k_compustat_2009_2019.csv")
# none <- "None"
# nu_compustat[, none] <- NA
# nu_comp_clean <-
#   subset(
#     nu_compustat,
#     xrd_norm < 1 &
#       sale > 0 &
#       abs(Effective_Rate) < 200 ,
#     select = c(
#       "logsale",
#       "logsale.percentile",
#       "tic",
#       "sale",
#       "fyear",
#       "NAICS",
#       "sale",
#       "xrd_shock",
#       "Effective_Rate",
#       "Foreign_Rate",
#       "None"
#     )
#   )
# 
# nu_comp_clean$tic <- as.factor(nu_comp_clean$tic)
# 
# q_sale <-
#   nu_comp_clean %>% group_by(fyear) %>% summarise("nu_comp_clean_q_sale" = quantile(sale, 0.9, na.rm = T)) %>% ungroup()
# 
# 
# nu_comp_clean_q_sale <-
#   subset(nu_comp_clean,
#          fyear == 2012 & sale > q_sale$nu_comp_clean_q_sale[4])
# 
# q_sale_tic <- nu_comp_clean_q_sale$tic
# 
# nu_comp_clean_q_sale <-
#   nu_comp_clean[nu_comp_clean$tic %in% q_sale_tic, ]
# 
# nu_comp_clean_q_sale$quant_sale <-
#   quantcut(nu_comp_clean_q_sale$logsale, q = 10, na.rm = T) #factorizes into quantiles for effective rates
# 
# wgt_vec <-as.vector(matrix(1/nrow(nu_comp_clean_q_sale),nrow=nrow(nu_comp_clean_q_sale)))
# 
# nu_comp_clean_q_sale <-
#   mutate(
#     nu_comp_clean_q_sale,
#     filt = as.numeric(nu_comp_clean_q_sale$NAICS) * as.numeric(nu_comp_clean_q_sale$quant_sale),
#     naics = NAICS,
#     equal_weight = wgt_vec
#   )

nu_comp_clean_q_sale <- read_feather("C:\\Users\\Jackson_Mejia\\Documents\\ERM_Practice\\mcgrattan_app\\compustat_speedway_parsed.feather")

nu_comp_clean_q_sale <- as.data.frame(nu_comp_clean_q_sale)

nu_comp_clean_q_sale <- subset(nu_comp_clean_q_sale, !is.na(NAICS) & fyear >= 2010 & logsale > 10)


yr <- c(2010, 2011, 2012)
# 
# nu_comp_clean_q_sale <- mutate(nu_comp_clean_q_sale, logsale = logsale)
# 
# nu_comp_clean_q_sale <- nu_comp_clean_q_sale[, !duplicated(colnames(nu_comp_clean_q_sale), fromLast = TRUE)] 
# 
# nu_comp_clean_q_sale[, -which(names(nu_comp_clean_q_sale) == "logsale")]  #only works on a single column

#n
#Theme Functions
my_econ_white <- theme_economist_white() +
  theme(axis.title.y = element_text(margin = margin(
    t = 0,
    r = 10,
    b = 0,
    l = 0,
    unit = "pt"
  )))


my_econ <- theme_economist() +
  theme(axis.title.y = element_text(margin = margin(
    t = 0,
    r = 10,
    b = 0,
    l = 0,
    unit = "pt"
  )))



# Define UI for app that draws a histogram ----
ui <-
  navbarPage(
    title = div(
      img(
        src = 'hhei.jpeg',
        style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
        height = 60
      )
    ),
    theme = "materia",
    footer = div(img(
      src = "hhei_full.jpeg",
      height = "10%",
      width = "10%"
    ), style = "text-align: center;"),
    windowTitle = "C-Corp Explorer",
    
    
    
    # App title ----
    
    
    tabPanel("Intro", 
             includeMarkdown("intro.md"),
    br(),
    hr()),
    
    tabPanel("Data Setup",
             tabsetPanel(tabPanel("Select Variables",
               fluidRow(column(width = 8, offset = 2,
                               h1("Select Variables"),
                               h4("Instructions"),
                               p("Use this tab to select variables. Note that several variables are required for analysis and are automatically included. These are fyear, tic, sale, and NAICS. While it is not required that logsale or xrd or Effective_Rate be included, they are included by default."),
                               hr())),
               fluidRow(column(width = 4, offset = 2,
                                     h3("Data Variables"),
                               selectizeInput(
                                 inputId ='var_select', label = "Choose which variables you want for analysis:", 
                                 choices = c(names(nu_comp_clean_q_sale[, !names(nu_comp_clean_q_sale) %in% c("tic", "NAICS", "fyear")])), multiple = TRUE, options = list(placeholder = 'Select variables'), selected = c("Effective_Rate", "xrd", "sale")
                               ),
                               
                               # pickerInput(
                               #         inputId = "var_select",
                               #         label = "Choose which variables you want for analysis:", 
                               #         choices = names(nu_comp_clean_q_sale[, !names(nu_comp_clean_q_sale) %in% c("tic", "NAICS", "fyear", "sale")]),
                               #         selected = NULL,
                               #         options = list(
                               #           `actions-box` = TRUE), 
                               #         multiple = TRUE
                               #       ),
                               br()
                               ),
               column(width = 3,
                      h3("Selected Variables"),
                      tableOutput("var_tab")
                      ),
               column(width = 12,
                      br(),hr()))),
              
               tabPanel("Filter and Subset",fluidRow(column(9, offset = 2, 
                                                            h1("Filter and Subset"),
                                                            h4("Instructions"),
                                                            p("This tab allows the user to subset and filter the data in a number of ways. In particular, the user can choose whether to subset by NAICS, by size (or some other numeric variable), by year, or whether to cut outliers."),
                                                            br(),
                                                            hr(),
                                                            h4("Choose if you want to subset by NAICS code, and if so, which industries to analyze. Default is all."),
                                                            pickerInput(
                                                              inputId = "naics_select",
                                                              label = "NAICS code:", 
                                                              choices = sort(unique(nu_comp_clean_q_sale$NAICS)),
                                                              selected = unique(nu_comp_clean_q_sale$NAICS),
                                                              options = list(
                                                                `actions-box` = TRUE), 
                                                              multiple = TRUE
                                                            ),
                                                            hr(),
                                                            h4("Choose which years should be in the analysis. Default is all."),
                                                            pickerInput(
                                                              inputId = "year_select",
                                                              label = "Year(s):", 
                                                              choices = sort(unique(nu_comp_clean_q_sale$fyear)),
                                                              selected = unique(nu_comp_clean_q_sale$fyear),
                                                              options = list(
                                                                `actions-box` = TRUE), 
                                                              multiple = TRUE
                                                            ),
                                                            hr(),
                                                            h4("Choose whether to subset by a particular numeric variable and the range of data desired. For example, suppose you want firms in the 75th percentile of sales or greater. Then you would pick the variable 'sale' and move the slider to 0.75. Default selection is null."),
                                                            br(),
                                                            column(3, pickerInput(inputId = "var_sub", label = "Select variable", choices = "",
                                                                        options = list(
                                                                          title = "This is a placeholder")),
                                                            sliderInput("var_quant", label = ("Percentile range"), min = 0, 
                                                                        max = 1, value = c(0, 1))),
                                                            column(3, pickerInput(inputId = "var_sub_1", label = "Select variable", choices = "",
                                                                                  options = list(
                                                                                    title = "This is a placeholder")),
                                                                   sliderInput("var_quant_1", label = ("Percentile range"), min = 0, 
                                                                               max = 1, value = c(0, 1))),
                                                            column(3, pickerInput(inputId = "var_sub_2", label = "Select variable", choices = "logsale",
                                                                                  options = list(
                                                                                    title = "This is a placeholder")),
                                                                   awesomeRadio(
                                                                     inputId = "var_sub_2_gl",
                                                                     label = "",
                                                                     choices = c(">", "<"),
                                                                     selected = NULL
                                                                   ),
                                                                   numericInput("var_sub_2_val", label = "" , value = NA, min = NA, max = NA, step = NA)),
                                                            column(3, pickerInput(inputId = "var_sub_3", label = "Select variable", choices = "logsale",
                                                                                  options = list(
                                                                                    title = "This is a placeholder")),
                                                                   awesomeRadio(
                                                                     inputId = "var_sub_3_gl",
                                                                     label = "",
                                                                     choices = c(">", "<"),
                                                                     selected = NULL
                                                                   ),
                                                                   numericInput("var_sub_3_val", label = "" , value = NA, min = NA, max = NA, step = NA)),
                                                            br(),
                                                            column(8,
                                                            hr(),
                                                            br(),
                                                            h4("Choose whether to use panel data. For example, you can choose to follow all firms throughout the chosen time period which were in the top quartile of sales in 2009."),
                                                            h5(tags$b("Choose to panel (default is no):")),
                                                            materialSwitch(
                                                              inputId = "panel",
                                                              label = "Panel", 
                                                              value = FALSE,
                                                              status = "primary"
                                                            ),
                                                            pickerInput(inputId = "var_panel", label = "Follow all firms which had...", choices = "", options = list(title = "This is a placeholder.")),
                                                            sliderInput("var_quant_panel", label = ("in the percentile range"), min = 0, 
                                                                        max = 1, value = c(0, 1)),

                                                            pickerInput(
                                                              inputId = "var_panel_yr",
                                                              label = "in the year", 
                                                              choices = c(""),
                                                              selected = NULL,
                                                              options = list(
                                                                `actions-box` = TRUE,
                                                                title = "This is a placeholder.")
                                                            ),
                                                            hr())
                                                            # h4("Decide whether to truncate all data using the slider. Truncation is done symmetrically, so that if .01 is selected, then only data in the percentile range (.01,.99) will remain. Default is no truncation."),
                                                            # sliderInput("var_trunc", label = ("Percentile range for truncation"), min = 0, 
                                                            #             max = 0.5, value = 0),
                                                            # br(),
                                                            # hr()
               ))),
               tabPanel(
                 "Data Viewer",
                 fluidRow(column(
                   12,
                   h1("Data Viewer"),
                   p("This tab shows the data resulting from filtering on the previous two tabs. Note that this data is used for the remainder of the analysis."
                   )
                   )),
                 hr(),
                 fluidRow(
                   sidebarPanel(
                     width = 3,
                     checkboxGroupInput(
                       "show_vars_dv",
                       "Columns in dataframe to show:",
                       choices = c("tic", "NAICS", "fyear"),
                       selected = c("tic", "NAICS", "fyear")
                     ),
                     downloadButton("downloadData_dv", "Download")
                   ),
                   mainPanel(DT::dataTableOutput("mydata_dv", height = "100%"),
                             br(),
                             hr())
                 )
                 ),
               navbarMenu("Data Definitions", tabPanel("Company Variables",
                                                                 fluidRow(column(12, h1("Company Variables"),
                                                                          br(),
                                                                          p("These variables are company-specific identifiers and are generally invariant across time."),
                                                                          hr(),
                                                                          tableOutput("cvartab"),
                                                                          br(),
                                                                          hr()
                                                                 ))),
                                                        tabPanel("Balance Sheet Variables",
                                                                 fluidRow(column(12, h1("Balance Sheet Variables"),
                                                                          br(),
                                                                          p("These variables relate specifically to a firm's balance sheet."),
                                                                          hr(),
                                                                          tableOutput("balvartab"),
                                                                          br(),
                                                                          hr()
                                                                          
                                                                 ))),
                                                        tabPanel("Cash Flow Variables",
                                                                 fluidRow(column(12, h1("Cash Flow Variables"),
                                                                          br(),
                                                                          p("These variables relate specifically to a firm's cash flow in a given year."),
                                                                          hr(),
                                                                          tableOutput("cfvartab"),
                                                                          br(),
                                                                          hr()
                                                                 ))),
                                                        tabPanel("Tax Variables",
                                                                 fluidRow(column(12, h1("Tax Variables"),
                                                                          br(),
                                                                          p("These variables are company-specific tax rates made up from firm-level 10-K filings and Compustat data."),
                                                                          hr(),
                                                                          tableOutput("tvartab"),
                                                                          br(),
                                                                          hr()
                                                                 ))),
                                                        tabPanel("Income Statement Variables",
                                                                 fluidRow(column(12, h1("Income Statement Variables"),
                                                                          br(),
                                                                          p("These variables relate specifically to a firm's income statement."),
                                                                          hr(),
                                                                          tableOutput("isvtab"),
                                                                          br(),
                                                                          hr()
                                                                 ))),
                                                        tabPanel("Miscellaneous",
                                                                 fluidRow(column(12, h1("Miscellaneous Variables"),
                                                                          br(),
                                                                          p("These are company-specific miscellaneous variables pulled from Compustat or defined from previous data transformations."),
                                                                          hr(),
                                                                          tableOutput("mvartab"),
                                                                          br(),
                                                                          hr()
                                                                 ))),
                          tabPanel("NAICS Codes",
                                   fluidRow(column(12, h1("NAICS Codes"),
                                                   br(),
                                                   p("Definitions and descriptions for NAICS codes."),
                                                   hr(),
                                                   tableOutput("naicstab"),
                                                   br(),
                                                   hr())))
                          ))),
    
    
    tabPanel("Distributions",
             tabsetPanel(
               tabPanel(
                 "Histogram",
                 fluidRow(column(
                   12,
                   h1("Histogram"),
                   p("This tab helps us view the distribution of a particular variable, either across the whole time period or over a subset of that period using a histogram."),
                   h4("Instructions"),
                   p("Choose a variable from the dropdown. Decide whether to view the variable across all years or just for a particular year or range of years. Make a similar choice for NAICS codes. Decide whether to Use the slider to decide how much of the data to truncate."),
                   hr(),
                   fluidRow(
                     sidebarPanel(
                       width = 3,
                       br(),
                       pickerInput(
                         inputId = "hist",
                         label = h3("Histogram variable:"),
                         choices = c(""),
                         options = list(title = "This is a placeholder")
                       ),
                       pickerInput(
                         inputId = "hist_yr",
                         label = h3("Histogram Year"),
                         choices = c("ALL", ""),
                         options = list(title = "This is a placeholder",
                                        `actions-box` = TRUE), 
                         multiple = TRUE
                       ),
                       pickerInput(
                         inputId = "hist_naics",
                         label = h3("NAICS"),
                         choices = c("ALL", ""),
                         options = list(title = "This is a placeholder",
                                        `actions-box` = TRUE), 
                         multiple = TRUE
                       ),
                       
                       sliderInput("trunc_hist", label = h3("Truncation Range"), min = 0, 
                                   max = 1, value = c(.01, .99))
                     ),
                     mainPanel(plotlyOutput("hist1") %>% withSpinner(color="#0dc5c1"),
                               br(),
                               hr()
                     )
                   )
                 ))),
               tabPanel("Summary Statistics",
                        fluidRow(column(
                          12,
                          h1("Summary Statistics"),
                          p("View summary statistics on this tab."),
                          h4("Instructions"),
                          p("Choose which variable to view summary statistics for. Choose to weight equally or by sales weight. If desired, choose NAICS codes to subset by."),
                          hr())),
                        fluidRow(
                          sidebarPanel(
                            width = 3,
                            pickerInput(
                              inputId = "sumstat",
                              label = h3("Summary statistics variable:"),
                              choices = c(""),
                              options = list(title = "This is a placeholder.")
                            ),
                            materialSwitch(
                              inputId = "weight_sale",
                              label = "Sales-weighted",
                              value = FALSE, 
                              status = "info"
                            ),
                            pickerInput(
                              "ssnaics",
                              label = h3("Choose NAICS code"),
                              choices = c(""),
                              options = list(title = "This is a placeholder",
                                             `actions-box` = TRUE), 
                              multiple = TRUE
                            )
                          ),
                          mainPanel(br(),
                                    h1("Summary Statistics"),
                                    htmlOutput("sstab"),
                                    br(),
                                    hr())
                          
                          #htmlOutput("sstab"),
                          #       tags$style(HTML("#sstab table{
                          #                      margin: auto;
                          #                     }")))
                          
                        )),
               tabPanel("Outlier Analysis",
                        fluidRow(column(
                          12,
                          h1("Outlier Analys"),
                          p("Conduct outlier analysis on this tab using both a boxplot and a table view.."),
                          h4("Instructions"),
                          p("First, choose which variable to view a boxplot for. Then choose whether to group by NAICS code, year, or no group at all. Then choose whether to subset by year or NAICS code. Under the boxplot, view rows of outliers and decide which variables to view. An option to download as a csv is available."),
                          hr())),
                        fluidRow(
                          sidebarPanel(width = 3,
                                       pickerInput(inputId = "boxvar_1", label = "Select variables", choices = c(""), options = list(title = "This is a placeholder",
                                                                                                                                   `actions-box` = TRUE)
                                       ),
                                       prettyRadioButtons(
                                         inputId = "boxfacet",
                                         label = "Choose whether to group and if so, then by year or industry:", 
                                         choices = c("None","Year", "Industry"),
                                         inline = TRUE, 
                                         status = "danger",
                                         fill = TRUE
                                       ),
                                       pickerInput(inputId = "boxyear_1", label = "Select years", choices = c(""), options = list(title = "This is a placeholder",
                                                                                                                                `actions-box` = TRUE), multiple = TRUE),
                                       
                                       pickerInput(inputId = "boxnaics_1", label = "Select industries", choices = c(""),options = list(title = "This is a placeholder",
                                                                                                                                     `actions-box` = TRUE),
                                                   multiple = TRUE),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       hr(),
                                       checkboxGroupInput(
                                         "show_vars_outliers",
                                         "Columns in dataframe to show:",
                                         choices = c("tic", "NAICS", "fyear"),
                                         selected = c("tic", "NAICS", "fyear")
                                       ),
                                       downloadButton("downloadData_outliers", "Download")
                                       ),
                          
                          mainPanel(plotlyOutput("boxplot1") %>% withSpinner(color="#0dc5c1"),
                                    hr(),
                                    DT::dataTableOutput("mydata_outliers", height = "100%"),
                                    br(),
                                    hr()
                                    
                          ))
               )             )),
    
    
    tabPanel("Line Plots",
             fluidRow(column(
               12,
               h1("Line Plot"),
               p("This tab helps us view the movement of variables across time for a particular corporation. The first tab contains all firms, the second contains aggregates of firms by NAICS code, and the third tab allows the user to select which firms to view."
               ),
               h4("Instructions"),
               p(
                 "Use the dropdown menu to choose a variable to plot against time on the x-axis. Double-click on a NAICS code on the legend to focus in on that code. Hover the cursor over a particular firm-year to view relevant information."
               )
             )),
      tabsetPanel(tabPanel(
        "All Firms",
        
        hr(),
        fluidRow(
          sidebarPanel(
            width = 3,
            helpText("Choose which variable to plot"),
            pickerInput(
              inputId = "scatter",
              label = h3("Variable:"),
              choices = c(""),
              options = list(title = "This is a placeholder")
            ),
            pickerInput(
              "linenaics",
              label = h5("Choose NAICS code"),
              choices = c(""),
              options = list(title = "This is a placeholder",
                             `actions-box` = TRUE), 
              multiple = TRUE
            ),
            pickerInput("var_sub_line", label = h5("Choose to follow firms which had"),
                        choices = c(""),
                        options = list(title = "This is a placeholder",
                                       `actions-box` = TRUE)),
            sliderInput("trunc_line", label = h5("in the percentile range"), min = 0, 
                        max = 1, value = c(0, 1)),
            pickerInput("line_year", label = h5("in the year"), choices = c(""))
          ),
          
          mainPanel(plotlyOutput("Ellen", height = 500) %>% withSpinner(color="#0dc5c1"),
                    br(),
                    hr())
        )
      ),
      tabPanel("NAICS",
               hr(),
               fluidRow(
                 sidebarPanel(
                   width = 3,
                   helpText("Choose which variable to plot"),
                   pickerInput(
                     inputId = "scatter_1",
                     label = h3("Variable:"),
                     choices = c(""),
                     options = list(title = "This is a placeholder")
                   ),
                   pickerInput(
                     "linenaics_1",
                     label = h5("Choose NAICS code"),
                     choices = c(""),
                     options = list(title = "This is a placeholder",
                                    `actions-box` = TRUE), 
                     multiple = TRUE
                   ),
                   pickerInput("var_sub_line_1", label = h5("Choose to follow firms which had"),
                               choices = c(""),
                               options = list(title = "This is a placeholder",
                                              `actions-box` = TRUE)),
                   sliderInput("trunc_line_1", label = h5("in the percentile range"), min = 0, 
                               max = 1, value = c(0, 1)),
                   pickerInput("line_year_1", label = h5("in the year"), choices = c(""))
                 ),
                 
                 mainPanel(plotlyOutput("Ellen_naics", height = 500) %>% withSpinner(color="#0dc5c1"),
                           br(),
                           hr())
               )),
      tabPanel("Single Firm",
                hr(),
               fluidRow(
                 sidebarPanel(
                   width = 3,
                   helpText("Choose which variable to plot"),
                   pickerInput(
                     inputId = "scatter_2",
                     label = h3("Variable:"),
                     choices = c(""),
                     options = list(title = "This is a placeholder")
                   ),
                   h3("To narrow the list of TICs, you can filter by NAICS code or subset the data by percentile:"),
                   selectizeInput(
                     "line_tic",
                     label = h5("Choose TIC"),
                     choices = c(""),
                     options = list(placeholder = "This is a placeholder",
                                    `actions-box` = TRUE), 
                     multiple = TRUE
                   ), 
                   pickerInput(
                     "linenaics_2",
                     label = h5("Choose NAICS code"),
                     choices = c(""),
                     options = list(title = "This is a placeholder",
                                    `actions-box` = TRUE), 
                     multiple = TRUE
                   ),
                   pickerInput("var_sub_line_2", label = h5("Choose to follow firms which had"),
                               choices = c(""),
                               options = list(title = "This is a placeholder",
                                              `actions-box` = TRUE)),
                   sliderInput("trunc_line_2", label = h5("in the percentile range"), min = 0, 
                               max = 1, value = c(0, 1)),
                   pickerInput("line_year_2", label = h5("in the year"), choices = c(""))
                 ),
                 
                 mainPanel(plotlyOutput("Ellen_single_firm", height = 500) %>% withSpinner(color="#0dc5c1"),
                           br(),
                           hr())
               ))
    )),
    
    tabPanel(
      "Scatterplot",
      fluidRow(column(
        12,
        h1("Scatterplot"),
        p("This tab helps us view the movement of variables across time for a particular corporation. Each line represents a single corporation color-coded by NAICS. The size of a dot corresponds to the size of the firm represented by that dot in terms of log sales."
        ),
        h4("Instructions"),
        p("Use the dropdown menu to choose which variable to plot. Hover over a firm-year to see additional relevant information.")
      )),
      hr(),
      fluidRow(
        sidebarPanel(
          width = 3,
          helpText("Choose which variabl to view"),
          pickerInput(
            inputId = "scatter_a",
            label = h3("Variable:"),
            choices = c(""),
            options = list(title = "This is a placeholder.")
          ),
          pickerInput(
            "scatternaics",
            label = h5("Choose NAICS code"),
            choices = c(""),
            options = list(title = "This is a placeholder",
                           `actions-box` = TRUE), 
            multiple = TRUE
          ),
          pickerInput("var_sub_scatter", label = h5("Choose to follow firms which had"),
                      choices = c(""),
                      options = list(title = "This is a placeholder",
                                     `actions-box` = TRUE)),
          sliderInput("trunc_scatter", label = h5("in the percentile range"), min = 0, 
                      max = 1, value = c(0, 1)),
          pickerInput("scatter_year", label = h5("in the year"), choices = c("")),
          pickerInput("scatter_size", label = h4("Which variable should determine dot size?"), choices = c(""),options = list(title = "This is a placeholder",
                                                                                                                              `actions-box` = TRUE))
        ),
        mainPanel(
          plotlyOutput("Anmol", height = 500) %>% withSpinner(color="#0dc5c1"),
          br(),
          hr()
        )
      )
    ),
    
    
             
tabPanel("Regression",tabsetPanel(tabPanel(
  "Model Summary",
  fluidRow(column(
    12,
    h1("Model Summary"),
    p("Use this tab to run regressions on any combination of variables."),
    br(),
    h4("Instructions"),
    p("Use sidebar to choose dependent variable, independent variable(s), and interaction terms between continuous and discrete variables. Note that it is possible to run both OLS and panel regressions. Conduct the latter by using fyear and tic as independent variables. Scroll down for a plot of fitted vs actual values and a residual plot."
    )
  )),
  hr(),
  fluidRow(
    sidebarPanel(
      width = 3,
      uiOutput("dependent"),
      uiOutput("independent"),
      tags$hr(),
      h5('Generate New Interaction Variables Here!'),
      uiOutput("makeFactInteract"),
      uiOutput("makeNumInteract"),
      uiOutput("uiAdded"),
      #uiOutput("interactionTerms"),
      #uiOutput("interacts"),
      actionButton("actionBtnAdd", "Create Interaction Term!")
    ),
    mainPanel(
      htmlOutput("regTab"),
      tags$style(HTML("#regTab table{
                      margin: auto;
                      }")),
                br(),
      hr(),
      
      br(),
      hr(),
      br(),
      plotlyOutput("fittdPlot") %>% withSpinner(color="#0dc5c1"),
      br(),
      hr(),
      br(),
      plotlyOutput("residualPlot") %>% withSpinner(color="#0dc5c1"),
      br(),
      hr()
      )
  )
),
tabPanel("Data with Residuals",fluidRow(column(
  12,
  h1("Data with Residuals"),
  p("This tab shows the data obtained from the SEC Edgar database and merged with Compustat, in addition to residuals obtained from regression modelling. Note that this is compressed to only contain the variables of interest to us.
    Filter, sort and search. Select which variables to display using the checkboxes below and download using the download button if desired."
  )
  )),
  hr(),
  fluidRow(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(
        "show_vars_resid",
        "Columns in dataframe to show:",
        choices = ""
      ),
      downloadButton("downloadData_resid", "Download")
    ),
    mainPanel(DT::dataTableOutput("myresiduals", height = "100%"),
              br(),
              hr())
  ))
)),
tabPanel(
  "Raw Data",
  fluidRow(column(
    12,
    h1("The raw data"),
    p("This tab shows the data obtained from the SEC Edgar database and merged with Compustat. Note that this is compressed to only contain the variables of interest to us.
      Filter, sort and search. Select which variables to display using the checkboxes below and download using the download button if desired."
    )
    )),
  hr(),
  fluidRow(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(
        "show_vars",
        "Columns in dataframe to show:",
        names(nu_comp_clean_q_sale),
        selected = names(nu_comp_clean_q_sale)[1:5]
      ),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(DT::dataTableOutput("mydata", height = "100%"),
              br(),
              hr())
  )
  )

                        )

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  #Datalist tables
  
  output$cvartab <- renderTable({datalist_cvar})
  output$balvartab <- renderTable({datalist_bal})
  output$cfvartab <- renderTable({datalist_cfv})
  output$tvartab <- renderTable({datalist_tax})
  output$isvtab <- renderTable({datalist_isv})
  output$mvartab <- renderTable({datalist_mvar})
  output$naicstab <- renderTable({naics_def})

  
  
  varselect <- reactive({
    if(is.null(input$var_select)){
      varselect <- c()
    } else{
     varselect <- c(input$var_select)
    }
  })
 
  output$var_tab <- renderTable({filter(datalist_full, datalist_full$Variable %in% varselect())})
  
  observe({
    varselect <- c(input$var_select)
    naics_list <- c(input$naics_select)
    updatePickerInput(session, inputId = "linenaics_2", choices = c(naics_list), selected = naics_list)
    updatePickerInput(session, inputId = "var_sub_line_2", choices = c("None selected", varselect), selected = "None selected")
    
    
    yr_select <- c(input$year_select)
    
    updatePickerInput(session, inputId = "line_year_2", choices = c(yr_select), selected = yr_select)
  })
  
  
  
  
  tic_choices <- reactive({
    
    
    if(!is.null(input$var_select) & !is.null(input$linenaics_2)){  
      
      df_ellen <-filter(df_1(), NAICS %in% c(input$linenaics_2))
      
      
      df_1 <- if(input$var_sub_line_2 != "None selected"){
        df_2 <- df_ellen
        
        df_sale <- df_2 %>% group_by(fyear) %>% summarise(
          "lower" = stats::quantile(df_2[,get(input$var_sub_line_2)], input$trunc_line_2[1], na.rm = T),
          "upper" = stats::quantile(df_2[,get(input$var_sub_line_2)], input$trunc_line_2[2], na.rm = T)
        ) %>% ungroup()
        
        df_sale <- filter(df_sale, fyear == input$line_year_2)
        
        df_q_sale <- subset(df_2, fyear == input$line_year_2 & get(input$var_sub_line_2) >= df_sale$lower[1] & get(input$var_sub_line_2) <= df_sale$upper[1])
        
        q_sale_tic <- df_q_sale$tic
        
        df_ellen_1 <- df_2[df_2$tic %in% q_sale_tic, ]
        
        tic_choices1 <- unique(df_ellen_1$tic)
        
        tic_choices <- as.data.frame(tic_choices1)
      } else {
        tic_choices1 <- unique(df_ellen$tic)
        
        tic_choices <- as.data.frame(tic_choices1)
      }
    }
    else{
      tic_choices <- data.frame("tic_choices1" = "NA")
    }
    
  })
  
  
  
   observe({
    varselect_1 <- c(input$var_select)
   
     updatePickerInput(session, inputId = "var_sub", choices = varselect_1 )
     
     updatePickerInput(session, inputId = "var_sub_1", choices = varselect_1)
     
     updatePickerInput(session, inputId = "var_sub_2",
                       choices = varselect_1)
     
     updatePickerInput(session, inputId = "var_sub_3",
                       choices = varselect_1)
     
     updatePickerInput(session, inputId = "var_panel",
                       choices = varselect_1)
  
    updateCheckboxGroupInput(session, inputId = "show_vars_dv", choices = c("tic", "fyear", "NAICS", varselect_1), selected = c("tic", "fyear", "NAICS"))
       
    updateCheckboxGroupInput(session, inputId = "show_vars_outliers", choices = c("tic", "fyear", "NAICS", varselect_1), selected = c("tic", "fyear", "NAICS"))
    
    
       updatePickerInput(session, inputId = "scatter", choices = varselect_1, selected = varselect_1[1])
       
       updatePickerInput(session, inputId = "scatter_1", choices = varselect_1, selected = varselect_1[1])
       
      updatePickerInput(session, inputId = "scatter_2", choices = varselect_1, selected = varselect_1[1])


      updatePickerInput(session, inputId = "scatter_a", choices = varselect_1, selected = varselect_1[1])

      updatePickerInput(session, inputId = "hist", choices = varselect_1, selected = varselect_1[1])

      updatePickerInput(session, inputId = "sumstat", choices = varselect_1, selected = varselect_1[1])
      
      updatePickerInput(session, inputId = "boxvar", choices = varselect_1, selected = varselect_1[1])
      
      updatePickerInput(session, inputId = "boxvar_1", choices = varselect_1, selected = varselect_1[1])

      updatePickerInput(session, inputId = "var_sub_line", choices = c("None selected", varselect_1), selected = "None selected")

      updatePickerInput(session, inputId = "var_sub_line_1", choices = c("None selected", varselect_1), selected = "None selected")

      updatePickerInput(session, inputId = "var_sub_scatter", choices = c("None selected", varselect_1), selected = "None selected")

      updatePickerInput(session, inputId = "scatter_size", choices = c("None selected", varselect_1), selected = "None selected")

      updateCheckboxGroupInput(session, inputId = "show_vars_resid", choices = c(varselect_1,"tic", "fyear", "NAICS", "residuals"), select = c("tic", "fyear", "NAICS", "residuals"))

      naics_list <- c(input$naics_select)

      updatePickerInput(session, inputId = "ssnaics", choices = c(naics_list), selected = naics_list)

      updatePickerInput(session, inputId = "linenaics", choices = c(naics_list), selected = naics_list)

      updatePickerInput(session, inputId = "linenaics_1", choices = c(naics_list), selected = naics_list)

      updatePickerInput(session, inputId = "scatternaics", choices = c(naics_list), selected = naics_list)

      updatePickerInput(session, inputId = "hist_naics", choices = c(naics_list), selected = naics_list)
      
      updatePickerInput(session, inputId = "boxnaics", choices = c(naics_list), selected = naics_list)
      
      updatePickerInput(session, inputId = "boxnaics_1", choices = c(naics_list), selected = naics_list)

       yr_select <- c(input$year_select)
       
       updatePickerInput(session, inputId = "var_panel_yr", choices = c(yr_select))
       
       updatePickerInput(session, inputId = "hist_yr", choices = c(yr_select), selected = yr_select)
       
       updatePickerInput(session, inputId = "line_year", choices = c(yr_select), selected = yr_select)
       
       updatePickerInput(session, inputId = "line_year_1", choices = c(yr_select), selected = yr_select)
       
       updatePickerInput(session, inputId = "scatter_year", choices = c(yr_select), selected = yr_select)
       
       updatePickerInput(session, inputId = "boxyear", choices = c(yr_select), selected = yr_select)
       
       updatePickerInput(session, inputId = "boxyear_1", choices = c(yr_select), selected = yr_select)
       
       
    })
  
  observe({
    tic_select <- tic_choices()
    
    updateSelectizeInput(session, inputId = "line_tic", choices = tic_select$tic_choices1, selected = tic_select$tic_choices1[1])
  })
  
  
  ##Defining DF
  
  
  df_1 <- reactive({

      df_1 <- nu_comp_clean_q_sale[, c("tic", "fyear", "NAICS", "logsale", input$var_select)]
      
      df_1 <- filter(df_1, NAICS %in% input$naics_select)
      df_1 <- filter(df_1, fyear %in% input$year_select)
      
      df_1 <-  if(input$var_sub == "" & input$var_sub_1 == ""){
        df_1
      } else if (input$var_sub == "" & input$var_sub_1 != ""){
        subset(df_1, get(input$var_sub_1) <= quantile(df_1[, input$var_sub_1], input$var_quant_1[2], na.rm = T) &
                 get(input$var_sub_1) >= quantile(df_1[, input$var_sub_1], input$var_quant_1[1], na.rm = T))
             }
      else if(input$var_sub != "" & input$var_sub_1 == ""){ 
        subset(df_1, get(input$var_sub) <= quantile(df_1[, input$var_sub], input$var_quant[2], na.rm = T) &
                 get(input$var_sub) >= quantile(df_1[, input$var_sub], input$var_quant[1], na.rm = T)) 
        }
      else if(input$var_sub != "" & input$var_sub_1 != ""){ 
        subset(df_1, (get(input$var_sub_1) <= quantile(df_1[, input$var_sub_1], input$var_quant_1[2], na.rm = T) &
                 get(input$var_sub_1) >= quantile(df_1[, input$var_sub_1], input$var_quant_1[1], na.rm = T)) & 
                 ( get(input$var_sub) <= quantile(df_1[, input$var_sub], input$var_quant[2], na.rm = T) &
                          get(input$var_sub) >= quantile(df_1[, input$var_sub], input$var_quant[1], na.rm = T)))
        }
      
      df_1 <- if (input$var_sub_2 != "" & input$var_sub_2_gl == "<" & !is.na(input$var_sub_2_val)) {
        subset(df_1, get(input$var_sub_2) < (input$var_sub_2_val))
      } else if(input$var_sub_2 != "" & input$var_sub_2_gl == ">" & !is.na(input$var_sub_2_val)) {
        subset(df_1, get(input$var_sub_2) > (input$var_sub_2_val))
      }
      else{
        df_1}
      
      
      df_1 <- if (input$var_sub_3 != "" & input$var_sub_3_gl == "<" & !is.na(input$var_sub_3_val)) {
        subset(df_1, get(input$var_sub_3) < (input$var_sub_3_val))
      } else if(input$var_sub_3 != "" & input$var_sub_3_gl == ">" & !is.na(input$var_sub_3_val)) {
        subset(df_1, get(input$var_sub_3) > (input$var_sub_3_val))
      }
      else{
        df_1}
      
      
  df_1 <- if(input$panel == TRUE & input$var_panel != ""){
    df_2 <- df_1

    df_sale <- df_2 %>% group_by(fyear) %>% summarise(
      "lower" = quantile(df_2[,input$var_panel], input$var_quant_panel[1], na.rm = T),
      "upper" = quantile(df_2[,input$var_panel], input$var_quant_panel[2], na.rm = T)
    ) %>% ungroup()

    df_sale <- filter(df_sale, fyear == input$var_panel_yr)

    df_q_sale <- subset(df_2, fyear == input$var_panel_yr & get(input$var_panel) >= df_sale$lower[1] & get(input$var_panel) <= df_sale$upper[1])

    q_sale_tic <- df_q_sale$tic

    df_1 <- df_2[df_2$tic %in% q_sale_tic, ]
  } else {
    df_1
  }
  
 
  # df_1 <- if(input$var_trunc > 0){
  #   treat_outliers(df_1,input$var_trunc, truncate = TRUE)
  # } else{
  #   df_1
  # }
  # 
  })
  
  
  output$mydata_dv <- DT::renderDataTable({
    df_1 <- df_1()
    df_1 <- mutate_if(df_1, is.numeric, round, digits = 6)
    
    DT::datatable(df_1[, input$show_vars_dv, drop = FALSE],
                  options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  })
  
  output$downloadData_dv <- downloadHandler(
    filename = function() {
      paste("data_sub-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_1(), file)
    }
  )
  
  
  
  #Histogram
  output$hist1 <- renderPlotly({
    
      if(!is.null(input$var_select)){
      df <- filter(df_1(), fyear %in% c(input$hist_yr), NAICS %in% c(input$hist_naics)) 
      
      df <- as.data.frame(df)
      
      hist1 <- plot_ly(data = df,
                       x = ~get(input$hist),
                       type = "histogram",
                       histnorm = "probability")
      
      
      hist1 <- hist1 %>% layout(
        title = 'Histogram',
        xaxis = list(title = input$hist,
                     
                     range=c(quantile(df[, input$hist], input$trunc_hist[1], na.rm = T), quantile(df[, input$hist], input$trunc_hist[2], na.rm = T))),
        yaxis = list(title = "Probability"))
      
      hist1
      }
    else{
      hist1 <- plotly_empty()
    }
  })
  
  #Summary Statistics
  output$sstab <- renderText ({
    
    if(!is.null(input$var_select)){
    
    df_ss <- df_1()
      
      if (input$weight_sale == TRUE) {
          df <- filter(df_ss, NAICS %in% c(input$ssnaics)) 
          
          sstab <- df %>% group_by(fyear) %>% summarise(
            "Average" = weighted.mean(get(input$sumstat), w = sale, na.rm = T),
            "St Dev" = sd(get(input$sumstat), na.rm = T),
            "per_10" = weighted_quantile(get(input$sumstat), w = sale, 0.1, na.rm = T),
            "per_25" = weighted_quantile(get(input$sumstat), w = sale, 0.25, na.rm = T),
            "Median" = weighted_median(get(input$sumstat), w = sale, na.rm = T),
            "per_75" = weighted_quantile(get(input$sumstat), w = sale, 0.75, na.rm = T),
            "per_90" = weighted_quantile(get(input$sumstat), w = sale, 0.9, na.rm = T),
            "count" = n()
          ) %>% ungroup()
          
          
          kable(sstab, digits = 4, format = "html") %>%
            kable_styling(
              font_size = 15,
              bootstrap_options = c("striped", "hover", "condensed"))
      } else if(input$weight_sale == FALSE){
          df <- filter(df_ss, NAICS %in% c(input$ssnaics)) 
          
          sstab <- df %>% group_by(fyear) %>% summarise(
            "Average" = mean(get(input$sumstat), na.rm = T),
            "St Dev" = sd(get(input$sumstat), na.rm = T),
            "per_10" = quantile(get(input$sumstat), 0.1, na.rm = T),
            "per_25" = quantile(get(input$sumstat), 0.25, na.rm = T),
            "Median" = median(get(input$sumstat), na.rm = T),
            "per_75" = quantile(get(input$sumstat), 0.75, na.rm = T),
            "per_90" = quantile(get(input$sumstat), 0.9, na.rm = T),
            "count" = n()
          ) %>% ungroup()
          
          
          kable(sstab, digits = 4, format = "html") %>%
            kable_styling(
              font_size = 15,
              bootstrap_options = c("striped", "hover", "condensed"))
        }
    } else{
      print(HTML("Warning: Please select parameters in the data setup tab."))
    }
    
  })
  
  
  #Box plot
  
  
  output$boxplot1 <- renderPlotly({
    if(!is.null(input$var_select) & !is.null(input$boxnaics_1) & !is.null(input$boxyear_1)){
      if(input$boxfacet == "None"){
      df_box <- df_1()
      
      df_box <- filter(df_box, NAICS %in% c(input$boxnaics_1), fyear %in% c(input$boxyear_1))
      
      fig <- ggplot(df_box, aes(y=get(input$boxvar_1))) +  geom_boxplot() + theme_minimal()
      
      fig <- ggplotly(fig)
      
      
      ax <- list(
        title = "",
        zeroline = TRUE,
        showline = TRUE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      
      fig <- fig %>% layout(
        xaxis = ax,
        yaxis = list(title = input$boxvar_1),
        showlegend = FALSE)
      
      }
      else if(input$boxfacet == "Year"){
        df_bp <- filter(df_1(), NAICS %in% c(input$boxnaics_1), fyear %in% c(input$boxyear_1))
        
        fig <- ggplot(df_bp, aes(x=as.factor(fyear), y=get(input$boxvar_1),color = as.factor(fyear), group = fyear)) +  geom_boxplot() + theme_minimal()
        
        fig <- ggplotly(fig)
      
  
          fig <- fig %>% layout(
            xaxis = list(title = 'Year'),
          yaxis = list(title = input$boxvar_1),
          showlegend = FALSE)
          
}
      else{
        df_bp <- filter(df_1(), NAICS %in% c(input$boxnaics_1), fyear %in% c(input$boxyear_1))
        
        fig <- ggplot(df_bp, aes(x=as.factor(NAICS), y=get(input$boxvar_1),color = NAICS, group = NAICS)) +  geom_boxplot() + theme_minimal()
        
        fig<- ggplotly(fig, tooltip = c("tic", input$boxvar_1))
        fig <- fig %>% layout(
          xaxis = list(title = 'NAICS'),
          yaxis = list(title = input$boxvar_1),
          showlegend = FALSE)
      }
    }
    else {
      fig <- plotly_empty()
    }
  })
  
  df_outliers <- reactive({
    df_outliers1 <- filter(df_1(), NAICS %in% c(input$boxnaics_1), fyear %in% c(input$boxyear_1))
    
    df_outliers <- if(input$boxfacet == "Year"){
      as.data.frame(df_outliers1 %>% 
        group_by(fyear) %>% 
        identify_outliers(input$boxvar_1))
    } else if(input$boxfacet == "Industry"){
      as.data.frame(df_outliers1 %>% 
        group_by(NAICS) %>% 
        identify_outliers(input$boxvar_1))
    } else{
      as.data.frame(df_outliers1 
            %>% identify_outliers(input$boxvar_1))
    }
    })
  
  output$mydata_outliers <- DT::renderDataTable({
    df_outliers1 <- df_outliers()
    df_outliers1 <- mutate_if(df_outliers1, is.numeric, round, digits = 6)
    
    DT::datatable(df_outliers1[, input$show_vars_outliers, drop = FALSE],
                  options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  })
  
  
  
  output$downloadData_outliers <- downloadHandler(
     
     filename = function() {
       paste("data_outliers-", Sys.Date(), ".csv", sep = "")
     },
     content = function(file) {
       write.csv(df_outliers(), file)
     }
   )
  
  
  #Anmol's Plot
  output$Anmol <- renderPlotly({
    
    
    if(!is.null(input$var_select) & !is.null(input$scatternaics)){  
      
      df_scatter <-filter(df_1(), NAICS %in% c(input$scatternaics))
      
      
      df_scatter <- if(input$var_sub_scatter != "None selected"){
        df_2 <- df_scatter
        
        df_sale <- df_2 %>% group_by(fyear) %>% summarise(
          "lower" = quantile(df_2[,input$var_sub_scatter], input$trunc_scatter[1], na.rm = T),
          "upper" = quantile(df_2[,input$var_sub_scatter], input$trunc_scatter[2], na.rm = T)
        ) %>% ungroup()
        
        df_sale <- filter(df_sale, fyear == input$scatter_year)
        
        df_q_sale <- subset(df_2, fyear == input$scatter_year & get(input$var_sub_scatter) >= df_sale$lower[1] & get(input$var_sub_scatter) <= df_sale$upper[1])
        
        q_sale_tic <- df_q_sale$tic
        
        df_scatter <- df_2[df_2$tic %in% q_sale_tic, ]
      } else {
        df_scatter
      }
    
      if(input$scatter_size != "None selected"){
      
    Anmol <-
      plot_ly(
        data = df_scatter,
        x = ~ fyear,
        y = ~  get(input$scatter_a),
        color = ~ as.factor(NAICS),
        size = ~ logsale,
        hoverinfo = 'text',
        text = ~ paste(
          "</br>TIC: ",
          tic, "</br>NAICS:", NAICS,
          "</br>Variable Value:", round(get(input$scatter_a), digits = 5),
          "</br>Year:", fyear
        ),
        type = "scatter",
        mode = "markers"
      )
    
    Anmol <- Anmol %>% layout(
      xaxis = list(title = 'Year'),
      yaxis = list(title = input$scatter_a),
      legend = list(title = list(text = '<b> NAICS </b>'))
    )
    
    Anmol
      }
      else{
        Anmol <-
          plot_ly(
            data = df_scatter,
            x = ~ fyear,
            y = ~  get(input$scatter_a),
            color = ~ as.factor(NAICS),
            hoverinfo = 'text',
            text = ~ paste(
              "</br>TIC: ",
              tic, "</br>NAICS:", NAICS,
              "</br>Variable Value:", round(get(input$scatter_a), digits = 5),
              "</br>Year:", fyear
            ),
            type = "scatter",
            mode = "markers"
          )
        
        Anmol <- Anmol %>% layout(
          xaxis = list(title = 'Year'),
          yaxis = list(title = input$scatter_a),
          legend = list(title = list(text = '<b> NAICS </b>'))
        )
        
        Anmol
      }
    }
    else{
      Anmol <- plotly_empty()
    }
    
    
   # Anmol <- ggplot() +
    ##  geom_point(data = nu_comp_clean_q_sale, aes(
    #    x = fyear,
    #    y = get(input$scatter),
    #    group = tic,
    #    color = factor(NAICS)
    #  )) +
     # ylab("Rate") +
    #  labs(color = "") +
    #  xlab("Year") +
    #  my_econ +
    #  scale_colour_economist()
    
    
  #  Anmol <- ggplotly(Anmol)
  #  
  #  Anmol <-
  #    Anmol  %>% layout(legend = list(title = list(text = '<b> NAICS </b>')))
  #  Anmol
    
    
  })
  
  
  #Ellen's Plot
  output$Ellen <- renderPlotly({
    
  #  Ellen <-
  #    plot_ly(
  #      data = nu_comp_clean_q_sale,
  #      x = ~ as.factor(fyear),
  #      y = ~  get(input$scatter),
  #      hoverinfo = 'text',
  #      text = ~ paste(
  #        "</br>TIC: ",
  #        tic,  "</br>Year", fyear, 
  #        "</br>Effective Rate: ",
  #        Effective_Rate,"</br>Foreign Rate: ",
  #        Foreign_Rate
  #      ),
  #      type = "scatter",
  #      mode = "lines",
  #      transforms = list(list(
  #        color = ~as.factor(NAICS),
  #        
   #       type = 'groupby',
  #        groups = ~tic
  #      ))
  #      
  #    )
  #  
   # Ellen <- Ellen %>% layout(
  #    xaxis = list(title = 'Year'),
  #    yaxis = list(title = input$scatter),
  #    legend = list(title = list(text = '<b> NAICS </b>'))
  #  )
  #  
  #  Ellen
    
    
    #Ellen1 <- ggplot(data = nu_comp_clean_q_sale, aes(x = fyear,
    #   y = get(input$scatter), group = tic, color = factor(naics))) +
    # geom_line()

    #Ellen2 <- Ellen1 + geom_line(aes(color = factor(NAICS), alpha = quant_sale)) + scale_alpha_manual(values = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))


    #ylab("Rate") +
    #xlab("Year") +
    #labs(color = "NAICS") +
    #my_econ_white +
    #  scale_colour_economist()

  if(!is.null(input$var_select) & !is.null(input$linenaics)){  
    
    df_ellen <-filter(df_1(), NAICS %in% c(input$linenaics))
    
    df_ellen <- as.data.frame(df_ellen)
    
    df_1 <- if(input$var_sub_line != "None selected"){
      df_2 <- df_ellen
      
      df_sale <- df_2 %>% group_by(fyear) %>% summarise(
        "lower" = quantile(df_2[,input$var_sub_line], input$trunc_line[1], na.rm = T),
        "upper" = quantile(df_2[,input$var_sub_line], input$trunc_line[2], na.rm = T)
      ) %>% ungroup()
      
      df_sale <- filter(df_sale, fyear == input$line_year)
      
      df_q_sale <- subset(df_2, fyear == input$line_year & get(input$var_sub_line) >= df_sale$lower[1] & get(input$var_sub_line) <= df_sale$upper[1])
      
      q_sale_tic <- df_q_sale$tic
      
      df_ellen <- df_2[df_2$tic %in% q_sale_tic, ]
    } else {
      df_ellen
    }
    
    # 
    # if(input$var_sub_line != "None selected"){
    #   df_ellen <- subset(df_ellen, get(input$var_sub_line) <= quantile(df_ellen[, input$var_sub_line], input$trunc_line[2], na.rm = T) &
    #            get(input$var_sub_line) >= quantile(df_ellen[, input$var_sub_line], input$trunc_line[1], na.rm = T))
    # }
    # else{df_ellen
    # }

    Ellen <- ggplot() +
      geom_line(
       data = df_ellen,
        linetype = "solid",
        aes_(
          x = as.name("fyear"),
          y = as.name(input$scatter),
          group = as.name("tic"),
          color = factor(df_ellen$NAICS)
        )
      ) +
    labs(color = "") +
      ylab(input$scatter) +
     xlab("Year") +
    theme_minimal()
      #my_econ_white +
      #scale_colour_economist()
  

    Ellen <- ggplotly(Ellen)

    Ellen <-
      Ellen  %>% layout(legend = list(title = list(text = '<b> NAICS </b>')))
    Ellen

#    Ellen3 <-ggplotly(Ellen2)

    #Ellen <- Ellen3 %>% style(Ellen2, showlegend = FALSE, traces = 1:length(unique(nu_comp_clean_q_sale$NAICS))
  }
    else{
      Ellen <- plotly_empty()
    }
  })
  
  
  #Ellen's Plot (one line per NAICS code)
  output$Ellen_naics <- renderPlotly({
    
   
    if(!is.null(input$var_select) & !is.null(input$linenaics_1)){  
      
      df_ellen <-filter(df_1(), NAICS %in% c(input$linenaics_1))
      
      
      df_1 <- if(input$var_sub_line_1 != "None selected"){
        df_2 <- df_ellen
        
        df_sale <- df_2 %>% group_by(fyear) %>% summarise(
          "lower" = quantile(df_2[,input$var_sub_line_1], input$trunc_line_1[1], na.rm = T),
          "upper" = quantile(df_2[,input$var_sub_line_1], input$trunc_line_1[2], na.rm = T)
        ) %>% ungroup()
        
        df_sale <- filter(df_sale, fyear == input$line_year_1)
        
        df_q_sale <- subset(df_2, fyear == input$line_year_1 & get(input$var_sub_line_1) >= df_sale$lower[1] & get(input$var_sub_line_1) <= df_sale$upper[1])
        
        q_sale_tic <- df_q_sale$tic
        
        df_ellen_1 <- df_2[df_2$tic %in% q_sale_tic, ]
        
        df_ellen <- df_ellen_1 %>% group_by(fyear, NAICS) %>% summarise(
          "average" = mean(get(input$scatter_1), na.rm = T),
          "count" = n()
        ) %>% ungroup()
        
        df_ellen <- df_ellen[order(df_ellen$NAICS, df_ellen$fyear),]
  
        
      } else {
        df_2 <- df_ellen
        
        df_ellen <- df_2 %>% group_by(fyear, NAICS) %>% summarise(
          "average" = mean(get(input$scatter_1), na.rm = T),
          "count" = n()
        ) %>% ungroup()
        
        df_ellen <- df_ellen[order(df_ellen$NAICS, df_ellen$fyear),]
      }
      
      
      Ellen <- plot_ly(
        data = df_ellen,
        x = ~ fyear,
        y = ~  (average),
        color = ~ as.factor(NAICS),
        hoverinfo = 'text',
        text = ~ paste(
          "</br>NAICS:", NAICS,
          "</br>Variable Value:", round(average, digits = 5),
          "</br>Year:", fyear,
          "</br>Count:", count
        ),
        type = "scatter",
        mode = "lines + markers"
      )
      
      Ellen_naics <- Ellen %>% layout(
        xaxis = list(title = 'Year'),
        yaxis = list(title = input$scatter_1),
        legend = list(title = list(text = '<b> NAICS </b>'))
      )
      
      Ellen_naics
      
    }
    else{
      Ellen_naics <- plotly_empty()
    }
  })
  
  
  #Ellen's Plot (one line per NAICS code)
  
  
  output$Ellen_single_firm <- renderPlotly({
      
    if(!is.null(input$var_select) & !is.null(input$linenaics_2)){
      df_ellen <- df_1()
      
      df_ellen <- filter(df_ellen, tic %in% c(input$line_tic))
      
      df_ellen <- df_ellen[order(df_ellen$tic,df_ellen$fyear),]
      
      
      Ellen_single_firm <- plot_ly(
        data = df_ellen,
        x = ~ fyear,
        y = ~ get(input$scatter_2),
        color = ~ as.factor(tic),
        hoverinfo = 'text',
        text = ~ paste(
          "</br>NAICS:", NAICS,
          "</br>Variable Value:", round(get(input$scatter_2), digits = 5),
          "</br>Year:", fyear,
          "</br>TIC:", tic
        ),
        type = "scatter",
        mode = "lines + markers"
      )
      
      Ellen_single_firm <- Ellen_single_firm %>% layout(
        xaxis = list(title = 'Year'),
        yaxis = list(title = input$scatter_2),
        legend = list(title = list(text = '<b> NAICS </b>'))
      )
      
      Ellen_single_firm
      
    }
      
      
    else{
      Ellen_single_firm <- plotly_empty()
    }
  })
  
  
  
  
  
  #Data Output
  
  
  output$mydata = DT::renderDataTable({
    nu_comp_clean_q_sale <- nu_comp_clean_q_sale %>%
      mutate_if(is.numeric, round, digits = 7)
    
    DT::datatable(nu_comp_clean_q_sale[, input$show_vars, drop = FALSE],
                  options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nu_comp_clean_q_sale, file)
    }
  )
  
  #Regression output
  

  
  #-------------------------- the main named list that will be used in     other tasks
  listN <- reactiveValues()
  makeReactiveBinding("listN")
  #------Rendering the list to the ui
  output$uiAdded <- renderUI({
    checkboxGroupInput('added',
                       'List of combinations',
                       choices = names(listN))
    #multiple = TRUE,
    #selectize = FALSE)
  })
  #----------------------------------------------------------------
  
  observe({
    # Trigger Add actions
    input$actionBtnAdd
    isolate({
      new_selections <- c(input$makeNumInteract, input$makeFactInteract)
      new_selections_name <-
        new_selections %>% paste(collapse = "*")
      
      if (new_selections_name != "")
        listN[[new_selections_name]] <- new_selections
    })
  })
  
  
  
  #-----------------------First Variable To Select, at the top of the     sidebar--------
  output$dependent <- renderUI({
    selectInput("dependent", "Dependent Variable:", names(which(sapply(df_1(), is.numeric))))
  })
  #-------------------------Checkbox list of all numeric variables to use---------
  
  
  output$independent <- renderUI({
    checkboxGroupInput("independent",
                       "Independent (Predictor) Variables:",
                       unique(unlist(lapply(rbind(names(which(sapply(df_1(), is.factor))), names(which(sapply(df_1(), is.numeric)))[!names(which(sapply(df_1(), is.numeric))) %in% input$dependent]),as.character))))
                       
                      # na.omit(unique(stack(lapply(data.frame(names(which(sapply(df, is.factor))), 
                       #                                       names(which(sapply(df, is.numeric)))[!names(which(sapply(df, is.numeric))) %in% input$dependent]), as.character)))$values))
                       #names(which(sapply(df, is.numeric)))[!names(which(sapply(df, is.numeric))) %in% input$dependent],)
  })
    #-------------------Factor Variable to Add to the List of Combinations ---------
    output$makeFactInteract <- renderUI({
      selectInput("makeFactInteract",
                  "Factor Variable For Interaction:",
                  names(which(sapply(df_1(), is.factor))))
    })
    
    #---------------------Numerical Variable for List of Combinations-------------
    output$makeNumInteract <- renderUI({
      selectInput(
        "makeNumInteract",
        "Numeric Variable for Interaction:",
        names(which(sapply(df_1(), is.numeric)))[!names(which(sapply(df_1(), is.numeric))) %in% input$dependent],
        names(which(sapply(df_1(), is.numeric)))[!names(which(sapply(df_1(), is.numeric))) %in% input$dependent]
      )
    })
    #-----------This is the place to put in the listN objects....--------------
    
    
    runRegression <- reactive({
      if (!is.null(input$added)) {
        lm(as.formula(paste(
          input$dependent,
          " ~ ",
          paste(input$independent, collapse = "+"),
          paste("+", input$added, collapse = "+")
        )), data = df_1())
      } else{
        lm(as.formula(paste(
          input$dependent,
          " ~ ",
          paste(input$independent, collapse = "+")
        )), data = df_1())
      }
    })
    
    
    
    output$regTab <- renderUI({
      if (!is.null(input$independent)) {
        (HTML(
          stargazer(
            runRegression(),
            type = "html",
            title = "Panel Regression Summary",
            column.labels = c("Independent Variables"),
            dep.var.labels = input$dependent,
            dep.var.caption = "<em>Dependent:<em>"
          )
        ))
      } else{
        print(HTML("Warning: Please Select Model Parameters"))
      }
    })
    
    output$fittdPlot <- renderPlotly({
      df_fittd <- df_1()
      
      if (!is.null(input$independent)) {
        df <- merge(df_fittd, runRegression()$model)
        
        df <- unique(df)
        
        fittdPlot <-
          plot_ly(
            data = df,
            x = ~ fitted.values(runRegression()),
            y = ~ get(input$dependent),
            color = ~ as.factor(NAICS),
            size = ~ logsale,
            hoverinfo = 'text',
            text = ~ paste(
              "</br>TIC: ",
              tic,
              '</br>Predicted:',
              round(fitted.values(runRegression()), digits = 4),
              '</br>Actual:',
              round(get(input$dependent), digits = 4)
            ),
            type = "scatter",
            mode = "markers"
          )
        
        
        fittdPlot <- fittdPlot %>% layout(
          title = 'Fitted vs. Actual Values',
          xaxis = list(title = 'Fitted Values'),
          yaxis = list(title = input$dependent),
          legend = list(title = list(text = '<b> NAICS </b>'))
        )
        
        fittdPlot
      } else {
        fittdPlot <- plotly_empty()
      }
      
    })
    
    output$residualPlot <- renderPlotly({
      df_resid <- df_1()
      if (!is.null(input$independent)) {
        df <- merge(df_resid, runRegression()$model)
        
        df <- unique(df)
        
        residualPlot <-
          plot_ly(
            data = df,
            x = ~ fitted.values(runRegression()),
            y = ~ runRegression()$residuals,
            color = ~ as.factor(NAICS),
            size = ~ logsale,
            hoverinfo = 'text',
            text = ~ paste(
              "</br>TIC: ",
              tic,
              '</br>Predicted:',
              round(fitted.values(runRegression()), digits = 4),
              '</br>Actual:',
              round(get(input$dependent), digits = 4)
            ),
            type = "scatter",
            mode = "markers"
          )
        
        residualPlot <- residualPlot %>% layout(
          title = 'Residual Plot',
          xaxis = list(title = 'Fitted Values'),
          yaxis = list(title = 'Residuals'),
          legend = list(title = list(text = '<b> NAICS </b>'))
        )
        
        residualPlot
      } else {
        residualPlot <- plotly_empty()
      }
      
    })
    
    output$myresiduals <- DT::renderDataTable({
      df_resid <- df_1()
      df_resid[,"residuals"] <- NA
      
      if(!is.null(input$independent)){
        
      df_resid$residuals <- residuals(runRegression())
      
      df_resid <- mutate_if(df_resid, is.numeric, round, digits = 6)

      DT::datatable(df_resid[, input$show_vars_resid, drop = FALSE],
                    options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
      } else{
        df_resid_empty <- df_resid[FALSE,]
        
        DT::datatable(df_resid_empty[, input$show_vars_resid, drop = FALSE],
                      options = list(lengthMenu = c(10, 25, 50), pageLength = 10))      }
    })
    
   df_resid <- reactive({
     df_resid <- df_1()
     df_rsid <- mutate(df_resid, residuals = residuals(runRegression()))

   })
    
    
    output$downloadData_resid <- downloadHandler(
      
      filename = function() {
        paste("data_sub_resid-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df_resid(), file)
      }
    )
    
}

shinyApp(ui = ui, server = server)
