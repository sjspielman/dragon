library(shiny)
library(tidyverse)
library(colourpicker)


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title
  titlePanel("Exploring Mineral Chemistry Networks"),
    helpText("Written by Stephanie J. Spielman.", (a("Source code and instructions",
        href="https://github.com/spielmanlab/shinymineral"))),
        
  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
    
    # Element of interest?
    #textInput("element_of_interest","Enter the 1-2 letter symbol for the element you'd like to analyze",value="O"),
    
    selectInput("element_of_interest", "Select the element you'd like to analyze",
            c("Ac" = "Ac", "Ag" = "Ag", "Al" = "Al", "Am" = "Am", "Ar" = "Ar", "As" = "As", "At" = "At", "Au" = "Au", "B" = "B", "Ba" = "Ba", "Be" = "Be", "Bh" = "Bh", "Bi" = "Bi", "Bk" = "Bk", "Br" = "Br", "C" = "C", "Ca" = "Ca", "Cd" = "Cd", "Ce" = "Ce", "Cf" = "Cf", "Cl" = "Cl", "Cm" = "Cm", "Cn" = "Cn", "Co" = "Co", "Cr" = "Cr", "Cs" = "Cs", "Cu" = "Cu", "Db" = "Db", "Ds" = "Ds", "Dy" = "Dy", "Er" = "Er", "Es" = "Es", "Eu" = "Eu", "F" = "F", "Fe" = "Fe", "Fl" = "Fl", "Fm" = "Fm", "Fr" = "Fr", "Ga" = "Ga", "Gd" = "Gd", "Ge" = "Ge", "H" = "H", "He" = "He", "Hf" = "Hf", "Hg" = "Hg", "Ho" = "Ho", "Hs" = "Hs", "I" = "I", "In" = "In", "Ir" = "Ir", "K" = "K", "Kr" = "Kr", "La" = "La", "Li" = "Li", "Lr" = "Lr", "Lu" = "Lu", "Lv" = "Lv", "Mc" = "Mc", "Md" = "Md", "Mg" = "Mg", "Mn" = "Mn", "Mo" = "Mo", "Mt" = "Mt", "N" = "N", "Na" = "Na", "Nb" = "Nb", "Nd" = "Nd", "Ne" = "Ne", "Nh" = "Nh", "Ni" = "Ni", "No" = "No", "Np" = "Np", "O" = "O", "Og" = "Og", "Os" = "Os", "P" = "P", "Pa" = "Pa", "Pb" = "Pb", "Pd" = "Pd", "Pm" = "Pm", "Po" = "Po", "Pr" = "Pr", "Pt" = "Pt", "Pu" = "Pu", "Ra" = "Ra", "Rb" = "Rb", "Re" = "Re", "Rf" = "Rf", "Rg" = "Rg", "Rh" = "Rh", "Rn" = "Rn", "Ru" = "Ru", "S" = "S", "Sb" = "Sb", "Sc" = "Sc", "Se" = "Se", "Sg" = "Sg", "Si" = "Si", "Sm" = "Sm", "Sn" = "Sn", "Sr" = "Sr", "Ta" = "Ta", "Tb" = "Tb", "Tc" = "Tc", "Te" = "Te", "Th" = "Th", "Ti" = "Ti", "Tl" = "Tl", "Tm" = "Tm", "Ts" = "Ts", "U" = "U", "V" = "V", "W" = "W", "Xe" = "Xe", "Y" = "Y", "Yb" = "Yb", "Zn" = "Zn", "Zr" = "Zr")
    ),
    
    selectInput("age", "Choose a starting eon for earliest minerals to include:",
          c("Hadean (> 4 Ga)" = "hadean",
               "Archean (> 2.5 Ga)" = "archaen",
               "Paleoproteozoic (> 1.6 Ga)" = "paleo",
               "Present Day" = "present")
        ),
    br(),
    br(),
    h3("Node options"),
        
    selectInput("colornode", "Color node by?",
           c("Default igraph colors"    = "default",
             "Mean mineral redox state" = "redox",
             "Maximum mineral age"      = "maxage",
             "Degree"                   = "degree",
             "Mineral or element"       = "type", 
             "Do not color nodes"       = "nocolor")
            ),
    
    ### Element color should always be selected
    {colourInput("elementcolor", "Select element color:", value = "seagreen")},
    
    ##################### Node colors by type ###################
    conditionalPanel(condition = "output.colornodetype",   
        {colourInput("mineralcolor", "Select mineral color:", value = "seagreen")}
    ),


    ##################### Node color scale ###################
    selectInput("nodecolorscale", "Choose the node color palette:",
          c("Hadean (> 4 Ga)" = "hadean",
               "Archean (> 2.5 Ga)" = "archaen",
               "Paleoproteozoic (> 1.6 Ga)" = "paleo",
               "Present Day" = "present")
        ),


    selectInput("sizenode", "Size node by?",
              c("Default igraph size"      = "default",
                "Mean mineral redox state" = "redox",
                "Maximum mineral age"      = "maxage",
                "Degree"                   = "degree",
                "Mineral or element"       = "type", 
                "Do not color nodes"       = "nocolor")
               ),


    br(),
    br(),
    h3("Edge options"),
    
    selectInput("coloredge", "Color edge by?",
           c("Default edge color (all black)" = "black",
             "Element redox state" = "redox")
        ),

    #################### Edge color scale ###################
    # TODO
        
    br(),br(),
    actionButton("go","Build Network",width="100%")),   
    
    
    ## To Do: Export network file
    ## Display network
    
    # Main panel for displaying outputs
    mainPanel(
        #verbatimTextOutput("summary") #,
      
        #div(style = "height: 500px;",
        #    plotOutput("mahplot", height = "100%")
        #),
    #
    #    uiOutput("download")

    )
  )
)
