pageWithSidebar(
  headerPanel('Puentes 4th Grade Scrabble Tournament'),
  sidebarPanel(
    tableOutput('tab1')),
   # selectInput('team', 'Team Members', c(" ", team_names)),
    #h5(textOutput('text'), align='center', style = 'color:blue')),
  mainPanel(
    plotOutput('plot1')
  )
)