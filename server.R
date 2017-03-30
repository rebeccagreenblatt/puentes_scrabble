library(shiny)
library(ggplot2)
library(googlesheets)
library(dplyr)
library(reshape2)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

scrabble_sheet = gs_url("https://docs.google.com/spreadsheets/d/19MpxNYKjgVJukvBdgCDWrBhEgvvNH6S4FinO2g207RQ/pubhtml#")
teams = gs_read(scrabble_sheet, ws=1)
scores = gs_read(scrabble_sheet, ws=2)

colnames(teams) <- c("Student", "Team")
teams$Student <- sapply(teams$Student, function(i) simpleCap(i))
teams$Team <- sapply(teams$Team, function(i) simpleCap(gsub("_", " ", i)))

scores_long <- melt(scores)
colnames(scores_long) <- c("date", "team", "score")
#team_names <- unique(scores_long$team)
scores_long$team <- as.character(scores_long$team)
scores_long$team <- sapply(scores_long$team, function(i){
  simpleCap(gsub("_", " ", i))})
scores_long_cumulative <- scores_long %>% group_by(team) %>% mutate(total = cumsum(score))
scores_long_cumulative$date <- factor(scores_long_cumulative$date, levels = as.character(scores$X))

ranking <- scores_long_cumulative %>% arrange(desc(total)) %>% group_by(team) %>%
  filter(row_number() == 1) %>% ungroup() %>% mutate(Points = total, Team = team) %>% 
  select(Team, Points) %>% arrange(desc(Points))

Members <- ave(teams$Student, teams$Team, FUN = function(x) paste0(x, collapse=", "))
teams <- cbind(teams, Members)
team_members <- teams %>% group_by(Team) %>%
  filter(row_number() == 1) %>%
  select(Members)

ranking_final <- inner_join(ranking, team_members, by=c("Team")) %>%
  select(Team, Members, Points)

shinyServer(function(input, output, session) {
    
  output$plot1 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(scores_long_cumulative, aes(x=date, y=total, color=as.factor(team))) + 
      geom_point() + 
      geom_line(aes(group=as.factor(team))) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x="Date of Game", y="Total Points", colour = "Team")   
  })
  
  output$tab1 <- renderTable({
    #students <- teams %>% filter(team_name == tolower(gsub(" ", "_", input$team))) %>% select(student_name)
    #simpleCap(paste(students$student_name, collapse=", "))
    ranking_final
  })
  
})
