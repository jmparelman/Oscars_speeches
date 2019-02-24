library(dplyr)

speeches <- read.csv("https://raw.githubusercontent.com/jmparelman/Oscars_speeches/master/Speeches.csv",
                     stringsAsFactors = FALSE)

speeches <- speeches[, 2:6]

speeches <- speeches %>%
  filter(Category != "") %>%
  mutate(cat_2 = case_when(
    Category == " Sound" ~ "Sound",
    Category == " Sound Recording" ~ "Sound Recording",
    Category == " Sound Effects" ~ "Sound Effects",
    Category == " Sound Editing" ~ "Sound Editing",
    Category == " Sound Mixing" ~ "Sound Mixing",
    Category == " Best Motion Picture" ~ "Best Picture",
    Category == " Best Picture" ~ "Best Picture",
    Category == " Writing (Original Screenplay)" ~ "Writing (Original Screenplay)",
    Category == " Writing (Original Story)" ~ "Writing (Original Screenplay)",
    Category == " Writing (Screenplay)" ~ "Writing (Screenplay)",
    Category == " Writing (Screenplay--based on material from another medium)" ~ "Writing (Adapted Screenplay)",
    Category == " Writing (Story and Screenplay--written directly for the screen)" ~ "Writing (Original Screenplay)",
    Category == " Writing (Screenplay Adapted from Other Material)" ~ "Writing (Adapted Screenplay)",
    Category == " Writing (Screenplay Written Directly for the Screen--based on factual material or on story material not previously published or produced)" ~ "Writing (Original Screenplay)",
    Category == " Writing (Screenplay Based on Material from Another Medium)" ~ "Writing (Adapted Screenplay)",
    Category == " Writing (Screenplay Written Directly for the Screen)" ~ "Writing (Original Screenplay)",
    Category == " Writing (Adapted Screenplay)" ~ "Writing (Adapted Screenplay)",
    Category == " Special Effects" ~ "Visual Effects",
    Category == " Special Visual Effects" ~ "Visual Effects",
    Category == " Visual Effects" ~ "Visual Effects",
    Category == " Actor" ~ "Leading Actor",
    Category == " Actor in a Leading Role" ~ "Leading Actor",
    Category == " Actor in a Supporting Role" ~ "Supporting Actor",
    Category == " Film Editing" ~ "Film Editing",
    Category == " Foreign Language Film" ~ "Foreign Language Film",
    Category == " Actress" ~ "Leading Actress",
    Category == " Actress in a Leading Role" ~ "Leading Actress",
    Category == " Actress in a Supporting Role" ~ "Supporting Actress",
    Category == " Costume Design" ~ "Costume Design",
    Category == " Cinematography" ~ "Cinematography",
    Category == " Documentary (Feature)" ~ "Documentary (Feature)",
    Category == " Documentary (Short Subject)" ~ "Documentary (Short)",
    Category == " Directing" ~ "Director",
    Category == " Music (Music Score of a Dramatic or Comedy Picture)" ~ "Music (Best Score)",
    Category == " Music (Scoring of a Musical Picture)" ~ "Music (Musical Score)",
    Category == " Music (Song)" ~ "Music (Song)",
    Category == " Music (Music Score--substantially original)" ~ "Music (Best Score)",
    Category == " Music (Scoring of Music--adaptation or treatment)" ~ "Music (Musical Score)",
    Category == " Music (Original Music Score)" ~ "Music (Best Score)",
    Category == " Music (Original Score--for a motion picture [not a musical])" ~ "Music (Best Score)",
    Category == " Music (Song--Original for the Picture)" ~ "Music (Song)",
    Category == " Music (Original Score)" ~ "Music (Best Score)",
    Category == " Music (Original Song Score)" ~ "Music (Musical Score)",
    Category == " Music (Original Dramatic Score)" ~ "Music (Best Score)",
    Category == " Music (Scoring: Adaptation and Original Song Score)" ~ "Music (Musical Score)",
    Category == " Music (Scoring: Original Song Score and Adaptation -or- Scoring: Adaptation)" ~ "Music (Musical Score)",
    Category == " Music (Original Song)" ~ "Music (Song)",
    Category == " Makeup" ~ "Makeup",
    Category == " Makeup and Hairstyling" ~ "Makeup"
  ))

acting <- c("Leading Actor", "Supporting Actor", "Leading Actress", "Supporting Actress")
technical <- c("Sound", "Sound Recording", "Sound Effects", "Sound Editing", "Sound Mixing",
               "Visual Effects", "Film Editing", "Costume Design", "Cinematography", "Makeup")
music <- c("Music (Best Score)", "Music (Musical Score)", "Music (Song)")
writing <- c("Writing (Original Screenplay)", "Writing (Adapted Screenplay)", "Writing (Screenplay)")
directing <- "Director"
production <- c("Best Picture", "Foreign Language Film", "Documentary (Feature)", "Documentary (Short)")

speeches <- speeches %>%
  mutate(cat_3 = case_when(cat_2 %in% acting ~ "acting",
                           cat_2 %in% technical ~ "technical",
                           cat_2 %in% music ~ "music",
                           cat_2 %in% writing ~ "writing",
                           cat_2 %in% directing ~ "directing",
                           cat_2 %in% production ~ "production"))

speeches$year_2 <- NA

for (i in 1:nrow(speeches)) {
  speeches$year_2[i] <- str_split(speeches$Year[i], " ")[[1]][2]
}

speeches$year_2 <- as.numeric(speeches$year_2)
