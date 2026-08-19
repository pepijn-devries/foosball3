news_tree <-
  list(
    root = list(
      chance = 1,
      no_tournament_selected = list(
        chance = "calculated",
        chance_param = "selected_tournament"
      ),
      tournament_selected = list(
        chance = "calculated",
        chance_param = "selected_tournament",
        tournament_news = list(
          chance = 0.4
        ),
        match_news = list(
          chance = 0.2
        ),
        person_news = list(
          chance = 0.4
        )
      )
    )
  )

jsonlite::toJSON(news_tree) |>
  writeLines("inst/foosball3/www/newstree.json")
