news_tree <-
  list(
    root = list(
      chance = 1,
      no_tournament_selected = list(
        chance = "calculated",
        chance_fun = "length_gt",
        chance_params = list("selected_tournament", 0, 0)
      ),
      tournament_selected = list(
        chance = "calculated",
        chance_fun = "length_gt",
        chance_params = list("selected_tournament", 0, 1),
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

jsonlite::toJSON(news_tree, auto_unbox = TRUE) |>
  writeLines("inst/foosball3/www/data/newstree.json")
