# AFCON 2023 - Ivory Coast Dashboard

An interactive dashboard exploring how Ivory Coast won AFCON 2023 despite changing their manager after the group stages. It also analyses who deserved to win.

**Live dashboard:** <https://YOUR-USERNAME.github.io/nba-analytics/>

Built with R, Shiny, and `bslib`, published to GitHub Pages via [Shinylive](https://shinylive.io/r/) — the app runs entirely in the visitor's browser, with no server.

## Data

Shot-level data pulled from the NBA Stats API via the [`hoopR`](https://hoopr.sportsdataverse.org/) package.

Football Event data pulled from StatsbombR via devtools::install_github("statsbomb/StatsBombR") package

## Repository layout

-   `app/` — the Shiny app (`app.R` and `NEWAFCONDATA2.RData`)
-   `docs/` — the Shinylive build served by GitHub Pages
-   `data/` — raw and cleaned data pulled from the API
