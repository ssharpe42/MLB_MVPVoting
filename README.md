# MLB_MVPVoting
Examining how MLB MVP voters think and predicting the MVP with the fused lasso and linear optimization

ScrapeBRef.R
---
Scrape all MVP votes from Baseball Reference.

Aggregate.R
---
Aggregate all stats and data associated with each player in MVP vote.

MVP_FusedLassoLP.R
---
A linear programming implementation of a fussed lasso. The LP learns weights to assign to specific player stats in order to correctly order players from most votes (MVP winner) to least in each year and league. 

