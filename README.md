# ggplot2-Functions
A repository to hold code for plotting functions that make creating vizzes using ggplot2 easy  

## xG Race Chart 

Create an xG race chart using data from Understat. [Code to create function](https://github.com/harshkrishna17/ggplot2-Functions/blob/main/xG%20Race%20Chart.R)

To Create, extract the data, run the function, and run the following line (setting specific parameters) for the final viz.

```R
plot_xgrace(data = data, team_home = "Manchester United", team_away = "Liverpool",
            home_color = "#e31a1c", away_color = "#980043", theme = "dark")
```            

![xgr](https://user-images.githubusercontent.com/87293901/138739917-c77a34b6-e2ed-49f3-9f84-481b49d47b0a.png)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Finishing Variance Line

Plot a Linechart that shows how a players finishing trends over time using Understat data. [Code here.](https://github.com/harshkrishna17/ggplot2-Functions/blob/main/Finishing%20Variance%20Line.R)

To Create, load the packages, run the function, and run the following line (setting specific parameters) for the final viz.

```R
plot_varline(player_id = 5555, roll_avg = 50, theme = "dark")
```

![Rplot274](https://user-images.githubusercontent.com/87293901/138848925-0a421e5f-8689-4354-af36-7bbc1fd64c56.png)
