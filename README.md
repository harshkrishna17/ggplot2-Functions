# ggplot2-Functions
A repository to hold code for plotting functions that make creating vizzes using ggplot2 easy  

## xG Race Chart 

Create an xG race chart using data from Understat. [Code to create function](https://github.com/harshkrishna17/ggplot2-Functions/blob/main/xG%20Race%20Chart.R)

To Create, extract the data, run the function, and run the following line (setting specific parameters) for the final viz.

```
plot_xgrace(data = data, team_home = "Manchester United", team_away = "Liverpool",
            home_color = "#e31a1c", away_color = "#980043", theme = "dark")
```            

![xgr](https://user-images.githubusercontent.com/87293901/138739917-c77a34b6-e2ed-49f3-9f84-481b49d47b0a.png)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## xG Trendline

Plot an xG vs xGA trendline using FBref(via StatsBomb data). [Function code](https://github.com/harshkrishna17/ggplot2-Functions/blob/main/xG%20Trendline.R)

To Create, extract the data, run the function, and run the following line (setting specific parameters) for the final viz.

```
plot_trendline(data = data, team = "RB Leipzig",
               colour_xg = "#08519c", colour_xga = "#cb181d",
               roll_avg = 10, theme = "dark")
```

![gg](https://user-images.githubusercontent.com/87293901/138740112-26195e9c-866d-494f-bfb7-de7b44c5e5c4.png)
