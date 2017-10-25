# shinyapps

> Some Shiny applications


## C'est la rentrée !

An application to explore the name of public schools in France. The names are represented by the gender, activity, and century of the personality by which establishments were named. More frequent names can be visualized, and you can access to the biography of a personality. All application is in French.

**Packages** : The application use the following packages : [`shiny`](https://shiny.rstudio.com/), [`shinyWidgets`](https://github.com/dreamRs/shinyWidgets), [`billboarder`](https://github.com/dreamRs/billboarder). To see which versions of these packages are used (and dependancies), look at the file `session_info.txt` in app directory. If packages are not installed, they will be on application launch.

**Data sources** : Names of shcools are from [data.gouv](https://www.data.gouv.fr/fr/datasets/etablissements-scolaires/), data about personality are from Wikipedia and DBpedia.

Launch application : 

```r
shiny::runGitHub(repo = "dreamRs/shinyapps", subdir = "rentree_scolaire")
```

![](rentree_scolaire/www/screenshot.png)

