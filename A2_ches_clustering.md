CHES Clustering
================
Rebecca & Simon

Packages
--------

``` r
pacman::p_load(dplyr, ggplot2, readr, haven, broom, purrr, tidyr, magrittr, labelled, sjPlot, viridis, forcats, ggthemes, cluster, factoextra, fpc)
```

Data
----

``` r
ches <- get(load("data/Rdata/ches_final.Rdata"))
```

Indeces
-------

``` r
normalize_range <- function(x){(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))}
ches <- ches %>% 
  mutate(populism = antielite_salience + corrupt_salience) %>% 
  mutate(populism2 = normalize_range(normalize_range(antielite_salience) + 
                               (1 - normalize_range(eu_position)))*100) %>% #+ 
                           #    (1 - range01(eu_budgets)))*100) %>% 
  mutate(liberalism = sociallifestyle + civlib_laworder + galtan) %>% 
  mutate(populism = normalize_range(populism)*100) %>% 
  mutate(liberalism = normalize_range(liberalism)*100) #%>% 
  #filter(year > 2009)
```

Horse-shoe
==========

``` r
ches %>% 
  ggplot(aes(liberalism, populism, colour = eu_position)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))+
  #geom_text_repel(aes(liberalism, populism, label = party_cntry)) +
  ggthemes::theme_hc() +
  viridis::scale_color_viridis()
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-4-1.png)

Clustering
----------

``` r
set.seed(2018)
ches_cluster_data <- ches %>% 
  select(party_name, vote_id, liberalism, populism2) %>% 
  drop_na(liberalism, populism2) %>% 
  as.data.frame()

ches_cluster <- ches_cluster_data %>% 
  select(-party_name, -vote_id) %>% 
  purrr::map_df(scale) 
```

``` r
distance <- get_dist(ches_cluster)
fviz_dist(distance, 
 gradient = list(low = "#00AFBB", 
                 mid = "white",
                 high = "#FC4E07"))
```

`kmeans()` function returns a list of components, including:

-   `cluster`: A vector of integers (from 1:k) indicating the cluster to which each point is allocated
-   `centers`: A matrix of cluster centers (clustqer means)
-   `totss`: The total sum of squares (TSS), i.e (xi ≠ x ̄)2. TSS measures the total variance in the data.
-   `withinss`: Vector of within-cluster sum of squares, one component per cluster
-   `tot.withinss`: Total within-cluster sum of squares, i.e. sum(withinss)
-   `betweenss`: The between-cluster sum of squares, i.e. totss ≠ tot.withinss
-   `size`: The number of observations in each cluster

``` r
k3 <- kmeans(ches_cluster, centers = 3, nstart = 25, iter.max = 10)
ggf <- fviz_cluster(k3, data = ches_cluster, show.clust.cent = T, text = "vote_id")
ggf + theme_gdocs()
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ches_cluster_data$k3 <- k3$cluster

ches_clust <- ches %>%
  left_join(ches_cluster_data)
```

    ## Joining, by = c("party_name", "vote_id", "populism2", "liberalism")

    ## Warning: Column `party_name` has different attributes on LHS and RHS of
    ## join

    ## Warning: Column `populism2` has different attributes on LHS and RHS of join

    ## Warning: Column `liberalism` has different attributes on LHS and RHS of
    ## join

``` r
# save(ches_clust, file = "data/Rdata/ches_clust.Rdata")
```

``` r
fviz_cluster(
  k3, 
  data = ches_cluster,
  palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
  ellipse.type = "euclid", # Concentration ellipse star.plot = TRUE, # Add segments from centroids to items repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal()
)
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# Elbow method
fviz_nbclust(ches_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method") + 
  theme_gdocs()
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# Silhouette method
fviz_nbclust(ches_cluster, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") +
  theme_gdocs()
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-10-2.png)

``` r
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
fviz_nbclust(ches_cluster, kmeans, nstart = 25, method = "gap_stat", nboot = 50) + 
  labs(subtitle = "Gap statistic method") +
  theme_gdocs()
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-10-3.png)

According to these observations, it’s possible to define k = 4 as the optimal number of clusters in the data.

2 dimensions do not need pca

``` r
library(purrr)
res <- purrr::map(2:8, ~ kmeans(ches_cluster, .))
library(ggfortify)
autoplot(res, data = ches_cluster, ncol = 3) + theme(legend.position = "none")
```

normal scatterplots

``` r
k_cluster_dat <- 2:8 %>%
  purrr::map(~ kmeans(ches_cluster, .x)$cluster) %>%
  reduce(cbind) %>%
  as_tibble() %>%
  set_names(paste0("k", 2:8)) %>%
  cbind(ches_cluster, .)

k_cluster_dat %>%
  gather("k", "value", -liberalism, -populism2) %>%
  ggplot(aes(liberalism, populism2, colour = as.factor(value))) +
  geom_point() +
  facet_wrap(~k) +
  scale_colour_viridis(discrete = T, direction = -1) + 
  theme_hc() +
  theme(legend.position = "none")
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
cbind(ches_cluster, cluster = k3$cluster) %>%
  group_by(cluster) %>%
  summarise_all(.funs = list(m = mean, s = sd))
```

    ## # A tibble: 3 x 5
    ##   cluster liberalism_m populism2_m liberalism_s populism2_s
    ##     <int>        <dbl>       <dbl>        <dbl>       <dbl>
    ## 1       1      -1.09         0.437        0.418       0.770
    ## 2       2       1.26         1.12         0.411       0.676
    ## 3       3      -0.0298      -0.784        0.656       0.396

K-Medoids
---------

The most common k-medoids clustering methods is the PAM algorithm (Partitioning Around Medoids, Kaufman & Rousseeuw, 1990).

``` r
res_pam <- pam(ches_cluster, 3, metric = "euclidean", stand = FALSE)
res_pam$clustering
```

    ##   [1] 1 1 1 1 2 2 2 2 2 1 3 1 3 2 1 2 1 2 1 3 1 1 2 2 2 1 1 2 3 3 1 1 2 2 1
    ##  [36] 3 3 3 1 1 3 1 2 1 2 2 2 1 1 2 1 1 1 1 1 1 1 1 1 2 3 3 2 2 2 2 1 1 2 2
    ##  [71] 2 1 1 1 1 1 3 2 3 2 1 1 2 3 3 2 2 2 2 1 2 1 1 3 1 2 3 1 1 2 1 1 1 1 1
    ## [106] 3 1 2 2 2 1 3 2 2 3 1 1 3 3 1 2 2 1 3 1 1 2 1 1 1 2 2 2 1 3 1 1 2 2 2
    ## [141] 3 3 2 2 1 3 3 2 2 2 3 2 1 2 1 3 3 2 2 2 1 1 1 2 3 3 1 1 1 3 2 2 3 2 3
    ## [176] 3 2 2 2 3 3 2 1 3 1 2 3 2 1 3 3 3 2 2 2 2 2 2 3 2 2 2 2 2 2 3 1 2 3 3
    ## [211] 2 2 1 2 2 2 2 1 1 1 2 1 2 1 1 1 3 3 1 3 1 3 1 3 1 2 3 2 1 3 3 1 1 3 1
    ## [246] 2 2 1 1 2 3 3 2 2 1 2 2 1 1 1 3 1 2 2 1 2 1 1

``` r
fviz_nbclust(ches_cluster, pam, method = "silhouette")+
theme_hc()
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
km_cluster_dat <- 2:8 %>%
  purrr::map(~ pam(ches_cluster, k = ., metric = "euclidean", stand = FALSE)$clustering) %>%
  reduce(cbind) %>%
  as_tibble() %>%
  set_names(paste0("k", 2:8)) %>%
  cbind(ches_cluster, .)

gg_km <- km_cluster_dat %>%
  gather("k", "value", -liberalism, -populism2) %>%
  ggplot(aes(liberalism, populism2, colour = as.factor(value))) +
  geom_point() +
  facet_wrap(~k) +
  scale_colour_viridis(discrete = T, direction = -1) + 
  theme_hc() +
  theme(legend.position = "none")
gg_km
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
res_pam$medoids
```

    ##      liberalism  populism2
    ## [1,] -0.9858867 -0.2065320
    ## [2,]  0.2614908 -0.7920694
    ## [3,]  1.3112089  1.2857607

``` r
cbind(ches_cluster, cluster = res_pam$clustering) %>%
  group_by(cluster) %>%
  summarise_all(.funs = list(m = median))
```

    ## # A tibble: 3 x 3
    ##   cluster liberalism_m populism2_m
    ##     <int>        <dbl>       <dbl>
    ## 1       1       -1.07       -0.207
    ## 2       2        0.299      -0.792
    ## 3       3        1.34        1.29

``` r
fviz_nbclust(ches_cluster, clara, method = "silhouette")+
theme_classic()
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-18-1.png)

HCA
===

``` r
res.dist <- dist(ches_cluster, method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
# Cut tree into 3 groups
grp <- cutree(res.hc, k = 3)

fviz_dend(
  res.hc, 
  k = 3, # Cut in four groups
  cex = 0.5, # label size
  k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
  color_labels_by_k = TRUE, # color labels by groups
  rect = TRUE # Add rectangle around groups
)
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
fviz_dend(
  res.hc, 
  cex = 1, 
  k = 3,
  k_colors = "jco", 
  type = "circular"
)
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
require("igraph")
```

    ## Loading required package: igraph

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
ggrep <- fviz_dend(res.hc, k = 3, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggrep
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
fviz_dend(res.hc, k = 3, # Cut in four groups
          k_colors = "jco",
          type = "phylogenic", 
          repel = TRUE,
          phylo_layout = "layout_with_drl")
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
fviz_dend(res.hc, k = 3, # Cut in four groups
          k_colors = "jco",
          type = "phylogenic", 
          repel = TRUE,
          phylo_layout = "layout_as_tree")
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
fviz_dend(res.hc, k = 3, # Cut in four groups
          k_colors = "jco",
          type = "phylogenic", 
          repel = TRUE,
          phylo_layout = "layout.gem")
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
fviz_dend(res.hc, k = 3, # Cut in four groups
          k_colors = "jco",
          type = "phylogenic", 
          repel = TRUE,
          phylo_layout = "layout.mds")
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
gg10 <- fviz_dend(res.hc, k = 3, # Cut in four groups
          k_colors = "jco",
          type = "phylogenic", 
          repel = TRUE,
          phylo_layout = "layout_with_lgl")
gg10
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-27-1.png)

Compare clustering algorithms in R
----------------------------------

``` r
library(clValid)
```

    ## 
    ## Attaching package: 'clValid'

    ## The following object is masked from 'package:igraph':
    ## 
    ##     clusters

``` r
# Iris data set:
# - Remove Species column and scale df <- scale(iris[, -5])
# Compute clValid
clmethods <- c("hierarchical","kmeans","pam") 
intern <- clValid(
  ches_cluster %>% as.matrix, 
  nClust = 2:8,
  clMethods = clmethods, 
  validation = "internal"
) 
```

    ## Warning in clValid(ches_cluster %>% as.matrix, nClust = 2:8, clMethods =
    ## clmethods, : rownames for data not specified, using 1:nrow(data)

``` r
summary(intern)
```

    ## 
    ## Clustering Methods:
    ##  hierarchical kmeans pam 
    ## 
    ## Cluster sizes:
    ##  2 3 4 5 6 7 8 
    ## 
    ## Validation Measures:
    ##                                  2       3       4       5       6       7       8
    ##                                                                                   
    ## hierarchical Connectivity  10.2683 17.5187 24.7937 35.0802 42.4873 45.0024 54.0444
    ##              Dunn           0.0489  0.0569  0.0586  0.0678  0.0830  0.0886  0.0939
    ##              Silhouette     0.4421  0.3938  0.4172  0.3977  0.3853  0.3710  0.3428
    ## kmeans       Connectivity  27.6107 28.8794 42.6972 48.6603 59.4067 59.1024 80.4679
    ##              Dunn           0.0336  0.0253  0.0327  0.0478  0.0201  0.0357  0.0419
    ##              Silhouette     0.4495  0.4394  0.4634  0.4296  0.3995  0.4060  0.3596
    ## pam          Connectivity  26.5952 39.1357 39.8861 38.1683 67.1135 73.0849 78.7849
    ##              Dunn           0.0357  0.0331  0.0306  0.0334  0.0264  0.0243  0.0254
    ##              Silhouette     0.4486  0.4179  0.4614  0.4267  0.4014  0.4006  0.3771
    ## 
    ## Optimal Scores:
    ## 
    ##              Score   Method       Clusters
    ## Connectivity 10.2683 hierarchical 2       
    ## Dunn          0.0939 hierarchical 8       
    ## Silhouette    0.4634 kmeans       4

``` r
# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(
  ches_cluster %>% as.matrix,
  nClust = 2:6, 
  clMethods = clmethods,
  validation = "stability"
) # Display only optimal Scores
optimalScores(stab)
```

Model-Based Clustering
----------------------

The model parameters can be estimated using the Expectation-Maximization (EM) algorithm initialized by hierarchical model-based clustering. Each cluster k is centered at the means μk, with increased density for points near the mean.

``` r
library(mclust)
```

    ## Warning: package 'mclust' was built under R version 3.4.3

    ## Package 'mclust' version 5.4
    ## Type 'citation("mclust")' for citing this R package in publications.

    ## 
    ## Attaching package: 'mclust'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
mc <- Mclust(ches_cluster) # Model-based-clustering 
summary(mc)
```

    ## ----------------------------------------------------
    ## Gaussian finite mixture model fitted by EM algorithm 
    ## ----------------------------------------------------
    ## 
    ## Mclust EII (spherical, equal volume) model with 4 components:
    ## 
    ##  log.likelihood   n df       BIC       ICL
    ##       -655.0623 268 12 -1377.216 -1431.308
    ## 
    ## Clustering table:
    ##  1  2  3  4 
    ## 91 88 53 36

``` r
# BIC values used for choosing the number of clusters 
fviz_mclust(mc, "BIC", palette = "jco")
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
# Classification: plot showing the clustering 
fviz_mclust(mc, "classification", geom = "point",
pointsize = 1.5, palette = "jco") # Classification uncertainty
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-31-2.png)

``` r
fviz_mclust(mc, "uncertainty", palette = "jco")
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-31-3.png)

``` r
ches %>% 
  ggplot(aes(liberalism, populism, colour = mc$classification)) + 
  geom_point() +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2))+
  #geom_text_repel(aes(liberalism, populism, label = party_cntry)) +
  ggthemes::theme_hc() +
  viridis::scale_color_viridis() + 
  geom_density2d(alpha = .7, color = "gray") # Add 2D density 
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
library(highcharter)

df1 <- tibble(vote = paste(ches_cluster_data$party_name, ches_cluster_data$vote_id, sep = " "), cluster = k3$cluster, x = as.vector(ches_cluster$liberalism), y = as.vector(ches_cluster$populism2)) %>%
  filter(stringr::str_detect(vote, "DE_"))
hchart(df1, hcaes(x = x, y = y, name = vote, color = cluster), type = 'scatter') %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_tooltip(
    formatter = JS("function(){
                    return ('Party: <strong>' + this.point.vote + '</strong><br> X: ' + this.x + ' <br> Y: ' + this.y + ' <br>')
                  }")) %>%
  hc_chart(zoomType = "xy")
```

World Map
---------

``` r
library(ggplot2)
world <- map_data("world")
```

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:mclust':
    ## 
    ##     map

    ## The following object is masked from 'package:cluster':
    ## 
    ##     votes.repub

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
world$iso3 <- countrycode::countrycode(world$region, "country.name", "iso3c")
```

    ## Warning in countrycode::countrycode(world$region, "country.name", "iso3c"): Some values were not matched unambiguously: Ascension Island, Azores, Barbuda, Bonaire, Canary Islands, Chagos Archipelago, Grenadines, Heard Island, Kosovo, Madeira Islands, Micronesia, Saba, Saint Martin, Siachen Glacier, Sint Eustatius, Virgin Islands

``` r
ches <- ches %>%
  mutate(country = stringr::str_replace(vote_id, "_.*?$", "")) %>%
  mutate(iso3 = countrycode::countrycode(country, "iso2c", "iso3c"))
world$value <- ifelse(world$iso3 %in% unique(ches$iso3), "yes", "no")
# table(world$value)
# world %>% 
#   ggplot(aes(long, lat, group = group)) + 
#     geom_polygon(fill='grey')

ggmap <- world %>% 
  ggplot(aes(long, lat, group = group, fill = value)) + 
  geom_polygon() +
  #xlim(-20,50) + 
  #ylim(30,80) +
  scale_fill_manual("selected", values = c("gray90", "blue")) +
  theme_map()
ggmap
```

![](A2_ches_clustering_files/figure-markdown_github/unnamed-chunk-34-1.png)

``` r
#ess_clean <- ess_sub  %>% 
  # mutate(eu_member =
  #          recode_factor(cntry,
  #               DE = 1958, BE = 1958, FR = 1958, NL = 1958, IE = 1973,
  #               GB = 1973, FI = 1995, AT = 1995, SE = 1995, EE = 2004,
  #               PL = 2004, SI = 2004, CZ = 2004, CH = 0, IL = 0,
  #               IS = 0, NO = 0, RU = 0
  #             )
  #       ) %>%
  # mutate(post_com = ifelse(region %in% c("Estonia", "Poland", "Slovenia", "Czech Republic", "Russian Federation"), "Post C", "West"))
```
