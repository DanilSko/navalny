AA: Navalny
================
Artjoms Šeļa
2023-08-17

- <a href="#tldr-so-far" id="toc-tldr-so-far">tl;dr (so far)</a>
- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#data-wrangling--book-keeping"
  id="toc-data-wrangling--book-keeping">Data wrangling &amp; book
  keeping</a>
  - <a href="#convenience-functions"
    id="toc-convenience-functions">Convenience functions</a>
- <a href="#exploratory-stylometry"
  id="toc-exploratory-stylometry">Exploratory stylometry</a>
  - <a href="#bootstrap-tree-words-all"
    id="toc-bootstrap-tree-words-all">Bootstrap tree, words, all</a>
  - <a href="#bootstrap-tree-chars-all"
    id="toc-bootstrap-tree-chars-all">Bootstrap tree, chars, all</a>
  - <a href="#bootstrap-tree-characters-true-samples"
    id="toc-bootstrap-tree-characters-true-samples">Bootstrap tree,
    characters, true samples</a>
- <a href="#features-to-clusters"
  id="toc-features-to-clusters">Features-to-clusters</a>
- <a href="#umap-projections" id="toc-umap-projections">UMAP
  projections</a>
  - <a href="#umap-global" id="toc-umap-global">UMAP global</a>
- <a href="#distribution-of-distances"
  id="toc-distribution-of-distances">Distribution of distances</a>
  - <a href="#character-n-grams" id="toc-character-n-grams">Character
    n-grams</a>
  - <a href="#words" id="toc-words">Words</a>
- <a href="#general-impostors" id="toc-general-impostors">General
  impostors</a>
  - <a href="#impostors-char-n-grams"
    id="toc-impostors-char-n-grams">Impostors: char n-grams</a>
  - <a href="#impostors-longer-samples"
    id="toc-impostors-longer-samples">Impostors: longer samples</a>

## tl;dr (so far)

////

So far, two strong candidates for “writings-from-jail” style emerged:
**1) Navalny himself** and **2) Maria Pevchikh**. Likely it’s a mixed
corpus.

////

1.  `Navalny_free` vs `Navalny_jail` samples show stable heterogeneity:
    there is similarity between them, but the difference is obvious,
    too.  
2.  When verified against potential candidates, `Navalny_jail` exhibits
    **mixed signal**, both `Navalny_free` and `Maria Pevchikh` show
    stylistic affinities with jail samples.  
3.  Overall, between-author relationships are muddied (e.g Zhdanov and
    Volkov show certain level of similarity), which can be a trace of
    editor/ghostwriter.  
4.  **Corpus needs more attention**: 1) I can’t rule out some systematic
    bias: what we might be seeing is a ‘platform signal’, not
    authorship. 2) More Y. Navalnaya’s samples, we see she is similar to
    Pevchikh and Navalny, but not enough data to do proper
    verification. 3) Something is off about Sobol samples that come from
    large telegram data (duplicates?).

## Setup

``` r
library(stylo)
library(tidyverse)
library(tidytext)
library(seetrees)
library(umap)
## the last one is the custom package for feature-to-cluster association
## run devtools::install_github("perechen/seetrees")
```

## Data wrangling & book keeping

1.  Make one pool of words for each author

``` r
## read paths (remove intersecting Yarmysh_1 )
files <- list.files("corpus/",full.names = T)[-20]
names <- str_extract_all(files,"(?<=//).*?(?=_)") %>% unlist() %>% tolower() %>% unique()


for (a in names) {
  author <- files[str_detect(files,regex(a,ignore_case = T))]
  
  t <- lapply(author,read_file) %>%
  paste(collapse = "\n\n")
  
  if (!a %in% c("free", "jailed")) {
  write_file(t, paste0("corpus_clean/", a,".txt"))
  } else
  {write_file(t, paste0("corpus_clean/","Navalny_", a, ".txt"))}
  
  
}
```

Quick cleaning

``` r
library(stringi)

files = list.files("corpus_clean/",full.names = T)
texts = tibble(path=files, author=str_extract_all(files,"(?<=//).*?(?=\\.txt)") %>% unlist(), text=lapply(files,read_lines))

corpus_tokenized = texts %>% 
  unnest(cols = c(text)) %>% 
  group_by(author) %>% 
  mutate(line_id=row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(input = text,output=word) %>% 
  mutate(is_cyrillics = stri_detect_regex(word, "\\p{Cyrillic}")) %>% 
  filter(is_cyrillics) %>% 
  mutate(word=str_remove_all(word,"https|\\:|\\."))

## n tokens (words) per author
corpus_tokenized %>%
  group_by(author) %>%
  summarise(n=max(row_number())) %>% arrange(-n)
```

    ## # A tibble: 10 × 2
    ##    author               n
    ##    <chr>            <int>
    ##  1 sobol          1183927
    ##  2 volkov          434341
    ##  3 zhdanov         152178
    ##  4 yarmysh         100619
    ##  5 pevchikh         16046
    ##  6 alburov          12522
    ##  7 shaveddinov       9960
    ##  8 Navalny_jailed    9870
    ##  9 Navalny_free      9707
    ## 10 navalnaya         4892

``` r
corpus_lines <- corpus_tokenized %>% group_by(path,author,line_id) %>% summarize(line=paste(word,collapse=" "))

corpus_clean <- corpus_lines %>% group_by(path) %>% summarize(text=paste(line,collapse="\n\n"))

for(p in 1:nrow(corpus_clean)) {
  write_lines(corpus_clean$text[p],file=corpus_clean$path[p])
}

## for easier sampling for char n grams we need to make sure that sequences are more or less equal 
corpus_chunks <- corpus_tokenized %>%
  group_by(author) %>%
  mutate(word_id = row_number()) %>% 
  group_by(author,word_id) %>% 
  mutate(chunk=ceiling(word_id/30)) %>% 
  group_by(author, chunk) %>% 
  summarize(line=paste(word, collapse=" "))

# n 'lines' (or pseudo paragraphs)
corpus_lines %>% 
  mutate(nchar=lengths(str_extract_all(line, " "))) %>% 
  group_by(author) %>%
  summarise(n=max(row_number()),tokens=sum(nchar)) %>% arrange(-n) %>% mutate(r=tokens/n)
```

    ## # A tibble: 10 × 4
    ##    author             n  tokens     r
    ##    <chr>          <int>   <int> <dbl>
    ##  1 sobol          20275 1163652  57.4
    ##  2 volkov          6795  427546  62.9
    ##  3 zhdanov         5576  146602  26.3
    ##  4 yarmysh         4137   96482  23.3
    ##  5 alburov          964   11558  12.0
    ##  6 pevchikh         519   15527  29.9
    ##  7 shaveddinov      482    9478  19.7
    ##  8 Navalny_free     474    9233  19.5
    ##  9 Navalny_jailed   358    9512  26.6
    ## 10 navalnaya        202    4690  23.2

### Convenience functions

Some convenience functions for later

``` r
sample_independent_opt <- function(tokenized_df,
                                   n_samples,
                                   sample_size,
                                   text_var="word",
                                   folder="corpus_sampled/",overwrite=T) {
  
  dir.create(folder)
  
  if(overwrite) {
  do.call(file.remove, list(list.files(folder, full.names = TRUE)))
  }
  
  shuff <- tokenized_df %>%
    group_by(author) %>% 
    sample_n(n_samples*sample_size) %>% 
    mutate(sample=sample(rep(1:n_samples, each=sample_size))) %>% 
    unite(sample_id,c(author,sample),remove = F) %>% 
    group_by(sample_id) %>%
    summarize(text=paste(!!sym(text_var), collapse=" "))
  
  for(i in 1:nrow(shuff)) {
  write_file(file=paste0(folder, shuff$sample_id[i],".txt"), shuff$text[i])
}


}

sample_independent <-function(tokenized_df, n_samples,sample_size,text_var="word",folder="corpus_sampled/",overwrite=T) {
  
  dir.create(folder)
  
  if(overwrite) {
  do.call(file.remove, list(list.files("corpus_sampled/", full.names = TRUE)))
  }
  
shuff <- tokenized_df  %>% 
  group_by(author) %>%
  do(sample_n(., size = nrow(.))) %>%  # reshuffle rows for each author
  group_by(author) %>% 
  mutate(sample=ceiling(row_number()/sample_size)) %>%  # enumerate samples
  unite(sample_id,c(author,sample),remove = F) %>% 
  filter(sample != max(sample)) # discard the last (shorter) sample

## seed samples
seed_s <- shuff %>%
  select(author, sample_id) %>%
  group_by(author) %>% 
  summarize(sample_id=unique(sample_id)) %>% 
  sample_n(n_samples)

s<-shuff %>% 
  filter(sample_id %in% seed_s$sample_id) %>% 
  group_by(sample_id) %>%
  summarize(text=paste(!!sym(text_var), collapse=" "))


for(i in 1:nrow(s)) {
  write_file(file=paste0(folder, s$sample_id[i],".txt"), s$text[i])
}

}

diy_stylo <- function(folder="corpus_sampled/",mfw=200,drop_words=T,feature="word",n_gram=1) {
  
  
tokenized.texts = load.corpus.and.parse(files = list.files(folder,full.names = T),features = feature,ngram.size = n_gram)

# computing a list of most frequent words (trimmed to top 2000 items):
features = make.frequency.list(tokenized.texts, head = 5000)
# producing a table of relative frequencies:
data = make.table.of.frequencies(tokenized.texts, features, relative = TRUE)[,1:mfw]
s_words <- str_detect(colnames(data),paste(strong_words,collapse="|"))

if(drop_words) {
data <- data[,!s_words]
}

rownames(data) <- str_remove_all(rownames(data), "^.*?//")
rownames(data) <- str_replace_all(rownames(data), "Navalny_jail", "NavalnyJail")
## detecting strong content words
return(data)
}


# sample_independent(corpus_tokenized %>% filter(author != 'navalnaya'),
#                    sample_size=4500,
#                    n_samples = 2)
# 
# 
# ## example for lines: bad idea to sample uniformly since tweets have low per-line 
# sample_independent(corpus_chunks %>% filter(author != 'navalnaya'),
#                    sample_size=150,
#                    text_var="line",
#                    n_samples = 2)
```

## Exploratory stylometry

``` r
set.seed(1989)
# Nav samples together
st_words <- stylo(gui=F,
            mfw.min=200,
            mfw.max=200,
            analyzed.features = "w",
            ngram.size = 1,
            distance.measure = "wurzburg",
#            sampling="random.sampling",
            sample.size=4000,
            number.of.samples=2,
            corpus.dir="corpus_clean/",
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Nav samples together
set.seed(1989)
st_chars <- stylo(gui=F,
            mfw.min=200,
            mfw.max=200,
            analyzed.features = "c",
            ngram.size = 4,
            distance.measure = "wurzburg",
            corpus.dir="corpus_clean/",
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
## true random sampling
sample_independent(corpus_tokenized %>% filter(author != 'navalnaya'),
                   sample_size=4500,
                   text_var = "word",
                   n_samples = 2)

st_word_true_samples <- stylo(gui=F,
            mfw.min=200,
            mfw.max=200,
            analyzed.features = "w",
            ngram.size = 1,
            distance.measure = "wurzburg",
            corpus.dir="corpus_sampled/",
            number.of.samples=2,
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
## true random sampling
sample_independent(corpus_chunks %>% filter(author != 'navalnaya'),
                   sample_size=160,
                   text_var = "line",
                   n_samples = 2)

st_word_true_samples <- stylo(gui=F,
            mfw.min=300,
            mfw.max=300,
            analyzed.features = "c",
            ngram.size = 4,
            distance.measure = "wurzburg",
            corpus.dir="corpus_sampled/",
            number.of.samples=2,
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

### Bootstrap tree, words, all

``` r
bct_words <- stylo(gui=F,
            mfw.min=100,
            mfw.max=500,
            mfw.incr = 1,
            analyzed.features = "w",
            ngram.size = 1,
            analysis.type = "BCT",
            consensus.strength = 0.5,
            distance.measure = "wurzburg",
            corpus.dir="corpus_clean/",
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Bootstrap tree, chars, all

``` r
bct_words <- stylo(gui=F,
            mfw.min=100,
            mfw.max=500,
            mfw.incr = 1,
            analyzed.features = "c",
            ngram.size = 4,
            analysis.type = "BCT",
            consensus.strength = 0.5,
            distance.measure = "wurzburg",
            corpus.dir="corpus_clean/",
#            sampling="random.sampling",
            sample.size=4000,
            number.of.samples=2,
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
\### Bootstrap tree, words, true samples

``` r
## true random sampling
sample_independent(corpus_tokenized %>% filter(author != 'navalnaya'),
                   sample_size=4500,
                   text_var = "word",
                   n_samples = 2)

bct_words <- stylo(gui=F,
            mfw.min=100,
            mfw.max=200,
            mfw.incr = 1,
            analyzed.features = "w",
            ngram.size = 1,
            analysis.type = "BCT",
            consensus.strength = 0.5,
            distance.measure = "wurzburg",
            corpus.dir="corpus_sampled/",
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Bootstrap tree, characters, true samples

``` r
## true random sampling
sample_independent(corpus_chunks %>% filter(author != 'navalnaya'),
                   sample_size=150,
                   text_var = "line",
                   n_samples = 2)

bct_words <- stylo(gui=F,
            mfw.min=100,
            mfw.max=400,
            mfw.incr = 1,
            analyzed.features = "c",
            ngram.size = 4,
            analysis.type = "BCT",
            consensus.strength = 0.5,
            distance.measure = "wurzburg",
            corpus.dir="corpus_sampled/",
            corpus.lang="Other")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Features-to-clusters

To check for overarching bias, genre or register signal, it is always
good idea to look at which features may drive overarching clustering.
Topic-related words like `камеру`, `приговор`, `договор`, `власти`,
`владимир`, `рф`, `войн(а)`, `путина`, `навальный`, `год`, `выборы`,
`поражение`, `соболь`, `шизо` could be candidates for excluding from
analysis.

``` r
st_words <- stylo(gui=F,
            mfw.min=200,
            mfw.max=200,
            analyzed.features = "w",
            ngram.size = 1,
            distance.measure = "wurzburg",
            corpus.dir="corpus_clean/",
            corpus.lang="Other",
            display.on.screen=F)

view_tree(st_words,
          k=2,
          right_margin = 10,
          p=0.05)
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](stylometry_navalny_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
strong_words <- c("^украин", "^росси", "^навальн", "^войн", "^мир", "^против$", "^челов", "^путин", "^рф$", "^cоболь", "^выбор", "^власт", "^алексе", "^расследова", "^шизо", "^видео")
```

## UMAP projections

On words:

``` r
sample_independent(tokenized_df = corpus_tokenized %>% filter(author!="navalnaya"),
                   sample_size = 4500,
                   n_samples = 2,
                   text_var = "word",
                   overwrite = T,
                   folder = "corpus_sampled/")


dt <- diy_stylo("corpus_sampled/",mfw=200,drop_words = T)

projection <- umap(dt,data="dist")


tibble(x=projection$layout[,1],
       y=projection$layout[,2],
       author=str_remove_all(rownames(dt),"_[0-9]*?$")) %>%
  ggplot(aes(x,y,color=author)) + 
  geom_text(aes(label=author),size=6) + 
  theme_bw() + 
  guides(color="none") + 
  scale_color_viridis_d() + labs(title="UMAP projection, cosine delta, 200 MFW, 4.5k words samples")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

On char 4-grams:

``` r
sample_independent(tokenized_df = corpus_chunks %>% filter(author!="navalnaya"),
                   sample_size = 160,
                   n_samples = 2,
                   text_var = "line",
                   overwrite = T,
                   folder = "corpus_sampled/")


dt <- diy_stylo("corpus_sampled/",mfw=200,drop_words = T,feature = "c",n_gram = 4)

projection <- umap(dt,data="dist")


tibble(x=projection$layout[,1],
       y=projection$layout[,2],
       author=str_remove_all(rownames(dt),"_[0-9]*?$")) %>%
  ggplot(aes(x,y,color=author)) + 
  geom_text(aes(label=author),size=6) + 
  theme_bw() + 
  guides(color="none") + 
  scale_color_viridis_d() + labs(title="UMAP projection, cosine delta, 200 MF character 4-grams, ~4.5k words samples")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
sample_independent(tokenized_df = corpus_chunks %>% filter(author!="navalnaya"),
                   sample_size = 160,
                   n_samples = 2,
                   text_var = "line",
                   overwrite = T,
                   folder = "corpus_sampled/")


dt <- diy_stylo("corpus_sampled/",mfw=500,drop_words = T,feature = "c",n_gram = 4)

projection <- umap(dt,data="dist")


tibble(x=projection$layout[,1],
       y=projection$layout[,2],
       author=str_remove_all(rownames(dt),"_[0-9]*?$")) %>%
  ggplot(aes(x,y,color=author)) + 
  geom_text(aes(label=author),size=6) + 
  theme_bw() + 
  guides(color="none") + 
  scale_color_viridis_d() + labs(title="UMAP projection, cosine delta, 500 MF character 4-grams, ~4.5k words samples")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### UMAP global

More samples for each author (if available)

``` r
shuff <- corpus_tokenized  %>% 
  group_by(author) %>%
  do(sample_n(., size = nrow(.))) %>%  # reshuffle rows for each author
  group_by(author) %>% 
  mutate(sample=ceiling(row_number()/4500)) %>%  # enumerate samples
  unite(sample_id,c(author,sample),remove = F) #%>% 
#  filter(sample != max(sample)) # discard the last (shorter) sample

## seed samples
seed_s <- shuff %>%
  select(author, sample_id) %>%
  group_by(author) %>% 
  summarize(sample_id=unique(sample_id)) %>% 
  mutate(n=row_number(),
         is_large=ifelse(max(n)>20,T,F))  %>% 
  group_by(is_large) %>% 
  group_split(is_large)
## for very large corpora sample something reasonable
seed_s[[2]] <- seed_s[[2]] %>% group_by(author) %>% sample_n(20)

seed_s = bind_rows(seed_s) 

s<-shuff %>% 
  filter(sample_id %in% seed_s$sample_id) %>% 
  group_by(sample_id) %>%
  summarize(text=paste(word, collapse=" "))


do.call(file.remove, list(list.files("corpus_sampled/", full.names = TRUE)))
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [16] TRUE TRUE TRUE

``` r
for(i in 1:nrow(s)) {
  write_file(file=paste0("corpus_sampled/", s$sample_id[i],".txt"), s$text[i])
}



dt <- diy_stylo("corpus_sampled/",mfw=200,drop_words = T)

projection <- umap(dt,data="dist")


tibble(x=projection$layout[,1],
       y=projection$layout[,2],
       author=str_remove_all(rownames(dt),"_[0-9]*?$")) %>%
  filter(author!="sobol") %>% 
  ggplot(aes(x,y,color=author)) + 
  geom_text(aes(label=author),size=5) +
  theme_bw() + 
  guides(color="none") + 
  scale_color_viridis_d() + labs(title="UMAP projection, cosine delta, 200 MF words, 4.5k words samples")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

I filtered out Sobol previously because something is really off about
her corpus.

## Distribution of distances

Let’s set up a small ‘bootstrap the distance’ experiment to see how
differences between (and within) authors are distributed. In spirit this
is similar to ‘general imposters’ verification, but just relying on a
distance distribution instead of nearest-neighbor classification.

For I iterations:  
1. Pick MFW cutoff randomly;  
2. Take random samples and calculate distances;  
3. Record distances for each pair of relationships.

### Character n-grams

``` r
### function to process distance table
process_distances <- function(stylo_res) {
  dt <- stylo_res %>%   as.matrix()
d_long <- dt %>% 
  as_tibble() %>%
  mutate(source=colnames(dt)) %>% 
  pivot_longer(1:nrow(dt),names_to="target",values_to ="distance") %>% 
  mutate(source=str_remove(source, "_[0-9]*$"),
         target=str_remove(target, "_[0-9]*$")) %>% 
  filter(distance != 0)
return(d_long)

}

## function to plot distance distributions 
plot_distances <- function(df) {
  df %>% 
  ggplot(aes(distance,fill=mean)) + 
    geom_density(alpha=0.6) + 
    geom_vline(aes(xintercept=mean)) + 
    facet_grid(source ~ target,scales = "free_y") + 
    theme_bw() + 
    scale_fill_gradient2(high=lannister[1],mid=lannister[3],low=lannister[5],midpoint=1.05)
  
}
```

``` r
iters = 1000
min_features = 50
max_features = 500


d_res <- vector(mode="list",length=iters)

for(i in 1:iters) {
  mfw <- sample(seq(min_features,max_features, by=10),1)
  
  ## not optimized because stylo processes corpus from scratch each time, but will do for now
  ## true random sampling
sample_independent_opt(corpus_chunks %>% filter(author != 'navalnaya'),
                   sample_size=150,
                   text_var = "line",
                   n_samples = 2)

  st_res <- stylo(gui=F,
                  mfw.min=mfw,
                  mfw.max=mfw,
                  analyzed.features = "c",
                  ngram.size = 4,
                  distance.measure = "wurzburg",
                  corpus.dir="corpus_sampled/",
                  corpus.lang="Other",
                  display.on.screen = F)
   d_long <- process_distances(st_res$distance.table)
  ## assign results to list
  d_res[[i]] <- d_long
}

saveRDS(d_res,file="d_res.rds")
```

``` r
d_res <- readRDS("d_res.rds")
lannister = c("#5C0000", "#890000", "#C50000", "#FB7E00", "#FFA700")



d_df <- d_res %>%
  bind_rows() %>% 
  group_by(source,target) %>% 
  mutate(target=str_remove(target,"_[0-9]*?$"),
         source=str_remove(source,"_[0-9]*?$")) %>%
  mutate(mean=mean(distance)) 

navjail_mean <- d_df %>% filter(source=="Navalny_jailed",target=="Navalny_jailed") %>% pull(mean) %>% unique()

d_df %>% 
  plot_distances() +
  labs(title="Between sample distance distributions", subtitle="Character 4-grams, 50 to 500 MFW, 4.5k x 2 random samples per author, 1000 iterations") +
  theme(strip.text = element_text(size = 10)) +
  geom_vline(data=. %>% filter(target=="Navalny_jailed"), aes(xintercept=navjail_mean),color="red",linewidth=1)
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggsave("inoutdistances.png",width = 10,height = 7)
```

### Words

Same, but for words (with the removal of ‘strong topical words’)

``` r
iters = 500
min_features = 50
max_features = 200 

d_res_w <- vector(mode="list",length=iters)

for(i in 1:iters) {
  mfw <- sample(seq(min_features,max_features, by=10),1)
  
  sample_independent_opt(corpus_tokenized %>% filter(author != 'navalnaya'),
                   sample_size=4500,
                   text_var = "word",
                   n_samples = 2)
  
  
# loading the files from a specified directory:
tokenized.texts = load.corpus.and.parse(files = list.files("corpus_sampled/",
                                                           full.names = T))

# computing a list of most frequent words (trimmed to top 2000 items):
features = make.frequency.list(tokenized.texts, head = 5000)
# producing a table of relative frequencies:
data = make.table.of.frequencies(tokenized.texts, features, relative = TRUE)[,1:mfw]
s_words <- str_detect(colnames(data),paste(strong_words,collapse="|"))
data <- data[,!s_words]

rownames(data) <- str_remove_all(rownames(data), "^.*?//")
## detecting strong content words

dt <- dist.wurzburg(data) %>%  as.dist(upper = T,diag = T)
d_long <- process_distances(dt)
  ## assign results to list
  d_res_w[[i]] <- d_long
}

saveRDS(d_res_w,file="d_res_w.rds")
```

``` r
d_res_w <- readRDS("d_res_w.rds")




d_df <- d_res_w %>%
  bind_rows() %>% 
  group_by(source,target) %>% 
  mutate(mean=mean(distance)) #%>%

navjail_mean <- d_df %>% filter(source=="Navalny_jailed",target=="Navalny_jailed") %>% pull(mean) %>% unique()

navfree_mean <- d_df %>% filter(source=="Navalny_free",target=="Navalny_free") %>% pull(mean) %>% unique()

d_df %>% 
  plot_distances() +
  labs(title="Between sample distance distributions", subtitle="Word frequencies, 50 to 200 MFW, 4.5k words x 2 random samples per author, 1000 iterations") +
  theme(strip.text = element_text(size = 10)) +
  geom_vline(data=. %>% filter(target=="Navalny_jailed"), aes(xintercept=navjail_mean),color="red",linewidth=1) +
  geom_vline(data=. %>% filter(target=="Navalny_free"), aes(xintercept=navfree_mean),color="red",linewidth=1)  
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
Pevchikh signal is visibly strong for Navalny-in-jail (but not the free
corpus).

## General impostors

Let’s run general impostors method on larger samples and words
frequencies. We also will remove words that influence overarching
clustering.

The goal of the method is determine the *affinity* of a target sample
(in our case - Navalny in jail) to a set of candidates/impostors.
Imposters are there to provide a stylistic alternative bucket to a
candidate (in our case - free Navalny). Method records the proportion of
times target text fit to each candidate class (nearest-neighbor method).

First, let’s try to determine the ‘confidence scope’ for a given corpus
(not incluidng Jailed N here). Values between which lies the uncertain
‘i don’t know’ zone.

``` r
op_list_w <- vector("list",20)
for(i in 1:20) {

sample_independent_opt(corpus_tokenized %>% filter(author != 'navalnaya'),
                   sample_size=4500,
                   text_var = "word",
                   n_samples = 2)

data <- diy_stylo(folder="corpus_sampled/",mfw=200,feature = "w",n_gram = 1,drop_words = T)

op_list_w[[i]] <- imposters.optimize(data[-c(5,6),])

}

#r <- imposters(reference.set = data[-c(3),],test = data[c(3),],features = 0.5,iterations = 1000,distance="wurzburg")

saveRDS(op_list_w, "op_list_w.rds")
```

``` r
op_list_w <- readRDS("op_list_w.rds")

min_mean_w <- map(op_list_w, 1) %>% unlist() %>% mean()
max_mean_w <- map(op_list_w, 2) %>% unlist() %>% mean()
```

Values range from \~0.1 to \~0.67, which is a wide uncertainty zone.
Indicate some problems with authorial distinction.

``` r
imp_res <- vector("list",length = 100)
c=0
for(i in 1:50) {

  sample_independent_opt(corpus_tokenized %>% filter(author != 'navalnaya'),
                   sample_size=4500,
                   text_var = "word",
                   n_samples = 2)
  
  data <- diy_stylo(folder="corpus_sampled/",mfw=200,feature = "w",n_gram = 1,drop_words = T)


for(s in c(5,6)) {

r <- imposters(reference.set = data[-c(5:6),],test = data[c(s),],features = 0.5,iterations = 100,distance="wurzburg")
c=c+1

imp_res[[c]] <- tibble(candidate=names(r),proportion=r)

  
}
}

saveRDS(imp_res,"imp_res.rds")
```

``` r
imp_res <- readRDS("imp_res.rds")
imp_res %>% bind_rows() %>% ggplot(aes(reorder(candidate,-proportion),proportion))  + geom_boxplot() + theme_bw() + 
  geom_hline(aes(yintercept=min_mean_w),linewidth=1,linetype=2,color="red") +
    geom_hline(aes(yintercept=max_mean_w),linewidth=1,linetype=2,color="red") +
  labs(x="Candidate", title="General impostors vs. NavalnyJail, by-candidate hits", subtitle="4.5k words x 2 random samples per author, Cosine Delta. Words are controlled for topicality\nRed lines marks the uncertainty zone.")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

Pevchikh signal remains strong, both Navalny attribution and Pevchikh
attribution remain in uncertainty zone (determined from <c@1> score).

### Impostors: char n-grams

``` r
op_list <- vector("list",20)
for(i in 1:20) {

sample_independent_opt(corpus_chunks %>% filter(author != 'navalnaya'),
                   sample_size=160,
                   text_var = "line",
                   n_samples = 2)

data <- diy_stylo(folder="corpus_sampled/",mfw=500,feature = "c",n_gram = 4,drop_words = F)
op_list[[i]] <- imposters.optimize(data[-c(5,6),])

}

#r <- imposters(reference.set = data[-c(3),],test = data[c(3),],features = 0.5,iterations = 1000,distance="wurzburg")

saveRDS(op_list, "op_list.rds")
```

``` r
op_list <- readRDS("op_list.rds")

min_mean <- map(op_list, 1) %>% unlist() %>% mean()
max_mean <- map(op_list, 2) %>% unlist() %>% mean()
```

``` r
imp_res_char <- vector("list",length = 100)
c=0
for(i in 1:50) {

sample_independent(corpus_chunks %>% filter(author != 'navalnaya'),
                   sample_size=160,
                   text_var = "line",
                   n_samples = 2)
  
data <- diy_stylo(folder="corpus_sampled/",mfw=500,feature = "c",n_gram = 4,drop_words = F)


for(s in c(5,6)) {

r <- imposters(reference.set = data[-c(5:6),],test = data[c(s),],features = 0.5,iterations = 100,distance="wurzburg")
c=c+1

imp_res_char[[c]] <- tibble(candidate=names(r),proportion=r)

  
}
}
saveRDS(imp_res_char,"imp_res_char.rds")
```

``` r
imp_res_char <- readRDS("imp_res_char.rds")

imp_res_char %>% bind_rows() %>% ggplot(aes(reorder(candidate,-proportion),proportion))  + geom_boxplot() + theme_bw() + geom_hline(aes(yintercept=max_mean),linewidth=1,color="red",linetype=2) +
  geom_hline(aes(yintercept=min_mean),linewidth=1,color="red",linetype=2) +labs(x="Candidate", title="General impostors vs. NavalnyJail, by-candidate hits", subtitle="5k 4-char ngrams x 2 random samples per author, Cosine Delta, 500 MFW \nRed lines marks the uncertainty zone")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->
Char n-grams better capture `Navalny_jail` as Navalny, but Pevchikh,
again, is uncharacteristically high (cannot reject her). Volkov and
Zhdanov interfere more with the signal, which might be because of
topical signal (n-grams are not filtered for “too distinctive”
non-stylistic features), or even a ‘platform’ signal.

### Impostors: longer samples

``` r
imp_res_lg <- vector("list",length = 50)

for(i in 1:100) {

sample_independent_opt(corpus_tokenized %>% filter(author != 'navalnaya'),
                   sample_size=9000,
                   text_var = "word",
                   n_samples = 1)
  
data <- diy_stylo(folder="corpus_sampled/",mfw=200,drop_words = T)



r <- imposters(reference.set = data[-c(3),],test = data[c(3),],features = 0.5,iterations = 100,distance="wurzburg")

imp_res_lg[[i]] <- tibble(candidate=names(r),proportion=r)

  
}
saveRDS(imp_res_lg,"imp_res_lg.rds")
```

``` r
imp_res_lg <- readRDS("imp_res_lg.rds")

imp_res_lg %>% bind_rows() %>% ggplot(aes(reorder(candidate,-proportion),proportion))  + geom_boxplot() + theme_bw() + geom_hline(aes(yintercept=min_mean_w),linewidth=1,color="red",linetype=2) + 
  geom_hline(aes(yintercept=max_mean_w),linewidth=1,color="red",linetype=2) + 
  labs(x="Candidate", title="General impostors vs. NavalnyJail, by-candidate hits", subtitle="9k words equal sample per author, Cosine Delta, 200 MFW \nRed lines marks the uncertainty zone")
```

![](stylometry_navalny_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
