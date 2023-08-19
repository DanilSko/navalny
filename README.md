# navalny

Efforts to reproduce "Navalny's texts from jail are not written by Navalny" claim and stylometric results.

## Data 
We gathered some stuff manually, but also a sizeable share of our data comes from: https://github.com/norpadon/navalny (many thanks!)  
See 'corpus' folder and other folders in this repo

## Research 

### tl;dr (so far)

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

### Full research: 
[Notebook for R and stylo](stylometry_navalny.md) (renders on GH)

### Misc
Check also [this attempt](https://github.com/religofsil/NavalnyResearchCheck/tree/main) on much smaller corpus.


