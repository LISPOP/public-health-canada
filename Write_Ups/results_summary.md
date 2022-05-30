---
title: "Summary of Results"
author: 
  - "Matthew Arp"
  - "Simon J. Kiss"
  - "Patrick Fafard"
  - "Ketan Shankardass"
  - "Erick Lachapelle"
  - "Andrea M. L. Perrella"
date: "18/02/2021"
output: 
#  word_document: default
  html_document: 
    keep_md: true
# knit: (function(input, ...) {
#     rmarkdown::render(
#       input,
#       output_dir = here::here("~", "Dropbox", "Public_Health"),
#     )
#   })
  
---



## Introduction

In 2021 we surveyed members of Canada's public health workforce. These are the results. 

Now that we have a partial data file, we can actually do some background work getting some code set up that will run automatically when we get the full data. 

### Formatting the document for Word

Matt, could you read through this [document](https://rmarkdown.rstudio.com/articles_docx.html) and fiddle with a Word template and see if you can get it too look a little nicer; maybe a little close to that LISPOP template? I am not sure though how many features we will be able to integrate with RMarkdown. 

## Methodology
Can we add a blurb about how we sampled and who we sampled. 




## Section 1

Here is our data: Turn on eval=T in the chunk below if you actually want to see it, otherwise, turn it off. 


```r
str(ph)
```

