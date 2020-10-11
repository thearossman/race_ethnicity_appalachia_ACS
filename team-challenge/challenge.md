Race, Ethnicity, & Appalachian Peoples: Scaffolding Back-Talk with ACS
Data
================
Your Name
2019-

  - [1. Background & motivation](#background-motivation)
  - [2. Where is Appalachia?](#where-is-appalachia)
  - [3. Background: race and ethnicity in Appalachian
    counties.](#background-race-and-ethnicity-in-appalachian-counties.)
  - [4. Get census data on race and ethnicity in
    Appalachia](#get-census-data-on-race-and-ethnicity-in-appalachia)
  - [5. Get census block data for race and ethnicity in Appalachian
    sub-counties](#get-census-block-data-for-race-and-ethnicity-in-appalachian-sub-counties)
  - [6. (optional) Do some more
    analysis?](#optional-do-some-more-analysis)
  - [7. Summary](#summary)

``` r
# Libraries
library(tidyverse)
library(dcl)
library(tidycensus)

# Parameters

ky_central_app_counties <- 
  c(
    "Adair", "Bath", "Bell", "Boyd", "Breathitt", "Carter", "Casey", "Clark", 
    "Clay", "Clinton", "Cumberland", "Edmonson", "Elliott", "Estill", "Fleming", 
    "Floyd", "Garrard", "Green", "Greenup", "Harlan", "Hart", "Jackson", 
    "Johnson", "Knott", "Knox", "Laurel", "Lawrence", "Lee", "Leslie", 
    "Letcher", "Lewis", "Lincoln", "McCreary", "Madison", "Magoffin", "Martin", 
    "Menifee", "Metcalfe", "Monroe", "Montgomery", "Morgan", "Nicholas", 
    "Owsley", "Perry", "Pike", "Powell", "Pulaski", "Robertson", "Rockcastle", 
    "Rowan", "Russell", "Wayne", "Whitley",  "Wolfe"
  )

tn_central_app_counties <-
  c(
    "Anderson", "Bledsoe", "Blount", "Bradley", "Campbell", "Cannon", "Carter", 
    "Claiborne", "Clay", "Cocke", "Coffee", "Cumberland", "De Kalb", "Fentress", 
    "Franklin", "Grainger", "Greene", "Grundy", "Hamblen", "Hamilton", 
    "Hancock", "Hawkins", "Jackson", "Jefferson", "Johnson", "Knox", "Lawrence", 
    "Lewis", "Loudon", "McMinn", "Macon", "Marion", "Meigs", "Monroe", "Morgan", 
    "Overton", "Pickett", "Polk", "Putnam", "Rhea", "Roane", "Scott", 
    "Sequatchie", "Sevier", "Smith", "Sullivan", "Unicoi", "Union", "Van Buren", 
    "Warren", "Washington", "White"
  )

va_central_app_counties <- 
  c(
    "Alleghany", "Bath", "Bland", "Botetourt", "Buchanan", "Carroll", 
    "Craig", "Dickenson", "Floyd", "Giles", "Grayson", "Henry", "Highland", 
    "Lee", "Montgomery", "Patrick", "Pulaski", "Rockbridge", "Russell", 
    "Scott", "Smyth", "Tazewell", "Washington", "Wise", "Norton", "Wythe"
  )

# note that all counties in WV are in the ARC def of Central Appalachia

#===============================================================================

# Code
```

## 1\. Background & motivation

Before beginning this challenge, read
[this](https://www.guernicamag.com/elizabeth-catte-appalachia-isnt-trump-country/)
interview with public historian Elizabeth Catte and
[this](https://www.thestayproject.net/blog-stay-in-the-news/stays-full-statement-re-jd-vance-at-the-2018-appalachian-studies-conference)
statement from the Stay Together Appalachian Youth (STAY) project.

Both of the pieces above reference the book *Hillbilly Elegy* by J.D.
Vance. Vance has joined a long line of journalists, writers, social
scientists (in particular, statisticians), politicians, and aid workers
who have, since the 1880s, presented a singular, exaggerated narrative
of the peoples living in the central and southern Appalachian mountains.
The narratives about Appalachia that the above authors reference have,
depsite historical changes, remained relatively static.

This challenge emerges from the ways in which census data, including the
American Community Survey, has been used to both perpetuate these
narratives about Appalachia and, by folks in the region, to buttress
personal stories in speaking back against them.

Two major ways that the former has happened, and how it shows up in
*Hillbilly Elegy*:

  - In referring to the nebulous population which he claims his
    experience represents, J.D. Vance uses the descriptors “Appalachian
    whites,” “the white working-class,” “poor whites,” “rural whites,”
    and “Appalachians” interchangeably. This collapsing of identities –
    assuming that working-class and poor white people live primarily in
    Appalachia, and that everyone in Appalachia is poor/working-class
    and white – is hardly unique to Vance and hardly new. It erases the
    experiences of people of color currently in the region, people of
    color with ancestral ties to the region, and the majority of poor
    and working-class white people (who live elsewhere).
  - J.D. Vance joins up with others (including white supremacist
    statistician Charles Murray, whom Elizabeth Catte references in her
    interview above) who draw on ACS and other data (e.g., two-parent
    home, marriage and divorce, religious practice) to scaffold a
    “culture of poverty” argument – e.g., the claim that individuals,
    not systems, are responsible for personal economic conditions.

One relatively simple tactic that people within the region have used to
push back is analyzing and citing census data that contradict bullet
point \#1.

**q1.1** Find at least two claims from the articles or paragraph above
that you imagine someone could be skeptical of and that you believe you
could help back up with data.

## 2\. Where is Appalachia?

Before we begin to pull data from the census, we need to define and get
FIPS codes for a workable geographic region.

I suggest basing your analysis on two different definitions of
Appalachia.

**q2.1** The first is from the [Appalachian Regional
Commission](https://www.arc.gov/appalachian_region/CountiesinAppalachia.asp).
This is the most common definition used. It comes from the 1960s War on
Poverty era. (borders are cultural and historical artifacts.)

You are going to be making maps and pulling census data, so you need
FIPS codes. On the ARC website, you will be able to find reports and
studies, some of which provide maps. These allow you to download
supporting data as an Excel file. These files include data for counties,
which include county FIPS codes. (Hint: I used a “County Economic Status
in Appalachia” study.)

Download one of these files, import the data, and create a tibble with
the FIPS code, county name, and state name. Call this
`all_appalachia_counties`.

**q2.2** The second is from the [Appalachian Community
Fund](http://www.appalachiancommunityfund.org/where-we-fund/). These
counties represent one boundary definition of “Central Appalachia.”

The county names from this webpage are in the parameters, divided by
state (note that all WV counties are on the list, so they are not in the
parameters). Create a tibble similar to `all_appalachia_counties` called
`central_appalachia_counties`.

I did this by getting FIPS codes from the census for all counties; then
filtering this by state and by a vector of county names; then binding
the rows of these tibbles together. (If there’s a faster way of doing
it, please do that\!)

## 3\. Background: race and ethnicity in Appalachian counties.

Skim and/or read through
[this](https://www.npr.org/sections/codeswitch/2014/04/03/298892382/stereotypes-of-appalachia-obscure-a-diverse-picture),
[this](https://www.thestayproject.net/what-is-stay.html),
[this](http://appvoices.org/2019/06/07/contending-with-contamination-in-minden-w-va/),
[this](https://www.hollerhealthjustice.org),
[this](https://www.thenation.com/article/archive/seeing-appalachia-through-the-eyes-of-appalachians/),
and
[this](https://www.facebook.com/highlandercenter/posts/2399172926781803).

A note: As Barbara Ellen Smith (among others) has noted, the
contemporary white majority in Appalachia should be taken not as “a
benign demographic fact,” but as “a product of active practices
characterized in part by persistent white supremacy.” Think about
colonization, land theft, and American Indian Removal policies;
discrimination in jobs and housing that made it harder for Black
families to stay; racial terrorism that forced some non-white people to
flee; and the challenges of living under Jim Crow that pushed many who
could to pass as white.

**q3.1** From the articles above, what are some things that stick out to
you?

## 4\. Get census data on race and ethnicity in Appalachia

**q4.1** Use tidycensus to get statistics on race and ethnicity and
estimates for total population for the Appalachian regions defined
above.

Some notes: For this, I used the ACS5 for 2018; Sara said that it’s
pretty accurate, and the most recent decennial census was almost 10
years ago. I considered “white” to mean “white alone, non-Hispanic.” I
chose variables for race that distinguish from Hispanic and/or Latinx
origin, and I did not break down American Indian/Alaska Native
populations by tribal citizenship.

**q4.2** Explore the data a bit. In doing this, I found it helpful to
create a “prop\_white” variable.

  - Check out which counties have the largest white and non-white
    populations. Check out averages and medians.
  - Check out how non-white residents self-identified on the census. If
    you want, pull additional data on ethnicity for the census for
    specific counties.
  - Maybe check out absolute numbers. What do you notice? One question
    that was interesting to me: in the perception that Appalachia is
    white, approximately how many people are being erased?
  - Other ideas?

**q4.3** Using the SF package, create EDA density maps of (1)
Appalachia, and (2) Central Appalachia, showing the proportion of
nonwhite county residents.

## 5\. Get census block data for race and ethnicity in Appalachian sub-counties

One issue with looking at race and ethnicity data by counties is that
counties are really big.

**q5.1** Using the same variables as above and the tidycensus package,
create *one* tibble for *Central Appalachia* with race and ethnicity
statistics for [census
blocks](https://www2.census.gov/geo/pdfs/reference/GARM/Ch11GARM.pdf)
(more details
[here](https://walkerke.github.io/tidycensus/articles/basic-usage.html)).

Notes: \* you will have to specify state in your API call, so you will
need multiple API calls (one for each of the states with counties in
central Appalachia). \* Hint for filtering to create the tibble: think
about how the GEOID for census block is related to the GEOID for the
county that it’s in.

Explore the data a bit (arrange, etc.). What do you notice? How is
looking at these data different from looking at counties?

## 6\. (optional) Do some more analysis?

**q6.1** Option: White households living below the poverty line,
nationally

One claim that Elizabeth Catte and others make is that Appalachia is not
significantly different or unique compared to the rest of the U.S.
Conflating the “white working-class” with Appalachia erases not only
people of color in the region, but also the vast majority of white poor
and working-class people who live elsewhere.

Using ACS data, perform an analysis of white households (and/or people)
living below the poverty line for the entire U.S. I found it helpful to
look at raw numbers to think through what it means to make assumptions
that lump this population together.

**q6.2** Option: Appalachia and 2016 election

[This](https://www.theguardian.com/us-news/2016/nov/09/white-voters-victory-donald-trump-exit-polls)
analysis of 2016 election exit polls suggested that the majority of
people who voted for Trump were white, despite many post-election
analyses and journalism that focused energy and attention toward the
“white working class” and/or Appalachia.

Using the dataset from the DCL elections challenge and demographic data
from the ACS, answer the question: did the majority of voting-age adults
in Appalachia (excluding people incarcerated in Appalachia, by default)
vote for Trump?

**q6.3** Population change: race and ethnicity in Appalachia

A 2017 [summary](https://files.eric.ed.gov/fulltext/ED596266.pdf) of ACS
data examines data on the change in race and ethnicity, 2010-2017, as
proportions of total population (page 25-28).

(Note: this summary was really useful for me in thinking through what
the ACS can offer and what I could recreate\!)

Choose a period of time of 7 years or longer, ending in 2017 or 2018.
Calculate and examine the percentage point change in the share of the
population in the Appalachian region, for the variables you used above
representing race and Hispanic/Latinx origin.

Using population raw numbers, make an argument about whether this
represents people of color coming into the region and/or white
non-Hispanic people leaving.

## 7\. Summary

Imagine that someone not connected to Appalachia recommended “Hillbilly
Elegy” to you. Using the resources above and the data analysis you
conducted, write a short paragraph that amplifies the work of
organizers, the historical context of narrative harms, and demographic
data to describe where this book (or another like it) misses the mark.
If you do have a connection to Appalachia, please include your own
experiences.

*Finally, optional: read “We play Billie Holiday at the abortion
clinic,” by Alice; in the last pages of the STAY project zine
[here](https://www.thestayproject.net/uploads/7/5/6/0/75605377/appalachian_love_story_zine.pdf);
and
[this](http://www.appfellows.org/appfellows-blog/2018/9/6/land-reform-cant-happen-without-the-abolishing-the-prison-industrial-complex-by-lill-prosperino)
essay by Lil on the prison industrial complex*
