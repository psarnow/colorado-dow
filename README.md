# Colorado Department of Wildlife big game hunt statistics (colorado-dow)
## Project Description - Why did I choose it?
I've personally spent countless hours pouring over hunt tables to decide where/when to hunt. Some analytical visual representations would certainly provide a more valuable solution. These will also lead to more questions about historical results.

This project has many variables that determine differing outcomes.

There are influences from multiple environments (supply/demand/regulations/weather)

This project also has an intuitive progression of the Analytics Roadmap

I started this back in 2015 which culminated into a Web-app example
https://dowproject.shinyapps.io/GJSON/

This year I'd like to walk through the analytics roadmap and detail the nuances of each phase.  The increase in value and difficulty as we progress through the phases should be pretty apparent.

I will demonstrate model building techniques common to Data Scientists by utilizing R. 
Its also important to note that data modelers have a sense of what they are modeling. Intuition and content expertise are incredibly valuable.  In this case I'll note that I have been an elk hunter for decades, and am also familiar with the data provided from CPW after attempting to make sense of their hunt tables over the years.

## Phase I -- Descriptive Analytics (What happened)
**Goal** I would like to hunt next year and would like to know which season will provide me the best chance of success for a certain Unit.  How did things go in past years?

#### Step 1 -- Data Acquisition
Elk hunting statistics from CPW - http://rpubs.com/psarnow/393142
#### Step 2 -- Wrangle the data
#### Step 3 -- Chart results
Model and chart data to answer our initial questions - http://rpubs.com/psarnow/393161

## Phase II -- Diagnostic Analytics (Why did it happen)
Any relationship between Units and their results?
Are there other factors to consider?
* Preference Points
* Herd size
* Weather
#### Step 1 -- Data Acquisition
Herd population estimates from CPW - http://rpubs.com/psarnow/393560

## Phase III -- Predictive Analytics (What will happen)
* Preprocessing and data transformations
* Model building / tuning / training
* Model testing

## Phase IV -- Prescriptive Analytics (How can we make it happen)
What can we influence? This is probably something DOW performs for future year’s regulations.
