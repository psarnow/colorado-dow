# Colorado Department of Wildlife big game hunt statistics (colorado-dow)
## Project Description - Why did I choose it?
Data science specificially is about knowledge sharing and communicating what the data means to other people. 
I've personally spent countless hours pouring over hunt tables to decide where/when to hunt next year. Some analytical visual representations would certainly provide a more valuable solution. These will also lead to more questions about historical results.

This project has many variables that determine differing outcomes.

There are influences from multiple environments (supply/demand/regulations/weather).

This project also has an intuitive progression of the Analytics Roadmap.

This project is about working with interesting data. A lot of people do things with financial information or Twitter data; those can work, but the data isn’t inherently that interesting.

I started this back in 2015 which culminated into a [Web-app example](https://dowproject.shinyapps.io/GJSON/)

This year I'd like to walk through the analytics roadmap and detail the nuances of each phase.  The increase in value and difficulty as we progress through the phases should be pretty apparent.

I will demonstrate model building techniques common to Data Scientists by utilizing R. 
Its also important to note that data modelers have a sense of what they are modeling. Intuition and content expertise are incredibly valuable.  In this case I'll note that I have been a Colorado elk hunter for decades, and am also familiar with the data provided from CPW after attempting to make sense of their hunt tables over the years.

## Setup -- Data Acquisition
CPW provides a lot of info for hunters to sift through.  Let's start by accessing what they provide.
* [Elk harvest data from CPW](http://rpubs.com/psarnow/404268)

* [Colorado elk population estimates from CPW](http://rpubs.com/psarnow/393560)

* [Hunt draw summaries from CPW](http://rpubs.com/psarnow/394721)

* [Hunt season dates from CPW](http://rpubs.com/psarnow/393655)

* [CPW hunt unit boundaries and major highways for mapping](http://rpubs.com/psarnow/405816)

Additionally, weather can play a large role in hunting success. Let's grab some historical data from Dark Sky using their API
* [Weather from Dark Sky](http://rpubs.com/psarnow/393658)

## Phase I -- Descriptive Analytics (What happened)
**Goal** I would like to hunt next year and would like to know which season will provide me the best chance of success for a certain Unit.  How did things go in past years?

#### Initial questions
* [Where are the elk at in Colorado?](http://rpubs.com/psarnow/396876)

* [Where are the hunters at in Colorado?](http://rpubs.com/psarnow/396897)

* [Where are the elk harvested at in Colorado?](http://rpubs.com/psarnow/405573)

* [What kind of elk are harvested?](http://rpubs.com/psarnow/406784)

* [Which units have higher Elk to Hunter ratios?](http://rpubs.com/psarnow/396916)

* [Which units have higher Hunter Success Rates?](http://rpubs.com/psarnow/397169)

* [Which units have higher License Draw Success Rates?](http://rpubs.com/psarnow/398440)

## Phase II -- Diagnostic Analytics (Why did it happen)
Any relationship between Units and their results?
Are there other factors to consider?
* Preference points
* Draw results
* Herd size
* Weather
* Hunt season dates
**Goal** What makes the first season in Unit 77 have the most hunter success? Why does it take less effort (hunting days) to be successful in the first season of Unit 77?

## Phase III -- Predictive Analytics (What will happen)
Use machine learning and predictive modeling to forcast future hunt seasons. 
* I will utilize Preprocessing and data transformations
* Model building / tuning / training
* Model testing
* Release web service to analyze new data

Begin with using historical data and forecasted weather to predict Elk Harvest. I will then be able to use that data to predict Population and other hunt results.

## Phase IV -- Prescriptive Analytics (How can we make it happen)
What can we influence? This is probably something CPW performs for future year’s regulations.
