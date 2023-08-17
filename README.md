# Ecological forecasting with `mvgam` and `brms`

### Physalia-Courses 

https://www.physalia-courses.org/

### Nicholas J Clark

#### 4&ndash;8th September, 2023

## COURSE OVERVIEW
Time series analysis and forecasting are standard goals in applied ecology. But most time series courses focus only on traditional forecasting models such as ARIMA or Exponential Smoothing. These models cannot handle features that dominate ecological data, including overdispersion, clustering, missingness, discreteness and nonlinear effects. Using the flexible and powerful Bayesian modelling software Stan, we can now meet this complexity head on. Packages such as {mvgam} and {brms} can build Stan code to specify ecologically appropriate models that include nonlinear effects, random effects and dynamic processes, all with simple interfaces that are familiar to most R users. In this course you will learn how to wrangle, visualize and explore ecological time series. You will also learn to use the [{mvgam}](https://github.com/nicholasjclark/mvgam) and [{brms}](https://github.com/paul-buerkner/brms) packages to analyse a diversity of ecological time series to gain useful insights and produce accurate forecasts. All course materials (presentations, practical exercises, data files, and commented R scripts) will be provided electronically to participants.

## TARGET AUDIENCE AND ASSUMED BACKGROUND
This course is aimed at higher degree research students and early career researchers working with time series data in the natural sciences (with particular emphasis on ecology) who want to extend their knowledge by learning how to add dynamic processes to model temporal autocorrelation. Participants should ideally have some knowledge of regression including linear models, generalized linear models and hierarchical (random) effects. But we’ll briefly recap these as we connect them to time series modelling.

Participants should be familiar with RStudio and have some fluency in programming R code. This includes an ability to import, manipulate (e.g. modify variables) and visualise data. There will be a mix of lectures and hands-on practical exercises throughout the course.

## LEARNING OUTCOMES
1.    Understand how dynamic GLMs and GAMs work to capture both nonlinear covariate effects and temporal dependence
2.    Be able to fit dynamic GLMs and GAMs in R using the {mvgam} and {brms} packages
3.    Understand how to critique, visualize and compare fitted dynamic models
4.    Know how to produce forecasts from dynamic models and evaluate their accuracies using probabilistic scoring rules

## COURSE PREPARATION

Please be sure to have at least version 4.1 &mdash; *and preferably version 4.2* &mdash; of `R` installed. Note that `R` and `RStudio` are two different things: it is not sufficient to just update `RStudio`, you also need to update `R` by installing new versions as they are released.

To download `R` go to the [CRAN Download](https://cran.r-project.org/) page and follow the links to download `R` for your operating system:

* [Windows](https://cran.r-project.org/bin/windows/)
* [MacOS X](https://cran.r-project.org/bin/macosx/)
* [Linux](https://cran.r-project.org/bin/linux/)

To check what version of `R` you have installed, you can run

```r
version
```

in `R` and look at the `version.string` entry (or the `major` and `minor` entries).

We will make use of several `R` packages that you'll need to have installed. Prior to the start of the course, please run the following code to update your installed packages and then install the required packages:

```r
# update any installed R packages
update.packages(ask = FALSE, checkBuilt = TRUE)

# packages to install
pkgs <- c("brms", "dplyr", "gratia", "ggplot2",
          "marginaleffects", "tidybayes", "zoo",
          "viridis", "remotes")

# install those packages by setting Ncpus to number of CPU cores you have available
install.packages(pkgs, Ncpus = 4)
```

For both `mvgam` and `brms`, *it is highly recommended that you use the `Cmdstan` backend*, with the `cmdstanr` interface, rather than using `rstan`. To install `Cmdstan` and the relevant interface, first install `cmdstanr` using the following:

```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

And then [follow instructions provided by the `Stan` development team here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) to ensure the backend is installed and the toolchain is setup properly


Finally, we will make use of the development version of the `mvgam` package as it is not quite ready for CRAN. You can install this package using the following:

```r
# Download and install mvgam
remotes::install_github('nicholasjclark/mvgam', force = TRUE)
```

## PROGRAM
09:00 - 12:00 (Berlin time): live lectures and introduction to / review of the practicals

3 additional hours: self-guided practicals using annotated R scripts

 

## Monday (day 1) 
[Lecture 1 Slides](https://nicholasjclark.github.io/physalia-forecasting-course/day1/lecture_1_slidedeck) | [Lecture 2 Slides](https://nicholasjclark.github.io/physalia-forecasting-course/day1/lecture_2_slidedeck) | [Tutorial](https://nicholasjclark.github.io/physalia-forecasting-course/day1/tutorial_1_physalia)
* Introduction to time series and time series visualization
* Some traditional time series models and their assumptions
* GLMs and GAMs for ecological modelling
* Temporal random effects and temporal residual correlation structures


## Tuesday (day 2) 
[Lecture 3 Slides](https://nicholasjclark.github.io/physalia-forecasting-course/day2/lecture_3_slidedeck) | [Tutorial](https://nicholasjclark.github.io/physalia-forecasting-course/day2/tutorial_2_physalia)
* Dynamic GLMs and Dynamic GAMs
* Autoregressive dynamic processes
* Gaussian Processes
* Dynamic coefficient models


## Wednesday (day 3)
[Tutorial](https://nicholasjclark.github.io/physalia-forecasting-course/day3/tutorial_3_physalia)
* Forecasting from dynamic models
* Point-based forecast evaluation
* Probabilistic forecast evaluation
* Bayesian posterior predictive checks


## Thursday (day 4)
* Multivariate ecological time series
* Vector autoregressive processes
* Dynamic factor models
* Multivariate forecast evaluation


## Friday (day 5)
* Extended practical examples using {mvgam}
* Review and open discussion
