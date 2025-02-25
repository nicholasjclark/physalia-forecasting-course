# Ecological forecasting with `mvgam` and `brms`

### Physalia-Courses 

https://www.physalia-courses.org/

### Nicholas J Clark

#### 24&ndash;28th March, 2025

## COURSE OVERVIEW
Time series analysis and forecasting are standard goals in applied ecology. But most time series courses focus only on traditional forecasting models such as ARIMA or Exponential Smoothing. These models cannot handle features that dominate ecological data, including overdispersion, clustering, missingness, discreteness and nonlinear effects. Using the flexible and powerful Bayesian modelling software `Stan`, we can now meet this complexity head on. Packages such as `{mvgam}` and `{brms}` can build Stan code to specify ecologically appropriate models that include nonlinear effects, random effects and dynamic processes, all with simple interfaces that are familiar to most `R` users. In this course you will learn how to wrangle, visualize and explore ecological time series. You will also learn to use the [`{mvgam}`](https://nicholasjclark.github.io/mvgam/) and [`{brms}`](https://paul-buerkner.github.io/brms/) packages to analyse a diversity of ecological time series to gain useful insights and produce accurate forecasts. All course materials (presentations, practical exercises, data files, and commented `R` scripts) will be provided electronically to participants.

## TARGET AUDIENCE AND ASSUMED BACKGROUND
This course is aimed at higher degree research students and early career researchers working with time series data in the natural sciences (with particular emphasis on ecology) who want to extend their knowledge by learning how to add dynamic processes to model temporal autocorrelation. Participants should ideally have some knowledge of regression including linear models, generalized linear models and hierarchical (random) effects. But we’ll briefly recap these as we connect them to time series modelling.

Participants should be familiar with RStudio and have some fluency in programming `R` code. This includes an ability to import, manipulate (e.g. modify variables) and visualise data. There will be a mix of lectures and hands-on practical exercises throughout the course.

## LEARNING OUTCOMES
1.    Understand how dynamic GLMs and GAMs work to capture both nonlinear covariate effects and temporal dependence
2.    Be able to fit dynamic GLMs and GAMs in R using the `{mvgam}` and `{brms}` packages
3.    Understand how to critique, visualize and compare fitted dynamic models
4.    Know how to produce forecasts from dynamic models and evaluate their accuracies using probabilistic scoring rules

## COURSE PREPARATION

**Please be sure to have at least version 4.2 &mdash; and preferably version 4.3 or above &mdash; of `R` installed**. Note that `R` and `RStudio` are two different things: it is not sufficient to just update `RStudio`, you also need to update `R` by installing new versions as they are released.

To install `RStudio`, go to https://posit.co/download/rstudio-desktop/ and follow the instructions.

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

# packages to install for the course
pkgs <- c("brms", "tidybayes", "mvgam")

# install packages, including their dependencies
install.packages(pkgs, dependencies = TRUE)
```

### INSTALLING AND CHECKING STAN
When working in `R`, there are two primary interfaces we can use to fit models with `Stan` (`{rstan}` and `CmdStan`). Either interface will work, however it is highly recommended that you use the `Cmdstan` backend, with the `{cmdstanr}` interface, rather than using `{rstan}`. More care, however, needs to be taken to ensure you have an up to date version of `Stan`. **For all `{mvgam}` and `{brms}` functionalities to work properly, please ensure you have at least version 2.29 of `Stan` installed**. The CRAN and GitHub development versions of `{rstan}` and `CmdStan` are currently several versions ahead of this, and all of these versions are stable. The exact version you have installed can be checked using either `rstan::stan_version()` or `cmdstanr::cmdstan_version()`

Compiling a Stan program requires a modern C++ compiler and the GNU Make build utility (a.k.a. “gmake”). The correct versions of these tools to use will vary by operating system, but unfortunately most standard Windows and MacOS X machines do not come with them installed by default. The first step to installing `Stan` is to update your C++ toolchain so that you can compile models correctly. [There are detailed instructions by the Stan team on how to ensure you have the correct C++ toolchain to compile models](https://mc-stan.org/docs/cmdstan-guide/installation.html#cpp-toolchain), so please refer to those and follow the steps that are relevant to your own machine. Once you have the correct C++ toolchain, you'll need to install `Cmdstan` and the relevant `R` pacakge interface. First install the `R` package `{cmdstanr}` by running the following command in a fresh R environment:

```{r}
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```
`cmdstanr` requires a working installation of [CmdStan](https://mc-stan.org/users/interfaces/cmdstan.html), the shell interface to `Stan`. If you don't have `CmdStan` installed then `{cmdstanr}` can install it for you, assuming you have a suitable C++ toolchain. To double check that your toolchain is set up properly you can call the `check_cmdstan_toolchain()` function:

```{r}
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE)
```
If your toolchain is configured correctly then `CmdStan` can be installed by calling the
[`install_cmdstan()`](https://mc-stan.org/cmdstanr/reference/install_cmdstan.html) function:

```{r}
install_cmdstan(cores = 2)
```
You should now be able to follow the remaining instructions on the [Getting Started with `{cmdstanr}` page](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) to ensure that `Stan` models can successfully compile on your machine. A quick way to check this would be to run this script:

```{r}
library(mvgam)
simdat <- sim_mvgam()
mod <- mvgam(y ~ s(season, bs = 'cc', k = 5) +
               s(time, series, bs = 'fs', k = 8),
             data = simdat$data_train)
```

But issues can sometimes occur when:
1. [you don't have write access to the folders that `CmdStan` uses to create model executables](https://discourse.mc-stan.org/t/problem-running-cmdstan-on-computing-cluster/34747/5)
2. [you are using a university- or company-imposed syncing system such as One Drive, leading to confusion about where your make file and compilers are located](https://discourse.mc-stan.org/t/system-command-make-failed-models-wont-compile/30528)
3. [you are using a university- or company-imposed firewall that is aggressively deleting the temporary executable files that `CmdStan` creates when compiling](https://discourse.mc-stan.org/t/trouble-with-cmdstan-toolchain-with-rtools42-on-windows-10-enterprise/28444)

If you run into any of these issues, it is best to consult with your IT department for support.

## PROGRAM
09:00 - 12:00 (Berlin time): live coding exercises and review of the lecture materials

3 additional hours: self-guided readings and viewing of lecture recordings

 

## Monday (day 1) 
Lecture 1 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day1/lecture_1_slidedeck) | [pdf](https://github.com/nicholasjclark/physalia-forecasting-course/raw/main/day1/lecture_1_slidedeck.pdf)) 
<br>
Lecture 2 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day1/lecture_2_slidedeck) | [pdf](https://github.com/nicholasjclark/physalia-forecasting-course/raw/main/day1/lecture_2_slidedeck.pdf))
<br>
Live code examples ([Random effects](https://raw.githubusercontent.com/nicholasjclark/physalia-forecasting-course/main/day1/live_code_examples/live_1_randomeffects.R))
<br>
Tutorial 1 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day1/tutorial_1_physalia))
* Introduction to time series and time series visualization
* Some traditional time series models and their assumptions
* GLMs and GAMs for ecological modelling
* Temporal random effects and temporal residual correlation structures


## Tuesday (day 2) 
Lecture 3 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day2/lecture_3_slidedeck) | [pdf](https://github.com/nicholasjclark/physalia-forecasting-course/raw/main/day2/lecture_3_slidedeck.pdf))
<br>
Live code examples ([Interactions](https://raw.githubusercontent.com/nicholasjclark/physalia-forecasting-course/main/day2/live_code_examples/live_2_interactions.R) | [Time-varying effects](https://raw.githubusercontent.com/nicholasjclark/physalia-forecasting-course/main/day2/live_code_examples/live_3_timevarying.R))
<br>
Tutorial 2 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day2/tutorial_2_physalia))
* Dynamic GLMs and Dynamic GAMs
* Autoregressive dynamic processes
* Gaussian Processes
* Dynamic coefficient models


## Wednesday (day 3)
Lecture 4 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day3/lecture_4_slidedeck) | [pdf](https://github.com/nicholasjclark/physalia-forecasting-course/raw/main/day3/lecture_4_slidedeck.pdf))
<br>
Live code examples ([Distributed lags](https://raw.githubusercontent.com/nicholasjclark/physalia-forecasting-course/main/day3/live_code_examples/live_4_distributedlags.R) | [Distributed MAs](https://raw.githubusercontent.com/nicholasjclark/physalia-forecasting-course/main/day3/live_code_examples/live_4_distributedmas.R))
<br>
Tutorial 3 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day3/tutorial_3_physalia))
* Bayesian posterior predictive checks
* Forecasting from dynamic models
* Point-based forecast evaluation
* Probabilistic forecast evaluation


## Thursday (day 4)
Lecture 5 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day4/lecture_5_slidedeck) | [pdf](https://github.com/nicholasjclark/physalia-forecasting-course/raw/main/day4/lecture_5_slidedeck.pdf))
<br>
Live code examples ([Functionals](https://raw.githubusercontent.com/nicholasjclark/physalia-forecasting-course/main/day4/live_code_examples/live_5_functional.R) | [Time-varying seasonality](https://raw.githubusercontent.com/nicholasjclark/physalia-forecasting-course/main/day4/live_code_examples/live_6_timevaryingseason.R))
<br>
Tutorial 4 ([html](https://nicholasjclark.github.io/physalia-forecasting-course/day4/tutorial_4_physalia))
* Multivariate ecological time series
* Vector autoregressive processes
* Dynamic factor models
* Multivariate forecast evaluation


## Friday (day 5)
* Group-based practical examples / case studies
* Review, feedback and open discussion
