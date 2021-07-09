# S-at-work
An agent based model of job satisfaction in work organization


<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the model](#about-the-model)
  * [Built With](#built-with)
* [Launch Model Simulations](#launch-model-simulations)
  * [Prerequisites](#prerequisites)
  * [Getting started](#getting-started)
* [Details](#details)
* [Future development](#future-development)
* [License](#license)


<!-- ABOUT THE PROJECT -->
## About The model
This model is based on my thesis work - https://hal.archives-ouvertes.fr/hal-01566985 - It is an updated version of the model, once coded using Repast Simphony, while being recoded in a more suitable platform (Gama). Most of references remain the same while improved.

### Built With

* [GAMA-Platform](http://gama-platform.org)

<!-- GETTING STARTED -->
## Launch Model Simulations

To get a local copy of the model and be able to run experiment with it, improve or (freely) reuse.

### Prerequisites

- [GAMA-Platform 1.8.1](https://github.com/gama-platform/gama/releases/tag/1.8.1)

### Getting started

1. First, download and extract the proper GAMA version (see link below, if you don't know which version to take, pick the corresponding OS - windows, linux or macos - and prefer the one with embedded JDK). If you need more information about how to install GAMA, check the [installation page](https://gama-platform.github.io/wiki/Installation)

2. Second, clone the model on your computer (in a terminal go to the desired repository and inpu ``git clone https://github.com/chapuisk/S-at-work.git``) or download the model [on GitHub](https://github.com/chapuisk/S-at-work) (click [here](https://github.com/chapuisk/S-at-work/archive/refs/heads/main.zip) to download it automatically)

2.a Extract that ZIP file somewhere on your computer, if you downloaded it.

3. [import the model in GAMA](https://gama-platform.github.io/wiki/ImportingModels).

4. Enjoy our model running on your computer.

<!-- USAGE EXAMPLES -->
## Details

We describe the context in which to run the proposed simulation experiments in the model

### Dataset

There is several entry for data at initialization: basic demographic variables, workforce, work characteristics and personnality

1. Demographics

**Data sources**
**Data files**

2. Working force dataset:

**Data sources**
* OECD dataset, see https://stats.oecd.org/
* Worldbank data, see https://databank.worldbank.org/reports.aspx?source=jobs
* Eurostats is the best, see https://ec.europa.eu/eurostat/web/main/data/database

**Data files**
- 'file name here' : is about age x sex distribution of employment
- '' : is about working time distribution
- '' : is about wage gap

3. Work characteristic dataset

**Data sources**
**Data files**
- JC-model : empty
- Work-role outcome model : empty

4. Personality dataset

**Data sources**
**Data files**
- Big Five Index (BFI) : empty

### Model init.

1. Data based initialization
2. Parameter based initialization

### Simulation experiments

### ODD

TODO : provide a link to the ODD

<!-- ROADMAP -->
## Future development

See the [current milestone](https://github.com/chapuisk/S-at-work/milestones) for a list of proposed features (and known issues).

<!-- LICENSE -->
## License

This project is distributed under the GPL-3.0 License. See [LICENSE](https://github.com/COMOKIT/COMOKIT-Model/blob/master/LICENSE) for more information.
