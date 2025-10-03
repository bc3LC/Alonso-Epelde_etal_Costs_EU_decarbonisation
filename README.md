# A high-resolution analysis of who bears the costs of EU decarbonisation

Eva Alonso-Epelde<sup>1*</sup>, Xaquín Garcia-Muros<sup>1,2</sup>, Jon Sampedro<sup>1,2</sup>, Clàudia Rodés-Bachs<sup>1</sup>, Natasha Frilingou<sup>3</sup>, and Dirk-Jan Van de Ven<sup>1</sup>, 

<sup>1 </sup> Basque Center for Climate Change, Leioa, Spain

<sup>2 </sup> IKERBASQUE, Basque Foundation for Science, Plaza Euskadi 5, 48009 Bilbao, Spain

<sup>3 </sup> National Technical University of Athens, Athens, Greece

\* corresponding author:  eva.alonso@bc3research.org

## Abstract
Addressing climate change with the necessary urgency will only be successful if climate policies are perceived as socially fair and equitable by the majority of the population. However, as recognised in the latest IPCC report, energy and climate modelling still struggles to bring together detailed economic and inequality impacts of climate policies. To help to reduce this gap, we link GCAM-Europe, a geographical expansion of a well-established integrated assessment model, with MEDUSA, a tool for high-resolution distributional analysis.  Our analysis explores how different implementations of the European Union (EU)'s climate policy portfolio affect various consumer groups across and within Member States. It finds that a general EU-wide carbon price, while cost-efficient, has regressive impacts, disproportionately burdening low-income Member States and households. In contrast, policies based on national plans (NECPs) are less regressive. Gender and the urban-rural dimension also play an important role, being man-headed and rural households the most affected.

## Code reference
Available at Zenodo: ADD Zenodo Link

ADD Code Citation


## Contributing modeling software
| Model | Version | Repository Link 
|-------|---------|-----------------
| Global Change Analysis Model Europe (GCAM-Europe) | 7.2 | [https://doi.org/10.5281/zenodo.15655568](https://zenodo.org/records/15655568) | 
| Medusa | 2.0 | [upcomming release](https://github.com/bc3LC/medusa) | 

| Component| Version | Repository Link 
|-------|---------|-----------------
| gcamdata | 1.0 | https://github.com/JGCRI/gcamdata | 
| rgcam | 1.2.0 | https://github.com/JGCRI/rgcam | 
| pridr | 0.1.0 | https://github.com/JGCRI/pridr | 
| rmap | 1.0.0 | https://github.com/JGCRI/rmap | 

## Reproduce the experiment
To reproduce the results and figures shown in Alonso-Epelde et al.,

1. Install `R` (https://www.r-project.org/).
2. Install `R studio` (https://www.rstudio.com/).
3. Run the script called `R/paper_figures.R` to generate the figures and reproduce the analysis.

## Acknowledgments
This project has received funding from the European Union's Horizon 2020 research and innovation program under grant agreement numbers 101056306 (IAM COMPACT project) and 101081179 (DIAMOND project).
