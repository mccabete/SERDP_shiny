# RC-2636 Decision Support Tool User Guide

Updated on 8/27/2021

Tempest McCabe, Whalen Dillion, Michael C. Dietze, Drew Hiatt, L. Page Fredericks, Allison Gardner, Luke Flory, and Brian Allan

**Statement of Need**

Reported cases of tick-borne diseases (TBD) reported in the United States (US) more than doubled from 2004-2016 (Rosenberg et al., 2018). In the southeastern US, the Gulf Coast tick (Amblyomma maculatum) and the lone star tick (A. americanum) have both undergone dramatic range expansions in the past 50 years (Sonenshine, 2018). Prescribed burning is a common management practice in the southeastern US that has been found to decrease tick populations in the short and long-term, and reduce TBD risk (Gleim et al., 2014, 2019). However, prescribed burns are primarily planned around other management objectives like forest productivity, fire risk reduction, and conservation. Reducing tick populations is likely an unanticipated benefit to existing practices. However, landscape features other than fire also affect tick populations. Humidity, tick habitat availability, and the overstory, are all likely to change fire’s efficacy as a control mechanism. In areas where burns are infeasible or low-priority, other reduction mechanisms could be utilized.

To quantify these relationships and thereby better inform fire and tick management practices, we measured the tick populations, tick hosts, vegetation, and recorded fire histories at nine Department of Defense (DoD) installations in the southeastern US. We then statistically estimated how each landscape feature relates to tick populations. This Decision Support Tool (DST) uses these data and estimates in order to:

1)	Report each installation’s tick-borne disease risk
2)	Allow installations to quantify the extent to which existing fire management reduces tick populations
3)	Explore how alternative fire management scenarios may affect tick populations
4)	Explore the efficacy of non-burning tick management strategies


**State of the field**

A number of DSTs exist already pertaining to the implementation of prescribed fire, such as: FARSITE (Finney, 2004), SPITFIRE (Thonicke et al., 2010), First Order Fire Effects model (Hood & Lutes, 2017),  FFI (Lutes et al., 2009), Interagency Fuels Treatment Decision Support System (Wells et al., 2009), or planting pines PINEMAP (T. A. Martin, 2019).  However, these management decisions often are made in isolation from decisions concerning efforts to mitigate tick-borne disease risk, for which there are no established DSTs at present to our knowledge.
	
**Intended Audience** 

This decision support tool is based on data collected from DoD sites. However, the DoD installations represent a large range of latitudes, soil types, and forests of the southeastern US. Estimates of tick-borne disease risk (Goal 1), may not correspond to levels of disease risk nearby. However, the underlying relationships between prescribed burns and ticks may be useful to public and private land managers to explore how burn frequency affects ticks (Goals 2-4). 

**Access and Installation Instructions**

To see the app navigate to: https://serdp2636.shinyapps.io/serdp2636/
The underlying code that generates the app is at: https://github.com/mccabete/SERDP_shiny/tree/main/code

Our tool requires an internet browser to access, and no other software to run. Graphics can appear differently depending on the browser. Data downloaded from our tool may require software to be opened or edited. We provide the options to download comma separated values or `.csv` files. 


**Summary of Features:** 

(Tabs with an * are still in development)

_Tick-Borne Disease_ 
-	Disease Risk Map: Map of Tick-Borne Disease risk (Risk of pathogen exposure per 24 hours), Tick abundance, and Pathogen prevalence at each of the DoD installations. Data available for download.  
-	Tick Pathogens: Searchable & downloadable database of pathogens that were detected at each base, and a key to diseases the pathogens cause. 
-	Tick Hosts: Searchable & downloadable database of animal hosts detected at each installation. 

_Vegetation_
-	Litter: Summary figures of litter depth and percent cover per installation. Data available for download. 
-	Canopy cover: Summary of figures of percent canopy cover per installation. Data available for download


_Exploring Hypotheticals_
-	Project Tick Populations: Project tick populations with new fire regimes, levels of vegetation biomass, canopy cover, and leaf litter. Figures available for download. 
-	Multiple Interacting Predictors of Tick Populations*: Project how changes to fire frequency affect litter, canopy cover, vegetation biomass, and ticks. Or, project how simultaneous changes to any of those variables affects tick populations. Figures available for download. 



**Example usage**

Please see the "wiki" page for specific usage examples. 

**How to Report a Problem or Request a Feature**

Let us know about any issues either by submitting a bug report on Github or by emailing Tempest McCabe at tmccabe@bu.edu. 
-	Submit a bug report by navigating to https://github.com/mccabete/SERDP_shiny/issues, and selecting a “new issue”. Please include a quick title summarizing the problem, and then in the main body describe the issue and how one could re-create it. If you email tmccabe@bu.edu, include the same details. 
-	You can request a feature the same ways! Instead of describing the problem, describe what you would like to see implemented, and how you would use the new feature. 

**Using The Data from this Tool**

The datasets that are downloadable from this tool are summarized versions of the datasets we collected. We ask that publications only incorporate the data from this tool with explicit permission from the RC-2636 team. Please contact Tempest McCabe tmccabe@bu.edu or Brian Allan ballan@illinois.edu if you are interested in collaboration. 


**License**

The data for this project are licensed Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

The underlying code used to display, format, and extrapolate from data is licensed under the MIT license. To view a copy of this license, visit https://opensource.org/licenses/MIT. 


**Acknowledgements**

LF, BA, AG, and MCD conceived of the project and acquired the funding. WD, AG, DH, designed data collection. WD, DH, SC, collected data at DoD installations. BA and LPF conducted pathogen analyses. BA and TDM interviewed partners to inform the design of this app. WD conducted statistical analyses. MCD advised statistical implementation of the app. TDM designed and implemented the app. 

This work was possible because of  a grant from the Strategic Environmental Research and Development Program (SERDP) Project RC-2636. We referenced Alessio Benedetti’s Biodiversity In National Parks shiny app https://github.com/abenedetti/bioNPS/, during the course of our app design. Special thanks to our DoD installation partners. 


**Citations**

Chang, W., Cheng, J., Allair, J., Xie, Y., & McPherson, J. (2019). shiny: Web Application Framework for R (R package version 1.4.0) [Computer software]. https://CRAN.R-project.org/package=shiny

Finney, M. A. (2004). FARSITE : Fire Area Simulator—Model Development and Evaluation (RMRS-RP-4).

Gleim, E. R., Conner, L. M., Berghaus, R. D., Levin, M. L., Zemtsova, G. E., & Yabsley, M. J. (2014). The Phenology of Ticks and the Effects of Long-Term Prescribed Burning on Tick Population Dynamics in Southwestern Georgia and Northwestern Florida. PLoS ONE, 9(11), e112174–e112174. https://doi.org/10.1371/journal.pone.0112174

Gleim, E. R., Zemtsova, G. E., Berghaus, R. D., Levin, M. L., Conner, M., & Yabsley, M. J. (2019). Frequent Prescribed Fires Can Reduce Risk of Tick-borne Diseases. Scientific Reports. https://doi.org/10.1038/s41598-019-46377-4

Hiers, J. K., Laine, S. C., Bachant, J. J., Furman, J. H., Greene, W. W., & Compton, V. (2003). Simple Spatial Modeling Tool for Prioritizing Prescribed Burning 
Activities at the Landscape Scale. Conservation Biology, 17(6), 1571–1578. https://doi.org/10.1111/j.1523-1739.2003.00381.x

Hood, S., & Lutes, D. (2017). Predicting Post-Fire Tree Mortality for 12 Western US Conifers Using the First Order Fire Effects Model (FOFEM). Fire Ecology, 13(2), 66–84. https://doi.org/10.4996/fireecology.130290243

Lutes, D. C., Benson, N. C., Keifer, M., Caratti, J. F., & Streetman, S. A. (2009). FFI: A software tool for ecological monitoring. International Journal of Wildland Fire, 18(3), 310. https://doi.org/10.1071/WF08083

Martin, K. L., Hurteau, M. D., Hungate, B. A., Koch, G. W., & North, M. P. (2015). Carbon Tradeoffs of Restoration and Provision of Endangered Species Habitat in a Fire-Maintained Forest. Ecosystems, 18(1), 76–88. https://doi.org/10.1007/s10021-014-9813-1

Martin, T. A. (2019). PINEMAP: Mapping the Future of Southern Pine Management in a Changing World. Forest Resources and Conservation Speaker Series.

R Core Team. (2019). R: A Language and Environment for Statistical Computing [R]. R Foundation for Statistical Computing. https://www.R-project.org/

Rosenberg, R., Lindsey, N. P., Fischer, M., Gregory, C. J., Hinckley, A. F., Mead, P. S., Paz-Bailey, G., Waterman, S. H., Drexler, N. A., Kersh, G. J., Hooks, H., 

Partridge, S. K., Visser, S. N., Beard, C. B., & Petersen, L. R. (2018). Morbidity and Mortality Weekly Report Vital Signs: Trends in Reported Vectorborne Disease Cases—United States and Territories, 2004–2016. 67(17), 496–501.

Sonenshine, D. E. (2018). Range expansion of tick disease vectors in north america: Implications for spread of tick-borne disease. International Journal of Environmental Research and Public Health, 15(3), 1–9. https://doi.org/10.3390/ijerph15030478

Thonicke, K., Spessa, A., Prentice, I. C., Harrison, S. P., Dong, L., & Carmona-Moreno, C. (2010). The influence of vegetation, fire spread and fire behaviour on biomass burning and trace gas emissions: Results from a process-based model. Biogeosciences, 7(6), 1991–2011. https://doi.org/10.5194/bg-7-1991-2010

Wells, G., Rohling, K., Kapus, J., & Swedberg, T. (2009). A Powerful new Planning environment for Fuels Managers: The interagency Fuels treatment Decision Support System. Fire Science Digest, 7.
