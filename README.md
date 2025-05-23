# An Analysis of Water Transparency in the Great Lakes

Here, we analyze water transparency in the great lakes (as measured by UV-B and PAR) as a function of Dissolved Organic Carbon (DOC).  We also look at water body types, seasons and long-term trends as potential influences.

A comprehensive backward stepwise selection procedure is implemented beginning with a full interaction model (interaction of DOC with Lake, season, water body type and presence/non-presence of a river). We then remove terms beginning with the most complicated interaction terms and working our way down to main effects.  

The code to perform the stepwise modeling is in `stepwise_lmer_code.R`.

The data needed to perform the analysis is in the two folders:

* `edi.1321.6` folder contains the 1% UV depth & 1% PAR depth measures
* `other_data_sources` contains information on the sites, their BlagraveID table, DOC measures and the Kd_calculations.

The analysis can be repeated by running the files in their named (numeric) order.

1. `01_data_processing.R` - inputs all the sources of data and merges them into a single dataset we use for analysis.
2. `02_eda_trends.R` - provides plots of the raw data in time and the relationship between PAR and UV with DOC.  Some of the plots includes loess smoother fits to plot any trends.
3. `03_map_of_data_sites.R` - this code builds the map included in the manuscript.
4. `04_modeling_doc.R` - this code performs the backward selection to model DOC as a response with a potential time influence, as well as Lake, Season, Waterbody type and presence of river. The selected mixed effects model includes the interaction of Season & Waterbody type, along with a linear trend in time. Some plots are made exploring the fitted model.
5. `05_modeling_uvm.R` - this code performs the backward selection to model 1% UV depth. The selected mixed effects model includes the interaction of Season & Waterbody type, DOC and Waterbody type, and Season & presence of River. Some plots are made exploring the fitted model.
6. `06_modeling_par.R` - this code performs the backward selection to model 1% PAR depth. The selected mixed effects model includes the interaction of DOC with WaterBody type, and includes Lake and Season as main effects. Some plots are made exploring the fitted model.

Lastly, the repository also includes the files

* `supplementalReport.qmd` which is a Quarto document building our supplemental report.
* `supplementalReport.html` a *rendered* version of the Quarto document into html format.
* `supplemntalReport.docx` a *rendered* version of the Quarto document in Microsoft Word format.

