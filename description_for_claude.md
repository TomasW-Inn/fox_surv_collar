This project aim to analyze mortality causes and survival (hazard) rates of red foxes in Sweden and Norway that has been captured and collared with GPS-collars. 

DATA:
The main data is in the file df_fox_new.csv, and have the columns:

FoxID: Name in text. If recapture the number 2 is added to the name.
CAUSE: Faith of the aninmal during the study
	#0 Alive when the study ended
	#1 Harvested
	#2 
	#3 Vehichle
	#4 Disese
	#5 Predation
	#6 Stress/Malnutrition
	#7 
	#8 Collar failure
	#9 Battery life
ID: Unique id number	
SEX: Sex of the fox	
AGE: Age of the fox at capture
	# AD = adult
	# SA = subadult less than one year	
AREA: Which part of the study are	
date.begin: Date when captured yyyy-mm-dd	
date.end:   Date when left the study yyyy-mm-dd		
CENS: 
	# 0 left the syudy without known mortality cause
	# 1 left the study with known mortality cause
	
PREVIOUS PRELIMINARY ANALYSIS:
Preliminary analysis are described in Preliminary_Analysis.pdf. 
I would llike to start fresh and redo the analysis from scratch.

QUESTIONS:
1. What can we conclude about mortality causes, and compare that with previous studies in Sweden and elsewhere.

2. What can we conclude from an analysis of survival(hazard) rates? Previouslyy we have used the cox-proportional hazard model and following the Anderson-Gill approach. 

"We used the Andersen-Gill modification of the CPH regression model (Andersen & Gill, 1982) similar to (Johnson et al., 2004; Åhlen et al., 2013). The non-parametric Kaplan-Meier estimator was used to plot and inspect the survival curves  (Pollock et al., 1989). We determined if an individual fox was present at each week in the study, and added those weeks to the data. Thus, each individual and each week became a row in the data. The last week a fox was present was either a known mortality or a censored observation due to collar drop-off or failure. All weeks the fox was present before the last was recorded as censored. Only weeks when we knew the status of a fox were included, for example the weeks during the period between the six recaptures were excluded. We pooled areas and years due to low sample size, and set the start of a red fox year to week 36 in autumn when cubs would have separated from their litters. We considered sub-adults to become adults after week 35 in their second year, and reclassified nine sub-adults to adults after week 35 in their second year. Eight sub-adults (3 females and 5 males) that contributed less than 2 weeks to data were excluded from the analysis."
- Andersen PK, Gill RD (1982) Cox’s Regression Model for Counting Processes: A Large Sample Study. The Annals of Statistics, 10. https://doi.org/10.1214/aos/1176345976
- Johnson CJ, Boyce MS, Schwartz CC, Haroldson MA (2004) MODELING SURVIVAL: APPLICATION OF THE ANDERSEN–GILL MODEL TO YELLOWSTONE GRIZZLY BEARS (Cypher, Ed,). Journal of Wildlife Management, 68, 966–978. https://doi.org/10.2193/0022-541X(2004)068[0966:MSAOTA]2.0.CO;2
- Åhlen P-A, Willebrand T, Sjöberg K, Hörnell-Willebrand M (2013) Survival of female capercaillie Tetrao urogallus in northern Sweden. Wildlife Biology, 19, 368–373. https://doi.org/10.2981/13-025
- Pollock KH, Winterstein SR, Bunck CM, Curtis PD (1989) Survival Analysis in Telemetry Studies: The Staggered Entry Design. The Journal of Wildlife Management, 53, 7–15. https://doi.org/10.2307/3801296

Is this a good approach or is there better ways to analyze the data? Would it be better to set up a bayesian model in jags?

AUXILLY INORMATION:

I have added papers and manuscripts of our previous work on red fox:
- Walton Z, Samelius G, Odden M, Willebrand T (2017) Variation in home range size of red foxes Vulpes vulpes along a gradient of productivity and human landscape alteration (MS Crowther, Ed,). PLOS ONE, 12, e0175291. https://doi.org/10.1371/journal.pone.0175291
- Willebrand T, Samelius G, Walton Z, Odden M, Englund J (2022) Declining survival rates of red foxes Vulpes vulpes during the first outbreak of sarcoptic mange in Sweden. Wildlife Biology, wlb3.01014. https://doi.org/10.1002/wlb3.01014
- Latitudinal effects on weight and age dependent survival of Red fox Vulpes vulpes. Manuscript.







