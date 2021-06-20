# README #

This repository stores the code to replicate the simulations in:

# Tax Evasion on a Social Network #

Duccio Gamannossi degl'Innocenti and Matthew D. Rablen

Reference:

Gamannossi degl'Innocenti, Duccio and Matthew D. Rablen. "[Tax Evasion on a Social Network](https://www.sciencedirect.com/science/article/pii/S0167268119303440)", Journal of Economic Behavior & Organization, 169, 79-91.

### Description of files in the Repo


* Figure_1_kappa_vs_revenues_baseline_pa.R 				:   script to replicate Figure 1

* Figure_2_kappa_vs_revenues_various_pa.R 				:   script to replicate Figure 2

* Figure_3_kappa_vs_revenues_ab.R	 			          :   script to replicate Figure 3
 
* license.txt							: 	license applying to the files in the repo	

* package_setup.R					:   script to install needed packages

* revenues_ab.R			      :   script to compute revenues gains from network information when preferences are imperfectly observed

* revenues_pa.R						:   script to compute revenues gains from network information for different values of preferential attachment


+ **data**								: 	folder containing results from revenues_ab.R and revenues_pa.R
  * revenues_ab...        : data from the script revenues_ab.R
  * revenues_pa...        : data from the script revenues_pa.R


+ **fig**									: 	folder containing figures produced with the scripts
	* Figure_1.pdf			    :   pdf of Figure 1
	* Figure_1.tex			    :   tikZ of Figure 1
	* Figure_2.pdf			    :   pdf of Figure 2
	* Figure_2.tex		    	:   tikZ of Figure 2
	* Figure_3.pdf		    	:   pdf of Figure 3
	* Figure_3.tex		    	:   tikZ of Figure 3


+ **R**									: 	folder containing functions used by the scripts
	
	* fun.R					        :   script storing all the functions needed to run the scripts
	* generate_BB_mod.R 	  : 	modified version of PAFit::generate_BB allowing for supplied fitness vector
	* parameters_ab.R 		  : 	parameters used in revenues_ab.R
	* parameters_opt.R 		  : 	parameters for optimization of p (used in both revenues_ab.R and revenues_pa.R)
	* parameters_pa.R 	  	: 	parameters used in revenues_pa.R


### Information

Figure 1 and Figure 2 are based on the results of revenues_pa.R 

Figure 3 is based on the results of revenues_ab.R

The scripts revenues_ab.R and revenues_pa.R are computationally heavy but in **/data** are stored their results so that the figures can be reproduced without running the simulations 

### Instructions

In order to set up the working environment:

1. Start a new R-project in an empty folder
2. Copy the content of the repo to the project folder
3. Run the script package_setup.R to install the needed packages
4. Run any of the scripts


The scripts have been tested on Win 10, R-3.5.1, RStudio-1.2.1335

The files are distributed under BSD license. For more information, please check the license.txt file.

For any question, suggestion or comment, write to: mail@dgdi.me