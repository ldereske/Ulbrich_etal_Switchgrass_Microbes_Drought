# Description of data with column names and metadata



### phyl_obj_SILVA_MERDS.RData

###### Raw bacterial OTU table, Taxonomy, phylogenetic tree, and metadata from the greenhouse experiement in the form of a phyloseq object


### SILVA_MERDS_rar_phylo_obj.RData

###### processed OTU table, Taxonomy, phylogenetic tree, and metadata in the form of a phyloseq object generated at line 101 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### Indicator_spp_live_no_start_seed_comm_association.RData

###### indicator species of each treatment in the final soils in the form of Rdata generated at line 623 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### Weight_Uni_SILVA_MERDS_rar_NS_dist.csv

###### Weighted unifrac distance across all samples generated at line 445 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### SILVA_MERDS_rar_NS_map.csv

###### Metadata matching order of Weight_Uni_SILVA_MERDS_rar_NS_dist.csv generated at line 443 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### trans_SILVA_MERDS_rar_live_NS_map.csv

###### Metadata matching order of trans_Weight_Uni_SILVA_MERDS_rar_live_NS_dist.csv generated at line 546 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### trans_Weight_Uni_SILVA_MERDS_rar_live_NS_dist.csv

###### Weighted unifrac distance across seedlings samples generated at line 548 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### seed_SILVA_MERDS_rar_live_NS_map.csv

###### Metadata matching order of seed_Weight_Uni_SILVA_MERDS_rar_live_NS_dist.csv generated at line 672 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### seed_Weight_Uni_SILVA_MERDS_rar_live_NS_dist.csv

###### Weighted unifrac distance across germinants samples generated at line 674 of Ulbrich_etal_Greenhouse_Bacterial_comm_structure_Final.RMD


### SG_trt_sheet.csv


###### Treatment data for the greenhouse experiment. 

+ Plant_Number = the pot number
+ raw_label = the raw label with short code of the treatments
+ soil_status = status of soils was if they were L=alive (not autoclaved) S=sterile (autoclaved)
+ root_association = where the soil was originally sampled B=bulk (dug with a shovel in patchs without Switchgrass) R=rhizosphere soils (soils shook from live plant daug up in the plot)
+ block = pots were blocked by the plant that they recieved soil from and location in the greenhouse)
+ precip = A=ambient precipitation (3.4 mL per day 1997-2017 May to July average monthly precipitation = 93.15 mm) or D=drought precipitation (3.4 mL every three days 32.85 mm per month based of over the 2012 drought)
+ life_stage = pot recieved either S=germinants (10 per pot) or G=seedlings transplanted from sterile sand/vermiculite
+ trt = the raw label with short code of the treatments with pot number removed

### SG_analyzed_MBC_DOC.csv

###### Soil and microbial carbon and nitrogen from the harvest of the greenhouse experiement.

+ unfumigated_DOC_concentration_ugC_gsoil = ug Dissolved Organic Carbon per g dry soil for unfumigated samples (K2SO4 extracts)
+ unfumigated_TN_concentration_ugN_gsoil = ug Total Nitrogen per g dry  soil for unfumigated samples (K2SO4 extracts)
+ fumigated_TN_concentration_ugN_gsoil = ug Dissolved Organic Carbon per g dry soil for fumigated samples (K2SO4 extracts)
+ fumigated_DOC_concentration_ugC_gsoil = ug Total Nitrogen per g dry soil soil for fumigated samples (K2SO4 extracts)
+ ugC_MicBiomass_g_dry_soil = ug Carbon in microbial biomass (fumigatedC-unfumigatedC/.45) per g dry soil; Neg. Values = NA
+ ugN_MicBiomass_g_dry_soil = ug Nitrogen in microbial biomass (fumigatedN-unfumigatedN/.54) per g dry soil; Neg. Values = NA


### SG_start_NO3NH4_ug_gdrysoil.csv


###### Soil inorganic nitrogen from the initial soil inoculum for the greenhouse experiement.

+ ug_N_NO3_g_dry_soil = NO3 value from K2SO4 soil extracts (ug N/g dry soil) 
+ ug_N_NH4_g_dry_soil = NH4 value from K2SO4 soil extracts (ug N/g dry soil) 
+ NO3ppm = NO3 ppm from K2SO4 extracts
+ NH4ppm = NH4 ppm from K2SO4 extracts
+ extraction_vol = 30mL k2so4 used to extract soils 
+ Unfumigated_soil_g = field wet weight of soil (g) used for unfumigated extracts (unfumigated extracts used for NO3 and NH4 assays)
+ Tin_wt_g = Tin weight (g) prior to adding soils
+ Wetsoil_wt_g = Amount of wet soil added to tin (g) 
+ Drysoil_tin_wt_g = Final dry weight of soil (g) after drying - used to calculate soil moisture content 
+ Notes = notes from observer
+ percent_soil_moisture_dry_weight = (Wetsoil_wt_g -Drysoil_wt_g)/Drysoil_wt_g
+ percent_soil_moisture_fresh_weight = (Wetsoil_wt_g -Drysoil_wt_g)/WetSoil_wt_g
+ ratio_dry_soil_to_fresh_soil = Drysoil_wt_g/Wetsoil_wt_g
+ massMoist  = g of moist soil extracted with 0.5M K2SO4 
+ soil_status = status of soils was if they were L=alive (not autoclaved) S=sterile (autoclaved)
+ root_association = where the soil was originally sampled B=bulk (dug with a shovel in patchs without Switchgrass) R=rhizosphere soils (soils shook from live plant daug up in the plot)
+ block = pots were blocked by the plant that they recieved soil from and location in the greenhouse)
+ precip = A=ambient precipitation (3.4 mL per day 1997-2017 May to July average monthly precipitation = 93.15 mm) or D=drought precipitation (3.4 mL every three days 32.85 mm per month based of over the 2012 drought)
+ life_stage = pot recieved either S=germinants (10 per pot) or G=seedlings transplanted from sterile sand/vermiculite


### SG_NO3NH4_ug_gdrysoil.csv

###### Soil inorganic nitrogen from the harvest of the greenhouse experiment. 

+ Plant_Number = the pot number
+ ug_N_NO3_g_dry_soil = NO3 value from K2SO4 soil extracts (ug N/g dry soil) 
+ ug_N_NH4_g_dry_soil = NH4 value from K2SO4 soil extracts (ug N/g dry soil) 
+ ug_N_NH4_g_dry_soil_negto0 = NO4 value from K2SO4 soil extracts (ug N/g dry soil) - changed all negative values to 0 
+ NO3ppm = NO3 ppm from K2SO4 extracts
+ NH4ppm = NH4 ppm from K2SO4 extracts
+ NH4ppm_flag_neg = flag = 1 if NH4ppm is negative
+ NH4ppm_negto0 = converted all negative NH4 ppm values into 0 
+ unfum_weight_g = field wet weight of soil (g) used for unfumigated extracts (unfumigated extracts used for NO3 and NH4 assays)
+ fum_weight_g = field wet weight of soil (g) used for fumigated extracts
+ Tin_wt_g = Tin weight (g) prior to adding soils
+ Wetsoil_wt_g = Amount of wet soil added to tin (g) 
+ Drysoil_tin_wt_g = Final dry weight of soil (g) after drying - used to calculate soil moisture content 
+ percent_soil_moisture_dry_weight = (Wetsoil_wt_g -Drysoil_wt_g)/Drysoil_wt_g
+ percent_soil_moisture_fresh_weight = (Wetsoil_wt_g -Drysoil_wt_g)/WetSoil_wt_g
+ ratio_dry_soil_to_fresh_soil = Drysoil_wt_g/Wetsoil_wt_g
+ extraction_vol = 30mL k2so4 used to extract soils 


### SG_roottraits.csv

###### Root trait data from the greenhouse experiement measured using GiA Roots.

+ Plant_Number = the pot number	
+ Rhizosheath_WaterControl_g = weight of 50mL tube +20mL H2O - unique per sample
+ Rhizosheath_WaterSoil_g	= weight of 50mL tube +20mL H2O + rhizosheath soil that fell off during vortex
+ WetRoots_g	DryRoots_g = g of wet SG roots 
+ AverageRootWidth_Diameter = g of dry SG roots 
+ NetworkWidth = GiaRoots Root Trait output
+ NetworkArea	= GiaRoots Root Trait output
+ GiaRoots_SpecificRootLength = GiaRoots Root Trait output
+ NetworkLength = GiaRoots Root Trait output
+ NetworkVolume = GiaRoots Root Trait output
+ RhizosheathSoil_g = Rhizosheath_WaterSoilg - Rhizosheath_WaterControl_g
+ RhizosheathSoil_WetRoots_g = RhizosheathSoil_g / WetRoots_g
+ RhizosheathSoil_DryRoots_g = RhizosheathSoil_g / DryRoots_g
+ SRL_length_drymass = NetworkLength / DryRoots_g
+ RLD_length_volume = NetworkLength / 158 (pot volume in mL) 


### SG_height_time_combine.csv


###### Plant data for surveys conducted during the greenhouse experiment.

+ Plant_Number= the pot number
+ height_cm= height in cm
+ leaf_num = number of leaves
+ coverscale_1_5 = percent of leaf that is shriveled or necrotic (scale needed)
+ notes = notes from observer
+ date= date of the given measurement m_dd
+ observer= person who measure the given pot or date
+ exp_period= period within the experiment (weekly_sur= weekly survey by a given observer, harvest= harvest of the 
+ experiment on 7/9/2018, planting= planting of the experiment on 6/12/2018, replacement= replacement of dead 
+ transplants in the first week)
+ date_raw = date in the mm/dd/yyy format
+ surv = observer determined dead plants 1 = alive 0 = dead, only for the harvest period


### SG_surv_transplant_replace.csv

###### Survival of seedlings for surveys conducted during the greenhouse experiment.

+ Plant_Number= the pot number
+ date= date of the given measurement m_dd
+ Rdate= date in the mm/dd/yyy format
+ survival= survival of transplant 1= alive and 0=dead
+ Transplant_height_cm= height in cm of plant used to replace a dead seedling
+ Transplant_leaf= number of leaves of plant used to replace a dead seedling
+ volunteer_dicot = number of dicots/volunteers removed from the pot
+ volunteer_grass = number of grass/volunteers removed from the pot


### SG_surv_Seed_germ.csv

###### Germination data for surveys of germinants conducted during the greenhouse experiment.

+ Plant_Number= the pot number
+ date= date of the given measurement m_dd
+ Rdate= date in the mm/dd/yyy format
+ exp_days = days since beginning of experiment
+ num_germinates = number of germinants on given day
+ volunteer_dicot = number of dicots/volunteers removed from the pot
+ num_removed = number of switchgrass seedlings removed on a given day to return the pot to one seedling (we plant 10 seeds)


### SG_TotalBiomass.csv

###### Biomass of plants at harvest of the greenhouse experiement. 

+ Plant_Number= the pot number
+ root_weight_g= weight of the harvested root in g
+ shoot_weight_g= weight of the harvested shoot in g
+ total_biomass= root_weight_g+shoot_weight_g
+ notes_roots=notes from oberserver weighing roots
+ notes_shoots=notes from oberserver weighing shoots
+ sandy_root= samples that the person weighing flagged as having a lot of sand attached to the roots


### SG_planting_data.csv

###### Data on the initial plantings of seedlings from the greenhouse experiement.

+ Plant_Number = the pot number	
+ height_cm = height in cm
+ leaf_num = number of leaves	
+ Notes = notes from oberserver measuring plantings.
+ Observer = person who measured and sowed the given pot


### rep_seq_16s_MERDS.fna

###### representative sequences for the bacterial OTUs used in the greenhouse experiment.

### MERDS_GerminationExperiment_Rerun_RAWdata.csv

###### Germination and biomass data from the growth chamber experiment 

+ ID = the Petri dish number	
+ Day1 to Day12	= columns with the number of germinants recorded each day
+ TotalGerminant_shoot_dry_g = combined biomass of the germnants
+ Number_NonSG_germinants = number of volunteer germinant from the sed bank of soil inoculum
+ FirstGermination_Day = day of first recorded germinant
+ NOTES = notes from researcher

### MERDS_GerminationExperiment_Rerun_Metadata.csv

###### Petri dish metadata from the growth chamber experiment.

+ ID = the Petri dish number
+ Soil_Treatment = status of soils was if they were Live (live soil added) or Sterile (only sterile play sand)
+ Nitrogen_Treatment = level of ammonium added to the Petri were High 9 µg NH4+ g-1 dry soil or Low 1 µg NH4+ g-1 dry soil
+ sand_g = ammount of dry sterile play sand in the Petri dish
+ inocula_g	= ammount of dry live soil added to the Petri dish
+ Placement	= placement of the Petri dish on each shelf in the growth chamber was Front, Middle, or Back
+ Shelf = shelf placement of the Petri dish in the growth chamber was Top or Bottom
