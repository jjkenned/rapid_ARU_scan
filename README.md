# rapid_ARU_scan

This workflow uses (1) acoustic indices to visually assess background noise sources (eg. wind, rain, traffic, frogs) present in autonomous recording unit (ARU) collected recordings, and (2) creates jpg spectrogram images from selected nights to visually scan for species of interest. 

## Overview 
The steps, with accomanying scripts, are: 

1) Calculate acoustic indices for a (i) transect of ARUs or (ii) an individual ARU 
- PowerShell ~ 1_Acoustic_Indices_By_Station.ps1

2) Rename output files to mimic continuous recording schedules for the formation of Long Duration False Colour Spectrograms (LDFCS) 
- R ~ 2_Rename_Index_output_files.R

3) Create LDFCS visualizations and place in transect-specific folder
- PowerShell ~ 3_Concatenate_for_LDFCS.ps1
- R ~ 4_Bring_LDFCS_Visualizations_to_single_Folder.R

4) Visualize the LDFCS using TimeLapse for (i) removing sessions of poor weather, (ii) choosing sessions of good weather, (iii) assessing background noise within your recording dataset, (iv) looking for wildlife with distincting acoustic signatures
- TimeLapse ~ IndicesProcessing2.tdb

5) Build *jpg spectrograms (using sox) for scanning in search of target vocalizations/species 
- R ~ 5_write_spectrograms_single_frame_SoX.R

6) Scan spectrograms for species of interest 
- TimeLapse ~ CoastOwlsBC.tdb


Additional Scripts:

a) Move all spectrograms with species detections
- R ~ Gather_Spectrograms_with_detections.R


## Acoustic Indices 
