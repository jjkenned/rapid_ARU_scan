# rapid_ARU_scan

This workflow uses (1) acoustic indices to visually assess background noise sources (eg. wind, rain, traffic, frogs) present in autonomous recording unit (ARU) collected recordings, and (2) creates jpg spectrogram images from selected nights to visually scan for species of interest. 


___All script, programs and code are freely available and opensource.___


## Overview 
The steps, with accompanying scripts, are: 

1) Calculate acoustic indices for a (i) transect of ARUs or (ii) an individual ARU 
- PowerShell ~ __1_Acoustic_Indices_By_Station.ps1__

2) Rename output files to mimic continuous recording schedules for the formation of Long Duration False Colour Spectrograms (LDFCS)
- R ~ __2_Rename_Index_output_files.R__

3) Create LDFCS visualizations and place in transect-specific folder
- PowerShell ~ __3_Concatenate_for_LDFCS.ps1__
- R ~ __4_Bring_LDFCS_Visualizations_to_single_Folder.R__

4) Visualize the LDFCS using TimeLapse for (i) removing sessions of poor weather, (ii) choosing sessions of good weather, (iii) assessing background noise within your recording dataset, (iv) looking for wildlife with distincting acoustic signatures
- __TimeLapse ~ IndicesProcessing2.tdb__

5) Build *jpg real time spectrograms (RTS) for scanning in search of target vocalizations/species 
- R ~ __5_write_spectrograms_single_frame_SoX.R__

6) Scan spectrograms for species of interest 
- TimeLapse ~ __CoastOwlsBC.tdb__


Additional Scripts:

a) Move all spectrograms with species detections
- R ~ __Gather_Spectrograms_with_detections.R__






## References 

__QUT Ecoacoustics__ ~ [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4274299.svg)](https://doi.org/10.5281/zenodo.4274299)

Acoustic indices are calculated and LDFCS are created by the Ecoacoustics 'Analysis Programs' code. 
- To download the appropriate files to run this code you can visit the **[QutEcoacoustics Github](https://github.com/QutEcoacoustics/audio-analysis)** page and to learn more about their research, visit the __[Ecosounds](https://www.ecosounds.org/)__ website. 


> Michael Towsey, Anthony Truskinger, Mark Cottman-Fields, & Paul Roe. (2020, November 15). QutEcoacoustics/audio-analysis: Ecoacoustics Audio Analysis Software v20.11.2.0 (Version v20.11.2.0). Zenodo. http://doi.org/10.5281/zenodo.4274299




__Timelapse__ ~ [Greenberg et al. (2019)](https://doi.org/10.1002/ece3.5767)

All visual assessment of spectrograms, both LDFCS and RTS, is completed in the Timelapse program
- For all information on this program, including downloads, instructions and background, visit the __[Timelapse website](https://saul.cpsc.ucalgary.ca/timelapse/)__. 


> Saul Greenberg, Theresa Godin, and Jesse Whittington. "Design patterns for wildlife‐related camera trap image analysis." Ecology and Evolution 9.24 (2019): 13706-13730. https://doi.org/10.1002/ece3.5767








































