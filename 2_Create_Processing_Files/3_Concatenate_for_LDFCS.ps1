







# $group = "MKSC-01" # station to station basis at this point

# set in and out directories 
<<<<<<< HEAD
$parent_input_dir = "F:\PMRA_SAR\Processing\indices\BIRD\2023\MKVI\MKVI-22\new_name_indices" 
$input_directories = Get-Childitem -Path "$parent_input_dir"
$output_directory = "F:\PMRA_SAR\Processing\indices\BIRD\2023\MKVI\MKVI-22\by_night" # output directory 
=======
$base_dir = "S:\ProjectScratch\398-173.07\PMRA_WESOke\PMRA_SAR\Processing"
$transect = "MKSC-01"
$region = $transect.Substring(0,4)
$year = "2023"
$species = "BIRD"
$from = "new_name_indices"
$to = "by_night"



$parent_input_dir = "$base_dir\$species\$year\$region\$from\$transect"
$input_directories = Get-Childitem -Path "$parent_input_dir"
$output_directory = "$base_dir\$species\$year\$region\$to\$transect" # output directory 
>>>>>>> 2993139175f9945ff72f21bb7f76354c5d195946
$name_filter = "*" # name filter(kinda unsure what it means)
$time_zone_offset = -0700

Set-Location -Path $base_dir


# Do not continue running the script if a problem is encountered
$ErrorActionPreference = "Stop"


# get the path for AP.exe. When do this to resolve some nice default config files.
# TODO: remove this when the default config file feature is implemented in AP.exe
$ap_path = "C:\AP"
$default_configs = Resolve-Path "$ap_path\ConfigFiles"


# check how it works

$intput_directory = Get-ChildItem -Path $parent_input_dir  |
         Sort-Object CreationTime |
         Select-Object -Skip 2 |
         Select-Object -First 1

foreach ($input_directory in $input_directories) {

    
    $current_group = $input_directory.Name
  
    $counter = 0;


    Write-Output "Concatenating files for $current_group"

    # for more information on how this command works, please see:
    # https://ap.qut.ecoacoustics.info/technical/commands/concatenate_index_files.html
    C:\AP\AnalysisPrograms.exe ConcatenateIndexFiles `
        --input-data-directory "$parent_input_dir\$current_group" `
        --output-directory "$output_directory" `
        -z $time_zone_offset `
        --file-stem-name $current_group `
        --directory-filter "*.*" `
        --index-properties-config "$default_configs/IndexPropertiesConfig.yml" `
        --false-colour-spectrogram-config "$default_configs/SpectrogramFalseColourConfig.yml" `
        --draw-images `
        --no-debug

}

Write-Output "Complete!"







