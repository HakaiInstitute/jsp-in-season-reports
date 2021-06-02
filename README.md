# jsp-in-season-reports
In season reports of juvenile salmon migration observations.

See the 'Hakai_Juvenile_Salmon_Report.Rmd' file in the current year's folder for the code that produced this years reports.

Annual steps to update the in season report script:
* Create new folder for current year and create figs and data folders
* Add current year map to the figs folder and find and replace in the script last years map filename with current year file name. ie find map_2021.png and replace with map_2022.png
* Copy figure_opts.tex file and hakai.png file from previous year folder into root of current year folder (ie 2021_in_season_report to 2022_in_season_report)
* In both the ctd_data_import_and_wranlge.R script and the Hakai_Juvenile_Salmon_Report_2021.Rmd find and replace all previous year text with current year text. Ie find 2021 and replace with 2022.
* Replace time series range (eg. 2015-2021) with the new time series range (2015-2021)
* Update the qu39_temp_anomaly data frame produced by the ctd_data_import script by manually adding rows as detailed in the script comments

