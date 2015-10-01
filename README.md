# Septic-GHGs
R code to process raw data downloaded from the GC.

In order for everything to work together, there are several things to know.
First, the order that you should use the programs in:

1. GC_data_transformation     (This takes raw GC data and organizes it in the correct file structure for subsequent scripts)
2. Septic_R_code_2015         (This processes the raw GC data and outputs processed data)
3. Flux_grab_amt              (This code assembles all of the flux files outputted by the previous R code and collates it)
4. Septic_stats_2015_v3       (Data analysis)

The functions at the bottom of the page are necessary for "Septic_R_code_2015.R" Chamber heights are necessary as well.

Second: It is VERY IMPORTANT that the GC_data_transformation program is accompanied by a properly formatted Batch_descriptions.csv file. This dataframe tells the script how the data is organized and where it is. It should have 7 columns: Batch, Collection_date, Range_begin, Range_end, House_owner, Order, and Notes. Each row in this file should indicate a sample set (or fraction of a sample set); batch is the GC raw data filename that contains the data, collection_date is self-explanatory, Range_begin indicates the beginning of the sample set in the raw data file, range_end indicates the end, house_owner is self-explanatory (no caps), order indicates which fragment of a fragmented sample set comes first. If a sample set is not fragmented, just put "1" down for order. Notes is for anything you might want to keep track of (ie. "The LN2 ran out during this run", etc.) and is not necessary for the program to run. Remember, the file must be saved as a .csv file.

Third: GC data often isn't perfect, and may contain gaps. If there is a gap, fill it in with an NA. NEVER SKIP SAMPLES otherwise everything will be offset or you will get an error. For example, if M1.1 is supposed to be followed by M1.2 but some greenhorn undergrad accidentally smashed the vial, the GC raw data file will have M1.1 followed by M1.3. You will have to manually go into the file and add M1.2 in the first column, with NAs all across the row.

Fourth: If you can't get things to work right, let Keiran or Allison know. 
