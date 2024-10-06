setwd('C:/Users/Brain Computers/Desktop/R-files')
# Install and load the rdhs package
library(rdhs)

#Download a specific dataset (e.g., "GHIR71FL") in RDS format
get_datasets(dataset_filenames = "NGBR61SV", download_option = "rds")

#Install and load the rdhs package
install.packages("rdhs")
library(rdhs)
# Set the rdhs configuration
#set_rdhs_config(cache_dir = "C:/Users/Brain Computers/Desktop/ICAMMDA Data/DHS_Aaron")
# Authenticate with your email address
#authenticate_dhs(config = list(your_email = "aaron.nwana.msc180003@fuoye.edu.ng"))

authenticate_dhs(config = list(your_email = "aaron.nwana.msc180003@fuoye.edu.ng",
    cache_dir = "C:/Users/Brain Computers/Desktop/ICAMMDA Data/DHS_Aaron"))


# List of dataset filenames (replace with your desired datasets)
dataset_filenames <- c("NGBR61SV", "NGHR61SV", "NGHR71SV", "NGHR81SV","NGIR61SV", "NGIR71SV",
                       "NGIR81SV", "NGKR61SV", "NGKR71SV", "NGKR81SV","NGPR61SV","NGPR71SV", "NGPR81SV")

# Download datasets in RDS format
for (filename in dataset_filenames) {
  get_datasets(dataset_filenames = filename, download_option = "rds")
}


#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGBR61SV.zip&Tp=1&Ctry_Code=NG&surv_id=392&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGHR61SV.zip&Tp=1&Ctry_Code=NG&surv_id=392&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGHR71SV.zip&Tp=1&Ctry_Code=NG&surv_id=474&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGHR81SV.zip&Tp=1&Ctry_Code=NG&surv_id=576&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGIR61SV.zip&Tp=1&Ctry_Code=NG&surv_id=392&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGIR71SV.zip&Tp=1&Ctry_Code=NG&surv_id=474&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGIR81SV.zip&Tp=1&Ctry_Code=NG&surv_id=576&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGKR61SV.zip&Tp=1&Ctry_Code=NG&surv_id=392&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGKR71SV.zip&Tp=1&Ctry_Code=NG&surv_id=474&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGKR81SV.zip&Tp=1&Ctry_Code=NG&surv_id=576&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR61SV.zip&Tp=1&Ctry_Code=NG&surv_id=392&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR71SV.zip&Tp=1&Ctry_Code=NG&surv_id=474&dm=1&dmode=nm
#https://www.dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR81SV.zip&Tp=1&Ctry_Code=NG&surv_id=576&dm=1&dmode=nm