import os
import datetime
import pandas as pd
import numpy as np
import sys
import re
import random
from idmtools.entities import IAnalyzer	
from idmtools.analysis.csv_analyzer import CSVAnalyzer, IAnalyzer
from idmtools.entities.simulation import Simulation
import manifest

## For plotting
import matplotlib.pyplot as plt
import matplotlib as mpl
import matplotlib.dates as mdates


class InsetChartAnalyzer(IAnalyzer):

    @classmethod
    def monthparser(self, x):
        if x == 0:
            return 12
        else:
            return datetime.datetime.strptime(str(x), '%j').month

    def __init__(self, expt_name, sweep_variables=None, channels=None, working_dir=".", start_year=2023, end_year=2028):
        super(InsetChartAnalyzer, self).__init__(working_dir=working_dir, filenames=["output/InsetChart.json"])
        self.sweep_variables = sweep_variables or [ "itn_age_usage_U5","Run_Number", "smc_cov", "irs_cov", 
                                                    "cm_U5","rtss_1cov"]
        self.inset_channels = channels or ['Statistical Population', 'New Clinical Cases','Rainfall', 'Air Temperature',
                                           'Adult Vectors', 'Infectious Vectors']
        self.expt_name = expt_name
        self.start_year = start_year
        self.end_year = end_year

    def map(self, data, simulation: Simulation):
        simdata = pd.DataFrame({x: data[self.filenames[0]]['Channels'][x]['Data'] for x in self.inset_channels})
        simdata['Time'] = simdata.index
        simdata['Day'] = simdata['Time'] % 365
        simdata['Year'] = simdata['Time'].apply(lambda x: int(x / 365) + self.start_year)
        simdata['date'] = simdata.apply(
            lambda x: datetime.date(int(x['Year']), 1, 1) + datetime.timedelta(int(x['Day']) - 1), axis=1)

        for sweep_var in self.sweep_variables:
            if sweep_var in simulation.tags.keys():
                simdata[sweep_var] = simulation.tags[sweep_var]
            elif sweep_var == 'Run_Number' :
                simdata[sweep_var] = 0
        return simdata

    def reduce(self, all_data):

        selected = [data for sim, data in all_data.items()]
        if len(selected) == 0:
            print("No data have been returned... Exiting...")
            return

        if not os.path.exists(os.path.join(self.working_dir, self.expt_name)):
            os.mkdir(os.path.join(self.working_dir, self.expt_name))

        adf = pd.concat(selected).reset_index(drop=True)
        adf.to_csv(os.path.join(self.working_dir, self.expt_name, 'Coastalinsectchat1.csv'), index=False)

class MonthlyPfPRAnalyzer(IAnalyzer):

    def __init__(self, expt_name, sweep_variables=None, working_dir='./', start_year=2023, end_year=2028,
                 burnin=None, filter_exists=False):

        super(MonthlyPfPRAnalyzer, self).__init__(working_dir=working_dir,
                                                   filenames=["output/MalariaSummaryReport_Allges.json"]
                                                   )
     
        self.sweep_variables = sweep_variables or ["itn_age_usage_U5", "Run_Number","smc_cov", "irs_cov", 
                                                   "cm_U5", "rtss_1cov"]
        self.expt_name = expt_name
        self.start_year = start_year
        self.end_year = end_year
        self.burnin = burnin
        self.filter_exists = filter_exists

    def filter(self, simulation: Simulation):
        if self.filter_exists:
            file = os.path.join(simulation.get_path(), self.filenames[0])
            return os.path.exists(file)
        else:
            return True

    def map(self, data, simulation: Simulation):
        adf = pd.DataFrame()
        fname = self.filenames[0]
        age_bins = data[self.filenames[0]]['Metadata']['Age Bins']
      
        for age in range(len(age_bins)):
            d = data[fname]['DataByTimeAndAgeBins']['PfPR by Age Bin'][:-1]
            pfpr = [x[0] for x in d]
          
            d = data[fname]['DataByTimeAndAgeBins']['Annual Clinical Incidence by Age Bin'][:-1]
            clinical_cases = [x[0] for x in d]
         
            d = data[fname]['DataByTimeAndAgeBins']['Annual Severe Incidence by Age Bin'][:-1]
            severe_cases = [x[0] for x in d]

            d = data[fname]['DataByTimeAndAgeBins']['New Infections by Age Bin'][:-1]
            new_infections = [x[0] for x in d]

            d = data[fname]['DataByTimeAndAgeBins']['Annual Mild Anemia by Age Bin'][:-1]
            mild_anemia = [x[0] for x in d]

            d = data[fname]['DataByTimeAndAgeBins']['Pf Gametocyte Prevalence by Age Bin'][:-1]
            gamete_prev = [x[0] for x in d]
            
            d = data[fname]['DataByTimeAndAgeBins']['Mean Log Parasite Density by Age Bin'][:-1]
            dens = [x[0] for x in d]            

            d = data[fname]['DataByTimeAndAgeBins']['Average Population by Age Bin'][:-1]
            pop = [x[0] for x in d]

            simdata = pd.DataFrame({'month': range(1, len(pfpr)+1),
                                    'PfPR': pfpr,
                                    'Cases': clinical_cases,
                                    'Severe_cases': severe_cases,
                                    'New_infections': new_infections,
                                    'Anemia': mild_anemia,
                                    'Mean_density': dens,
                                    'Gametocyte_prevalence': gamete_prev,
                                    'Pop': pop})
                       
            simdata['agebin'] = age_bins[age]


            adf = pd.concat([adf, simdata])

        for sweep_var in self.sweep_variables:
            if sweep_var in simulation.tags.keys():
                try:
                    adf[sweep_var] = simulation.tags[sweep_var]
                except:
                    adf[sweep_var] = '-'.join([str(x) for x in simulation.tags[sweep_var]])
        
        adf['Year'] = None
        adf['Year'] = np.floor((adf['month']-1)/12)+self.start_year
        
        return adf

    def reduce(self, all_data):

        selected = [data for sim, data in all_data.items()]
        print(len(selected))
        if len(selected) == 0:
            print("\nWarning: No data have been returned... Exiting...")
            return

        if not os.path.exists(os.path.join(self.working_dir, self.expt_name)):
            os.mkdir(os.path.join(self.working_dir, self.expt_name))

        print(f'\nSaving outputs to: {os.path.join(self.working_dir, self.expt_name)}')

        adf = pd.concat(selected).reset_index(drop=True)
        adf.to_csv((os.path.join(self.working_dir, self.expt_name, 'Coastalmonthly3.csv')),
                   index=False)
        
if __name__ == "__main__":

    from idmtools.analysis.analyze_manager import AnalyzeManager
    from idmtools.core import ItemType
    from idmtools.core.platform_factory import Platform

    
    expts = {
        #'exp_name' : 'ID'
        'Savannah_Pickup_Refined' : '19294623-cfc7-4678-bdcb-2abd40574341'
    }

    jdir = manifest.job_directory
    wdir = os.path.join(jdir, 'my_outputs')
    
    if not os.path.exists(wdir):
        os.mkdir(wdir)
        
    #import statements, setting directories

    serialize_years = 63  # Same as in run_example_burnin.py
    step = 'pickup'
    
    ## Set sweep_variables and event_list as required depending on experiment
    channels_inset_chart = ['Statistical Population', 'New Clinical Cases',
                            'Rainfall', 'Air Temperature','Adult Vectors', 'Infectious Vectors']
    
    sweep_variables = ['Run_Number']
    if step == 'pickup':
        sweep_variables = ["itn_age_usage_U5","Run_Number", #"smc_cov", "irs_cov",
                           "cm_U5","rtss_1cov"] # for times
       #when you add additional items to the pickup, you can add more sweep variables here

    # set desired InsetChart channels to analyze and plot
    channels_inset_chart = ['Statistical Population', 'New Clinical Cases',
                            'Rainfall', 'Air Temperature','Adult Vectors','Infectious Vectors']

    with Platform('SLURM_LOCAL',job_directory=jdir) as platform:

        for expt_name, exp_id in expts.items():
            analyzers_burnin = [InsetChartAnalyzer(expt_name=expt_name,
                                                   start_year=1960,
                                                   end_year=2009,
                                                   sweep_variables=sweep_variables,
                                                   working_dir=wdir)]

            analyzers_pickup = [InsetChartAnalyzer(expt_name=expt_name,
                                                   start_year=2023,
                                                   end_year=2028,
                                                   sweep_variables=sweep_variables,
                                                   working_dir=wdir),
                                MonthlyPfPRAnalyzer(expt_name=expt_name,
                                                    start_year=2023,
                                                    end_year=2028,
                                                    sweep_variables=sweep_variables,
                                                    working_dir=wdir)]

        if step == 'burnin':
            am = AnalyzeManager(configuration={}, ids=[(exp_id, ItemType.EXPERIMENT)],
                                 analyzers=analyzers_burnin, partial_analyze_ok=True)
            am.analyze()

        elif step == 'pickup':
            am = AnalyzeManager(configuration={}, ids=[(exp_id, ItemType.EXPERIMENT)],
                                 analyzers=analyzers_pickup, partial_analyze_ok=True)
            am.analyze()
  
        else:
            print('Please define step, options are burnin or pickup') 
            
 
    # read in analyzed InsetChart data
    expt_name=list(expts.keys())[0]
    df = pd.read_csv(os.path.join(wdir, expt_name, 'Coastalinsectchat1.csv'))
    df['date'] = pd.to_datetime(df['date'])
    df = df.groupby(['date'] + sweep_variables)[channels_inset_chart].agg(np.mean).reset_index()

    # make InsetChart plot
    fig1 = plt.figure('InsetChart', figsize=(12, 6))
    fig1.subplots_adjust(hspace=0.5, left=0.08, right=0.97)
    fig1.suptitle(f'Analyzer: InsetChartAnalyzer')
    axes = [fig1.add_subplot(2, 3, x + 1) for x in range(6)]
    for ch, channel in enumerate(channels_inset_chart):
        ax = axes[ch]
        for p, pdf in df.groupby(sweep_variables):
            ax.plot(pdf['date'], pdf[channel], '-', linewidth=0.8, label=p)
        ax.set_title(channel)
        ax.set_ylabel(channel)
        ax.xaxis.set_major_locator(mdates.MonthLocator(interval=12))
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    if len(sweep_variables) > 0:
        axes[-1].legend(title=sweep_variables)
    fig1.savefig(os.path.join(wdir, expt_name, 'CoastalChartpreg.png'))
    
