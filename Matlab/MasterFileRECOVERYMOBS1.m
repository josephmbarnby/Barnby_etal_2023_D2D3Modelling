%% Master File %%

%% Load Data
addpath('CBMCode', 'Data/SimulatedData')

%Global values
trials = 18;
Sess = ["LDOPA", "HALO", "PLAC"];

%Create data container
DFS = cell(length(Sess), 1);

for j = 1:length(Sess)

        %Load Dat
        Load  = readtable(strcat(Sess(j),'_sim.csv'));
        Loadi = table2array(Load(:,contains(Load.Properties.VariableNames,...
                {'decision', 'HI', 'SI', 'ID', 'iter'})));

        n = size(Loadi)/trials;
        DFS{j} = cell(n(1), 1);
        for i = 1:n(1)
            DFS{j}{i} = Loadi((i*trials)-(trials-1):i*trials,:);
            DFS{j}{i}(:,2:3) = DFS{j}{i}(:,2:3)/100;
        end
end



%% Check models

BayesBelief_NewEta_MOBS1(randn(1,9), DFS{1}{1}) % 9 parms
BayesBelief_OneUnc_MOBS1(randn(1,8), DFS{1}{1}) % 9 parms

%% Set WDs
Models = ["BBNew1Eta", "BBOneLik", "BBOneUnc"];
Folder = 'Lap_Subj_Sim';

%BB

for j = 1:length(Sess)
        for q = 1:length(Models)

        S = Sess(j); M = Models(q);
        mkdir(fullfile(Folder, strcat('lap_Subj_Sim',M,'_',S)));

        end
end

%% Set Priors

v = 6.5;

ModelsFunc = {
@BayesBelief_NewEta_MOBS1,...
@BayesBelief_OneLik_MOBS1,...
@BayesBelief_OneUnc_MOBS1
};

prior = {...
struct('mean', zeros(9,1), 'variance', v),...
struct('mean', zeros(8,1), 'variance', v),...
struct('mean', zeros(8,1), 'variance', v)
};

%% Run Lap
%% BB Models (Extended)

mkdir('LaplaceFit_Sim')
MetaFol    = 'Lap_Subj_Sim';

for j = 1:length(Sess) % run a loop for each subsample
        for q = 1:length(Models)

        DatUsei = DFS{j};
        S = Sess(j);
        M = Models(q);
        Folder = strcat('lap_Subj_Sim',M,'_',S);
        Subj = strcat('lap_Subj_Sim',M,'_',S, '_');
        file = strcat('LaplaceFit_Sim/lap_Sim_',M,'_',S,'.mat');

        parfor i = 1:length(DatUsei) %nested parfor loop for fitting
        % 1st input: data
        data_subj = DatUsei(i);
        % 2nd input: function handle of model (i.e. @model_mf)
        % 3rd input: a prior struct.
        % 4th input: output file
        fname_subj = fullfile(MetaFol, Folder,strcat(Subj, num2str(i), '.mat'));
        cbm_lap(data_subj, ModelsFunc{q}, prior{q}, fname_subj);
        end

        CALC = cell(length(DatUsei),1);
        for n=1:length(CALC)
            CALC{n} = fullfile(MetaFol, Folder,strcat(Subj, num2str(n), '.mat'));
        end

        CALCBIND = CALC;
        fname_BF = file;
        cbm_lap_aggregate(CALCBIND,fname_BF);

        end
end

%% Hierarchical fits

%% HBM CBM for Bayes models

mkdir('HBIFit_Sim')
addpath('LaplaceFit_Sim', 'HBIFIT_Sim')

ModelsFuncHBI = {
@BayesBelief_NewEta_MOBS1,...
@BayesBelief_OneLik_MOBS1,...
@BayesBelief_OneUnc_MOBS1
};

ModelsNameHBI = ["BBNew1Eta", "BBOneLik", "BBOneUnc"];

for j = 1:length(Sess) % run a loop for each subsample

       S = Sess(j);
       fcbm_maps =  {
           char('lap_Sim_' + ModelsNameHBI(1) + '_' + S + '.mat'), ...
           char('lap_Sim_' + ModelsNameHBI(2) + '_' + S + '.mat'), ...
           char('lap_Sim_' + ModelsNameHBI(3) + '_' + S + '.mat')
       };
       BFS_hbi = char('HBIFit_Sim/hbi_BB_Sim'+ S + '.mat');
       try
            cbm_hbi(DFS{j},ModelsFuncHBI,fcbm_maps,BFS_hbi);
       catch
       end

end

%% HBM CBM for Bayes models
for k = 1:3 % run a loop for each subsample

   models = {@BayesBelief_NewEta_MOBS1, @BayesBelief_OneLik_MOBS1, @BayesBelief_OneUnc_MOBS1};

   if k == 1
       fcbm_maps = {'lap_Sim_BBNew1EtaPLAC.mat', 'lap_Sim_BBOneLik_PLAC.mat', 'lap_Sim_BBOneUnc_PLAC.mat'};
       BFS_hbi = 'hbi_BB_SimPLAC.mat';
       cbm_hbi(DatUsePLAC,models,fcbm_maps,BFS_hbi);
   elseif k == 2
       fcbm_maps = {'lap_Sim_BBNew1EtaHALO.mat', 'lap_Sim_BBOneLik_HALO.mat', 'lap_Sim_BBOneUnc_HALO.mat'};
       BFS_hbi = 'hbi_BB_SimHALO.mat';
       cbm_hbi(DatUseHALO,models,fcbm_maps,BFS_hbi);
   elseif k == 3
       fcbm_maps = {'lap_Sim_BBNew1EtaLDOPA.mat', 'lap_Sim_BBOneLik_LDOPA.mat', 'lap_Sim_BBOneUnc_LDOPA.mat'};
       BFS_hbi = 'hbi_BB_SimLDOPA.mat';
       cbm_hbi(DatUseLDOPA,models,fcbm_maps,BFS_hbi);
   else
   warning('error1')
   end

end
