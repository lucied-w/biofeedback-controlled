addpath 'C:\Users\lucie\Desktop\resp scripts for github'

cd('D:\PhD\Biofeedback\Biofeedback resp segmented'); 

files = dir('*Resp.csv');
names_cell = {files.name};

for i = 8 %:size(files,1)

    data=readtable(files(i).name);  % read each file
    cols = {'Time__s_' 'RIP_Band_1'};
    scenario_rip = data(:,cols);

    scenario_rip = table2array(scenario_rip);
    nam = names_cell(:,i);

    breath_out = biofeedback_resp_analysis(scenario_rip, nam);
    file_out = "D:\PhD\Two Arm Biofeedback\TABio Resp Out\" + nam;

    writetable(breath_out, file_out);
    
end