
function [breath_out] = basic_resp_analysis(scenario_rip, participant_name)

    [scenario_rip] = butter_filter_biofeedback(scenario_rip, "y");


    sz = [1 4];
    varNames = ["pID", "BreathsPerMin", "Peaks", "Time"];
    varTypes = ["string", "double", "double", "double"];
    breath_out = table('Size', sz, 'VariableTypes', varTypes, 'VariableNames', varNames) ;

    clear varNames varTypes sz


    %%%% boat firstS
    
    half = (min(scenario_rip(:,2))) + (0.5*(max(scenario_rip(:,2) - min(scenario_rip(:,2)))));
    
    y = scenario_rip(:,2);
    y = (y - half);
    x = scenario_rip(:,1);
    
    minpeakheight = -0.01; % adjustable depending on dataset (fairly small value)

    minpeakdistance = 4; % adjustable depending on dataset
    
    %peak values

    [peak_vals,peak_locs] = findpeaks(scenario_rip(:,2), scenario_rip(:,1), 'MinPeakDistance',minpeakdistance);    
    
    %min values
    
    [peak_vals_min,peak_locs_min] = findpeaks(scenario_rip(:,2)*(-1),scenario_rip(:,1),'MinPeakDistance',minpeakdistance);
     
    %%% plotting this out
    subplot(2,1,1)
    plot(scenario_rip(:,1),scenario_rip(:,2), 'LineWidth', 2)
    hold on
    plot(peak_locs, peak_vals, 'o', 'MarkerFaceColor','g')
    
    %they need to be shifted to their original values here
    %plot(peak_locs_min, peak_vals_min*(-1), 'o', 'MarkerFaceColor','r')
    %making plot pretty
    %hold on
    xlim([scenario_rip(1,1), scenario_rip(end,1)+0.2]);
    ylim([(min(scenario_rip(:,2)-0.01)) (max(scenario_rip(:,2)+ 0.01))])
    xlabel("Minutes")
    ylabel(" Respiration Amplitude")
    title(strcat(participant_name, " ", "Scen"))
        
    missed_max = input("Boat: how many peaks are missing?");
    false_max = input("Boat: how many false peaks are there?");

    close

    total_peaks = (numel(peak_locs) + missed_max) - false_max;

    scen_time = ((scenario_rip((size(scenario_rip, 1)), 1)) - (scenario_rip(1,1)))/60;
    scen_per_min = min(total_peaks)/scen_time;
    scen_peaks = total_peaks;

    breath_out{1,"pID"} = {participant_name};
    breath_out{1,"BreathsPerMin"} = scen_per_min;
    breath_out{1,"Peaks"} = scen_peaks;
    breath_out{1, "Time"} = scen_time;


    clear missed_max missed_min false_min false_max x y half peak_vals peak_vals_min peak_locs peak_locs_min
    clear boat_rip boat_peaks boat_time boat_troughs boat_resp_per_min breaths total_troughs total_peaks
    
    
end

