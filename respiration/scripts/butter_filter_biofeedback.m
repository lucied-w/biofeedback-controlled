function [scenario_rip] = butter_filter_RAVE(scenario_rip, plotting)


%%
%DATA FILTERING
% Lowopass - Butterworth filter
%removes baseline wander from the signal
n = 10;
F = 0.1;
[y, x] = butter(n, F, 'low');
scenario_rip(:,2) = filter(y, x, scenario_rip(:,2));

%dungeon_rip = dungeon_rip(100:end, :);

%boat_rip = boat_rip(100:end, :);

if plotting == "y"

plot(scenario_rip(:,1), scenario_rip(:,2))
hold on; plot(scenario_rip(:,1), scenario_rip(:,2), 'r')



end

end