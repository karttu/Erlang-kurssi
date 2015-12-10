
merge_shading_sectors_if_possible(Sectors) ->
  merge_sorted_sectors_if_possible(lists:sort(Sectors)).

merge_sorted_sectors_if_possible([]) -> [];

merge_sorted_sectors_if_possible([{Sector1Alpha,Sector1Beta},{Sector2Alpha,Sector2Beta}|Rest])
  when (Sector1Beta*Sector2Beta) > 0, %% Both Betas of the same sign.
       Sector1Alpha =< Sector2Alpha, Sector1Beta >= Sector2Beta -> %% Sector2 wholly contained in Sector1?
     merge_sorted_sectors_if_possible([{Sector1Alpha,Sector1Beta}|Rest]); %% Then use only Sector1.

merge_sorted_sectors_if_possible([{Sector1Alpha,Sector1Beta},{Sector2Alpha,Sector2Beta}|Rest])
  when (Sector1Beta*Sector2Beta) < 0, %% Betas of the different signs. (Crossing the X-axis on left side).
       Sector1Alpha =< Sector2Alpha, Sector1Beta < 0 -> %% Sector2 wholly contained in Sector1?
     merge_sorted_sectors_if_possible([{Sector1Alpha,Sector1Beta}|Rest]); %% Then use only Sector1.


merge_sorted_sectors_if_possible([{Sector1Alpha,Sector1Beta},{Sector2Alpha,Sector2Beta}|Rest])
  when (Sector1Beta*Sector2Beta) > 0, %% Both Betas of the same sign.
       (Sector1Beta*Sector2Alpha) > 0, %% Sector1Beta and Sector2Alpha of the same sign.
       Sector1Alpha =< Sector2Alpha,
       Sector2Alpha =< Sector1Beta, Sector1Beta =< Sector2Beta -> %% Partially overlapping?
     merge_sorted_sectors_if_possible([{Sector1Alpha,Sector2Beta}|Rest]); %% Then merge them.


merge_sorted_sectors_if_possible([{Sector1Alpha,Sector1Beta},{Sector2Alpha,Sector2Beta}|Rest])
  when (Sector1Beta*Sector2Beta) < 0, %% Betas of the different signs. (Crossing the X-axis on left side).
       Sector1Alpha =< Sector2Alpha, Sector2Beta < 0 -> %% Partially overlapping?
     merge_sorted_sectors_if_possible([{Sector1Alpha,Sector2Beta}|Rest]); %% Then merge them.


merge_sorted_sectors_if_possible([Firstsector|Rest]) -> [Firstsector|merge_sorted_sectors_if_possible(Rest)].


