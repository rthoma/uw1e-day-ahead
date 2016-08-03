***************************************************************
*** SETS
***************************************************************

set t            index of time periods /t1*t24/;
set i            index of generators /i1*i38/;
set b            index of generator blocks /b1*b3/;
set s            index of buses /s1*s2764/;
set l            index of transmission lines /l1*l3318/;
set w            index of wind generators /w1*w73/;
set r            index of solar generators /r1*r5/;
set f            index of fixed generators /f1*f440/;
set day          day counter /day1*day5/;

*set j start up cost intervals /j1*j8/;
set from_to      lines from and to /from, to/;
set column       generator connected to bus /col/;
set wcolumn      wind connected to bus /wcol/;
set rcolumn      solar connected to bus /rcol/;
set fcolumn      fixed connected to bus /fcol/;
set iter         number of iterations /iter1*iter40/;
set d            set of storage units /d1/;
set snopud(s)    set of buses that belong to snopud area /s1831*s1958/;


***************************************************************
*** GENERATOR DATA
***************************************************************

** Locations for generating units in the transmission network
table gen_map_aux(i,column) generator map
*$call =xls2gms r=Generator_Map!e2:f40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=gmap2.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\gmap2.inc
;

** Transformation of the previous matrix into a vector
parameter gen_map(i);
gen_map(i)=sum(column,gen_map_aux(i,column));

** time varying generation cost curve MW block
table g_max_day(day,i,b) generator block generation limit
*$call =xls2gms r=Generator_CostCurve!a2:f192 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=block_max.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\block_max.inc
;

** time varying generation capacity (forced outage information included)
table g_cap_day(day,t,i) generator capacity
*$call =xls2gms r=Generator_Pmax!a2:ao122 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=gcap.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\gcap.inc
;

** time varying generation cost curve price block
table k_day(day,i,b) slope of each generator cost curve block
*$call =xls2gms r=Generator_CostCurve!i2:n192 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=k.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\k.inc
;

** start-up cost of generator i
table suc_sw_aux(i,column) generator stepwise start-up cost
*$call =xls2gms r=Generator_Data!au2:av40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=start_up_sw.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\start_up_sw.inc
;

** Transformation of the previous matrix into a vector
parameter suc_sw(i);
suc_sw(i)=sum(column,suc_sw_aux(i,column));


** time varying generation count off initial
table count_off_init_day(day,i) number of time periods each generator has been off
*$call =xls2gms r=Generator_InitOff!a2:am7 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux2.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux2.inc
;

** time varying generation count off initial
table count_on_init_day(day,i) number of time periods each generator has been on
*$call =xls2gms r=Generator_InitOn!a2:am7 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux3.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux3.inc
;

** Fixed operating cost of each generator
table aux4(i,column)
*$call =xls2gms r=Generator_Data!j2:k40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux4.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux4.inc
;

** Transformation of the previous matrix into a vector
parameter a(i) fixed operating cost of each generator;
a(i)=sum(column,aux4(i,column));

** Generator ramp up limit
table aux5(i,column)
*$call =xls2gms r=Generator_Data!m2:n40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux5.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux5.inc
;

** Transformation of the previous matrix into a vector
parameter ramp_up(i) generator ramp-up limit;
ramp_up(i)=sum(column,aux5(i,column));

** Generator ramp down limit
table aux6(i,column)
*$call =xls2gms r=Generator_Data!p2:q40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux6.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux6.inc
;

** Transformation of the previous matrix into a vector
parameter ramp_down(i) generator ramp-down limit;
ramp_down(i)=sum(column,aux6(i,column));

** Minimum down time for each generator
table aux7(i,column)
*$call =xls2gms r=Generator_Data!s2:t40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux7.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux7.inc
;

** Transformation of the previous matrix into a vector
parameter g_down(i) generator minimum down time;
g_down(i)=sum(column,aux7(i,column));

** Minimum up time for each generator
table aux8(i,column)
*$call =xls2gms r=Generator_Data!v2:w40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux8.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux8.inc
;

** Transformation of the previous matrix into a vector
parameter g_up(i) generator minimum up time;
g_up(i)=sum(column,aux8(i,column));

** Minimum power output
table aux9(i,column)
*$call =xls2gms r=Generator_Data!y2:z40 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux9.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux9.inc
;

** Transformation of the previous matrix into a vector
parameter g_min(i) generator minimum output;
g_min(i)=sum(column,aux9(i,column));

** time varying generation count off initial
table g_0_day(day,i) generator generation at t=0
*$call =xls2gms r=Generator_PInit!a2:am7 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux10.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux10.inc
;

parameter onoff_t0_day(day,i) on-off status at t=0;
onoff_t0_day(day,i)$(count_on_init_day(day,i) gt 0) = 1;

parameter L_up_min_day(day,i) used for minimum up time constraints;
L_up_min_day(day,i) = min(card(t), (g_up(i)-count_on_init_day(day,i))*onoff_t0_day(day,i));

parameter L_down_min_day(day,i) used for minimum up time constraints;
L_down_min_day(day,i) = min(card(t), (g_down(i)-count_off_init_day(day,i))*(1-onoff_t0_day(day,i)));

scalar M number of hours a unit can be on or off /2600/;

***************************************************************
*** LINE DATA
***************************************************************

** Origin and destination buses for each transmission line
table line_map(l,from_to) line map
*$call =xls2gms r=Line_Map!e1:g3319 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=line_map.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\line_map.inc
;

** Admittance of each transmission line
table aux11(l,column)
*$call =xls2gms r=Line_Data!a1:b3319 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux11.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux11.inc
;

** Transformation of the previous matrix into a vector
parameter admittance(l) line admittance;
admittance(l)=abs(sum(column,aux11(l,column)));

** Line capacities
table aux12(l,column)
*$call =xls2gms r=Line_Data!j1:k3319 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=aux12.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux12.inc
;

** Transformation of the previous matrix into a vector
parameter l_max(l) line capacities (long-term ratings);
l_max(l)=sum(column,aux12(l,column));

** Transmission lines connected to snopud buses
table snpd_lines_aux(l,column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\snpd_lines.inc
;

** Transformation of the previous matrix into a vector
parameter snpd_lines_map(l) line capacities (long-term ratings);
snpd_lines_map(l)=sum(column,snpd_lines_aux(l,column));


***************************************************************
*** DEMAND DATA
***************************************************************

** time varying demand
table d_day(day,s,t) demand at bus s
*$call =xls2gms r=Load1!a1:aa13821 i=BPA_fixed_load_Apr13.xlsx o=load1.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\load1.inc
;

** We have created this parameter to remove the load from islanded buses
table map_islands_aux(s,column) one-bus islands
$include C:\BPA_project\Test_connect_DA_new_ok\Data\map_islands.inc
;

** Transformation of the previous matrix into a vector
Parameter map_islands(s);
map_islands(s)=sum(column,map_islands_aux(s,column));

** I am ignoring the demand of the islanded areas
d_day(day,s,t)=d_day(day,s,t)*map_islands(s);

***************************************************************
*** WIND DATA
***************************************************************

** Locations of the wind power plants
table win_map_aux(w,wcolumn) wind map
*$call =xls2gms r=Wind!e1:f74 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=wmap2.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\wmap2.inc
;

** Transformation of the previous matrix into a vector
parameter win_map(w);
win_map(w)=sum(wcolumn,win_map_aux(w,wcolumn));

** time varying wind
table wind_deterministic_day(day,t,w) wind data
*$call =xls2gms r=Wind!h1:ce121 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=wind_deterministic.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\wind_deterministic.inc
;

***************************************************************
*** Solar DATA
***************************************************************

** Locations of the solar plants
table sol_map_aux(r,rcolumn) solar map
*$call =xls2gms r=Solar!e1:f6 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=rmap2.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\rmap2.inc
;

** Transformation of the previous matrix into a vector
parameter sol_map(r);
sol_map(r)=sum(rcolumn,sol_map_aux(r,rcolumn));

** time varying solar
table sol_deterministic_day(day,t,r) solar data
*$call =xls2gms r=Solar!h1:o121 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=solar_deterministic.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\solar_deterministic.inc
;

********************************************************************************
*** Fixed DATA
********************************************************************************

** Locations of the fixed generators
table fix_map_aux(f,fcolumn) fixed map
*$call =xls2gms r=Fixed!e1:f441 i=Input_Data_WECC2024_BPA_Apr13.xlsx o=fmap2.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\fmap2.inc
;

** Transformation of the previous matrix into a vector
parameter fix_map(f);
fix_map(f)=sum(fcolumn,fix_map_aux(f,fcolumn));

** time varying fixed dispatch
table fix_deterministic_day(day,f,t) fixed data
*$call =xls2gms r=Fixed!a1:aa2201 i=BPA_fixed_load_Apr13.xlsx o=fixed_deterministic.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\fixed_deterministic.inc
*;

********************************************************************************
********************************************************************************

scalar penalty_pf /2000/;


scalars
        VoRS                       value of wind spillage /20/
        s_base                     base power /100/
        counter                    counter /2/
        N_iter                     number of iterations /2/
        FEASIBILITY_PENALTY        feasibility penalty /100000000/
;

** Day number from 0 to N
scalar N /
$include C:\BPA_project\Test_connect_DA_new_ok\Data\Day_number.csv
/;

** DEfinition of parameter used in the optimization problems
parameter g_max(i,b),g_cap(t,i),k(i,b),g_0(i),onoff_t0(i),L_up_min(i),L_down_min(i),demand(s,t),wind_deterministic(t,w),sol_deterministic(t,r),fix_deterministic(f,t);

ramp_up(i)=ramp_up(i)/s_base;
ramp_down(i)=ramp_down(i)/s_base;
g_min(i)=g_min(i)/s_base;
l_max(l)=l_max(l)/s_base;
VoRS = VoRS * s_base;



** Reading the data that depend on the index "day"
loop(day$(ord(day) eq N+counter),

g_max(i,b)= g_max_day(day,i,b)/s_base;
k(i,b)=k_day(day,i,b)*s_base;
g_0(i)=g_0_day(day,i)/s_base;
demand(s,t)=d_day(day,s,t)/s_base;
sol_deterministic(t,r)=sol_deterministic_day(day,t,r)/s_base;
fix_deterministic(f,t)=abs(fix_deterministic_day(day,f,t))/s_base;
wind_deterministic(t,w)=wind_deterministic_day(day,t,w)/s_base;
onoff_t0(i)=onoff_t0_day(day,i);
L_up_min(i)=L_up_min_day(day,i);
L_down_min(i)=L_down_min_day(day,i);

);

********************************************************************************
*** STORAGE DATA (NEGOTIATION PROTOCOL AND INFORMATION ABOUT UNITS)
********************************************************************************

** Locations of the energy storage devices
table storage_map_aux(d,column) map energy storage - bus
*$call =xls2gms r=map_storage!a2:b22 i=ES_data.xlsx o=storage_map.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\storage_map.inc
;

** Area where the energy storage devices are located
table area_name_storage(d,column)
*$call =xls2gms r=map_storage!d2:e22 i=ES_data.xlsx o=area_map.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\area_map.inc
;

** Zone where energy storage devices are located
table zone_name_storage(d,column)
*$call =xls2gms r=map_storage!g2:h22 i=ES_data.xlsx o=zone_map.inc
$include C:\BPA_project\Test_connect_DA_new_ok\Data\zone_map.inc
;

** Transformation of the previous matrices into a vectors
Parameter storage_map(d),storage_area(d), storage_zone(d);

storage_map(d)=sum(column,storage_map_aux(d,column));
storage_area(d)=sum(column,area_name_storage(d,column));
storage_zone(d)=sum(column,zone_name_storage(d,column));

** ES maximum power rating
Parameter ES_power_max(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\ES_power_max.csv
$offdelim
/;

** ES maximum energy rating
Parameter Emax(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\Emax.csv
$offdelim
/;

** Initial energy state-of-charge
Parameter E_initial(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\E_initial.csv
$offdelim
/;

** Final energy state-of-charge
Parameter E_final(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\E_final.csv
$offdelim
/;

** Charging efficiency of the energy storage devices
Parameter alef_ch(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\Efficiency.csv
$offdelim
/;

** We assume that the charging and discharging efficiencies are identical
Parameters alef_dis(d);
alef_dis(d)=alef_ch(d);

ES_power_max(d)= ES_power_max(d)/s_base;
Emax(d)= Emax(d)/s_base;
E_initial(d)=E_initial(d)/s_base;
E_final(d)=E_final(d)/s_base;

** Definition of auxiliary parameters used to output some results
Parameter  action_aux(t,s),action(t,d),minimum_load_aux(t,s), maximum_load_aux(t,s) , minimum_load(t,d), maximum_load(t,d);
