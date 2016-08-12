*******************************************************************************
*** SETS                                                                      *
*******************************************************************************

set d        index of storage devices /d1/;
set i        index of generators /i1*i38/;
set b        index of generator blocks /b1*b3/;
set s        index of buses /s1*s2764/;
set l        index of transmission lines /l1*l3318/;
set w        index of wind generators /w1*w73/;
set r        index of solar generators /r1*r5/;
set f        index of fixed generators /f1*f440/;
set t        index of time periods /t1*t24/;

set snopud(s)        buses that belong to the snopud area /s1831*s1958/;
set from_to          lines from and to /from, to/;
set column           generator connected to bus /col/;
set wcolumn          wind connected to bus /wcol/;
set rcolumn          solar connected to bus /rcol/;
set fcolumn          fixed connected to bus /fcol/;
set iter             number of iterations /iter1*iter40/;
set day              day counter /day1*day5/;

*******************************************************************************
*** GENERATOR DATA                                                            *
*******************************************************************************

** Locations for generating units in the transmission network
table gen_map_aux(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\gmap2.inc
;

** Transformation of the previous matrix into a vector
parameter gen_map(i) generator map;
gen_map(i) = sum(column, gen_map_aux(i, column));

** Time varying generation cost curve MW block limit
table g_max_day(day, i, b) generator block output limit
$include C:\BPA_project\Test_connect_DA_new_ok\Data\block_max.inc
;

** Time varying generation capacity
table g_cap_day(day, t, i) generator capacity
$include C:\BPA_project\Test_connect_DA_new_ok\Data\gcap.inc
;

** Time varying generation cost curve price block
table k_day(day, i, b) slope of each generator cost curve block
$include C:\BPA_project\Test_connect_DA_new_ok\Data\k.inc
;

** Start-up cost of generator i
table suc_sw_aux(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\start_up_sw.inc
;

** Transformation of the previous matrix into a vector
parameter suc_sw(i) generator stepwise start-up cost;
suc_sw(i) = sum(column, suc_sw_aux(i, column));

** Time varying generation count off initial
table count_off_init_day(day, i) number of time periods each unit has been off
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux2.inc
;

** Time varying generation count on initial
table count_on_init_day(day, i) number of time periods each unit has been on
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux3.inc
;

** Fixed operating cost of each generator
table aux4(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux4.inc
;

** Transformation of the previous matrix into a vector
parameter a(i) fixed operating cost of each generator;
a(i) = sum(column, aux4(i, column));

** Generator ramp up limit
table aux5(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux5.inc
;

** Transformation of the previous matrix into a vector
parameter ramp_up(i) generator ramp-up limit;
ramp_up(i) = sum(column, aux5(i, column));

** Generator ramp down limit
table aux6(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux6.inc
;

** Transformation of the previous matrix into a vector
parameter ramp_down(i) generator ramp down limit;
ramp_down(i) = sum(column, aux6(i, column));

** Generator minimum down time
table aux7(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux7.inc
;

** Transformation of the previous matrix into a vector
parameter g_down(i) generator minimum down time;
g_down(i) = sum(column, aux7(i, column));

** Generator minimum up time
table aux8(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux8.inc
;

** Transformation of the previous matrix into a vector
parameter g_up(i) generator minimum up time;
g_up(i) = sum(column, aux8(i, column));

** Generator minimum power output
table aux9(i, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux9.inc
;

** Transformation of the previous matrix into a vector
parameter g_min(i) generator minimum power output;
g_min(i) = sum(column, aux9(i, column));

** Generator initial power output
table g_0_day(day, i) generator initial power output
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux10.inc
;

** Generator on-off status in initial period
parameter onoff_t0_day(day, i) initial unit on-off status;
onoff_t0_day(day, i)$(count_on_init_day(day, i) gt 0) = 1;

** Parameter used for minimum up-time constraints
parameter L_up_min_day(day, i) used for minimum up time constraints;
L_up_min_day(day, i) = min(card(t), (g_up(i) - count_on_init_day(day, i))
                                   * onoff_t0_day(day, i));

** Parameter used for minimum down-time constraints
parameter L_down_min_day(day, i) used for minimum down time constraints;
L_down_min_day(day, i) = min(card(t), (g_down(i) - count_off_init_day(day, i))
                                    * (1 - onoff_t0_day(day, i)));

*******************************************************************************
*** LINE DATA                                                                 *
*******************************************************************************

** Origin and destination buses for each transmission line
table line_map(l, from_to) to and from buses for each line
$include C:\BPA_project\Test_connect_DA_new_ok\Data\line_map.inc
;

** Admittance of each transmission line
table aux11(l, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux11.inc
;

** Transformation of the previous matrix into a vector
parameter admittance(l) line admittance;
admittance(l) = abs(sum(column, aux11(l, column)));

** Line capacity (long-term ratings)
table aux12(l, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\aux12.inc
;

** Transformation of the previous matrix into a vector
parameter l_max(l) line capacity;
l_max(l) = sum(column, aux12(l, column));

** Transmission lines connected to snopud buses
table snpd_lines_aux(l, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\snpd_lines.inc
;

** Transformation of the previous matrix into a vector
parameter snpd_lines_map(l) snopud transmission line map;
snpd_lines_map(l) = sum(column, snpd_lines_aux(l, column));

*******************************************************************************
*** DEMAND                                                                    *
*******************************************************************************

** Time varying demand data
table d_day(day, s, t) time varying demand data
$include C:\BPA_project\Test_connect_DA_new_ok\Data\load1.inc
;

** Auxiliary parameter to remove islanded buses
table map_islands_aux(s, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\map_islands.inc
;

** Transformation of the previous matrix into a vector
parameter map_islands(s) map of islanded buses;
map_islands(s) = sum(column, map_islands_aux(s, column));

** Ignore the demand of the islanded areas
d_day(day, s, t) = d_day(day, s, t)*map_islands(s);

*******************************************************************************
*** WIND DATA                                                                 *
*******************************************************************************

** Locations of the wind power plants
table win_map_aux(w, wcolumn)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\wmap2.inc
;

** Transformation of the previous matrix into a vector
parameter win_map(w) wind power plant locations;
win_map(w) = sum(wcolumn, win_map_aux(w, wcolumn));

** Time varying wind data
table wind_deterministic_day(day, t, w) time varying wind data
$include C:\BPA_project\Test_connect_DA_new_ok\Data\wind_deterministic.inc
;

*******************************************************************************
*** SOLAR DATA                                                                *
*******************************************************************************

** Locations of the solar plants
table sol_map_aux(r, rcolumn)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\rmap2.inc
;

** Transformation of the previous matrix into a vector
parameter sol_map(r) solar power plant locations;
sol_map(r) = sum(rcolumn, sol_map_aux(r, rcolumn));

** Time varying solar data
table sol_deterministic_day(day, t, r) time varying solar data
$include C:\BPA_project\Test_connect_DA_new_ok\Data\solar_deterministic.inc
;

*******************************************************************************
*** FIXED DATA                                                                *
*******************************************************************************

** Locations of the fixed generators
table fix_map_aux(f, fcolumn)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\fmap2.inc
;

** Transformation of the previous matrix into a vector
parameter fix_map(f) fixed generation unit locations;
fix_map(f) = sum(fcolumn, fix_map_aux(f, fcolumn));

** Time varying fixed generation data
table fix_deterministic_day(day, f, t) time varying fixed generation data
$include C:\BPA_project\Test_connect_DA_new_ok\Data\fixed_deterministic.inc
;

*******************************************************************************
*** SCALARS                                                                   *
*******************************************************************************

scalars
        penalty_pf                penalty factor /2000/
        VoRS                      value of wind spillage /20/
        VoFS                      value of fixed spillage /1000000/
        s_base                    base power /100/
        counter                   counter /2/
        M                         no. of hours a unit can be on or off /2600/
        N_iter                    number of iterations /2/
        INFEASIBLE_PENALTY        infeasibility penalty /100000000/
        ESS_ADJUST_PENALTY        ESS charge adjustment penalty /100/
;

** Index corresponding to the desired day
scalar N /
$include C:\BPA_project\Test_connect_DA_new_ok\DEPO\Day_number.csv
/;

*******************************************************************************
*** PARAMETERS                                                                *
*******************************************************************************

** Auxiliary parameters for data output
parameters
        action_aux(t, s),
        action(t, d),
        minimum_load_aux(t, s),
        maximum_load_aux(t, s),
        minimum_load(t, d),
        maximum_load(t, d)
;

** Auxiliary parameters for the optimization formulation
parameters
        g_max(i, b),
        g_cap(t, i),
        k(i, b),
        g_0(i),
        onoff_t0(i),
        L_up_min(i),
        L_down_min(i),
        demand(s, t),
        wind_deterministic(t, w),
        sol_deterministic(t, r),
        fix_deterministic(f, t)
;

** Normalization by the system base
ramp_up(i) = ramp_up(i)/s_base;
ramp_down(i) = ramp_down(i)/s_base;
g_min(i) = g_min(i)/s_base;
l_max(l) = l_max(l)/s_base;
VoRS = VoRS*s_base;

*******************************************************************************
*** STORAGE DATA                                                              *
*******************************************************************************

** Locations of the energy storage systems (ESS)
table storage_map_aux(d, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\storage_map.inc
;

** Transformation of the previous matrix into a vector
parameter storage_map(d) energy storage system locations;
storage_map(d) = sum(column, storage_map_aux(d, column));

** Area labels for energy storage systems
table area_name_storage(d, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\area_map.inc
;

** Transformation of the previous matrix into a vector
parameter storage_area(d) energy storage system area labels;
storage_area(d) = sum(column, area_name_storage(d, column));

** Zone labels for energy storage systems
table zone_name_storage(d, column)
$include C:\BPA_project\Test_connect_DA_new_ok\Data\zone_map.inc
;

** Transformation of the previous matrix into a vector
parameter storage_zone(d) energy storage system zone labels;
storage_zone(d) = sum(column, zone_name_storage(d, column));

** ES maximum power rating
parameter ES_power_max(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\DEPO\ES_power_max.csv
$offdelim
/;

** ES maximum energy rating
parameter Emax(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\DEPO\Emax.csv
$offdelim
/;

** Initial state of charge
parameter E_initial(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\DEPO\E_initial.csv
$offdelim
/;

** Final state of charge
parameter E_final(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\DEPO\E_final.csv
$offdelim
/;

** ES charging efficiency
parameter alef_ch(d) /
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\DEPO\Efficiency.csv
$offdelim
/;

** Assume charging and discharging efficiency is identical
parameter alef_dis(d);
alef_dis(d) = alef_ch(d);

** Normalize quantites by the system base
ES_power_max(d) = ES_power_max(d)/s_base;
Emax(d) = Emax(d)/s_base;
E_initial(d) = E_initial(d)/s_base;
E_final(d) = E_final(d)/s_base;

*******************************************************************************
*** TIME HORIZON LOGIC                                                        *
*******************************************************************************

** Reading input data for the selected day
loop(day$(ord(day) eq N+counter),
    g_max(i, b) = g_max_day(day, i, b)/s_base;
    k(i, b) = k_day(day, i, b)*s_base;
    g_0(i) = g_0_day(day, i)/s_base;
    demand(s, t) = d_day(day, s, t)/s_base;
    sol_deterministic(t, r) = sol_deterministic_day(day, t, r)/s_base;
    fix_deterministic(f, t) = abs(fix_deterministic_day(day, f, t))/s_base;
    wind_deterministic(t, w) = wind_deterministic_day(day, t, w)/s_base;
    onoff_t0(i) = onoff_t0_day(day, i);
    L_up_min(i) = L_up_min_day(day, i);
    L_down_min(i) = L_down_min_day(day, i);
);
