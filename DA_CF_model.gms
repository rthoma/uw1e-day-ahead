** a_DA_CF_model.gms

*******************************************************************************
*** OPTIONS                                                                   *
*******************************************************************************

$onempty
$offlisting
$offupper
$offsymlist
$offsymxref
$offuellist
$offuelxref

options
        limrow = 0,
        limcol = 0,
        solprint = off,
        sysout = off
;

*******************************************************************************
*** INPUT DATA                                                                *
*******************************************************************************

$include C:\BPA_project\Test_connect_DA_new_ok\input_data.gms

table g_bis2(i, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\gbis.csv
$offdelim
;

table glin_bis2A(i, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\glin_bisA.csv
$offdelim
;

table glin_bis2B(i, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\glin_bisB.csv
$offdelim
;

table glin_bis2C(i, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\glin_bisC.csv
$offdelim
;

table slack_wind_bis2(w, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\slackwindbis.csv
$offdelim
;

table slack_solar_bis2(r, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\slacksolarbis.csv
$offdelim
;

table slack_fixed_bis2(f, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\slackfixedbis.csv
$offdelim
;

table powerflowUC2(l, t)
$ondelim
$include C:\BPA_project\Test_connect_DA_new_ok\Data\powerflow.csv
$offdelim
;

parameters
        glin_bis(t, i, b)            gen block outputs from previous stage
        slack_solar_bis(r, t)        solar spillage in the previous stage
        slack_wind_bis(w, t)         wind spillage in the previous stage
        slack_fixed_bis(f, t)        fixed spillage in the previous stage
        gbis(t, i)                   gen power output in previous stage
        M_cong_aux(t, l)             congestion indicator by line
        M_cong(t)                    system wide congestion indicator
;

gbis(t, i) = g_bis2(i, t);
glin_bis(t, i, 'b1') = glin_bis2A(i, t);
glin_bis(t, i, 'b2') = glin_bis2B(i, t);
glin_bis(t, i, 'b3') = glin_bis2C(i, t);
slack_wind_bis(w, t) = slack_wind_bis2(w, t);
slack_solar_bis(r, t) = slack_solar_bis2(r, t);
slack_fixed_bis(f, t) = slack_fixed_bis2(f, t);
M_cong_aux(t, l)$(abs(powerflowUC2(l, t)) - l_max(l) ge 0) = 1;
M_cong(t)$(sum(l, M_cong_aux(t, l)) gt 0) = 1;

*******************************************************************************
*** VARIABLES                                                                 *
*******************************************************************************

variables
        obj                objective function
        pf(t, l)           line flows
        theta(t, s)        voltage angles
;

positive variables
        ch_total(t, d)                   total ESS charging amount
        dis_total(t, d)                  total ESS discharging amount
        deltag_plus(t, i)                positive dev. for g
        deltag_minus(t, i)               negative dev. for g
        deltag_lin_plus(t, i, b)         positive dev. for g_lin
        deltag_lin_minus(t, i, b)        negative dev. for g_lin
        slack_wind_plus(t, w)            positive dev. for wind power spillage
        slack_wind_minus(t, w)           negative dev. for wind power spillage
        slack_solar_plus(t, r)           positive dev. for solar power spillage
        slack_solar_minus(t, r)          negative dev. for solar power spillage
        slack_fixed_plus(t, f)           positive dev. for fixed power spillage
        slack_fixed_minus(t, f)          negative dev. for fixed power spillage
        soc(t, d)                        energy state of charge
;

binary variables
        v(t, i)           commitment variables
        y(t, i)           start up variables
        z(t, i)           shut down variables
        v_ch(t, d)        binary variable preventing simultaneous ESS action
;

equations
        cost                             objective function
        bin_set1(t, i)                   binary logic constraint 1
        bin_set10(t, i)                  binary logic constraint 1_2
        bin_set2(t, i)                   binary logic constraint 2
        min_updown_1(t, i)               initial statuses
        min_updown_2(t, i)               minimum up time constraint
        min_updown_3(t, i)               minimum down time constraint
        slack_wind_constr(t, w)          maximum wind spillage constraint
        slack_solar_constr(t, r)         maximum solar spillage constraint
        slack_fixed_constr(t, f)         maximum fixed spillage constraint
        gen_sum(t, i)                    summation over all blocks
        gen_min(t, i)                    minimum power output of generators
        block_output(t, i, b)            maximum power output of each block
        ramp_limit_min(t, i)             ramp dn constraint
        ramp_limit_max(t, i)             ramp up constraint
        ramp_limit_min_1(t, i)           ramp dn constraint for initial period
        ramp_limit_max_1(t, i)           ramp up constraint for initial period
        line_flow(t, l)                  power flow
        power_balance(t, s)              power balance equation
        voltage_angles_min(t, s)         minimum voltage phase angle limits
        voltage_angles_max(t, s)         maximum voltage phase angle limits
        line_capacity_min(t, l)          maximum line flow limits
        line_capacity_max(t, l)          minimum line flow limits
        slack_wind_constr2(t, w)         minimum wind spillage constraint
        slack_solar_constr2(t, r)        minimum solar spillage constraint
        slack_fixed_constr2(t, f)        minimum fixed spillage constraint
        eq_storage_init(t, d)            initial ESS stage of charge
        eq_storage(t, d)                 ESS state of charge calculation
        soc_limit(t, d)                  maximum ESS state of charge
        eq_soc_final(t, d)               final ESS state of charge
        ch_total_limit(t, d)             maximum ESS charging
        dis_total_limit(t, d)            maximum ESS discharging
;

alias (t, tt);

*******************************************************************************
*** CONSTRAINTS                                                               *
*******************************************************************************

** Objective function
cost..
    obj =e= sum((t, i), suc_sw(i)*y(t, i) + a(i)*v(t, i)
              + sum(b, (deltag_lin_plus(t, i, b)
                      + deltag_lin_minus(t, i, b))*k(i, b)))
          + sum((t, r), slack_solar_plus(t, r)
                      + slack_solar_minus(t, r))*penalty_pf
          + sum((t, w), slack_wind_plus(t, w)
                      + slack_wind_minus(t, w))*penalty_pf
          + sum((f, t), slack_fixed_plus(t, f)
                      + slack_fixed_minus(t, f))*penalty_pf
          + sum((t, d), ch_total(t, d)
                      + dis_total(t, d))*ESS_ADJUST_PENALTY
;

** Binary logic for the first period of the optimization horizon
bin_set10(t, i)$(ord(t) eq 1)..
        y(t, i) - z(t, i) =e= v(t, i) - onoff_t0(i);

** Binary logic for all periods except the first
bin_set1(t, i)$(ord(t) gt 1)..
        y(t, i) - z(t, i) =e= v(t, i) - v(t-1, i);

** Prevent simultaneous start-up and shutdown
bin_set2(t, i)..
        y(t, i) + z(t, i) =l= 1;

** Initial conditions for the minimum up and down time constraints
min_updown_1(t, i)$((L_up_min(i) + L_down_min(i) gt 0)
        and (ord(t) le L_up_min(i) + L_down_min(i)))..
                v(t, i) =e= onoff_t0(i);

** Minimum up time constraints for the rest of the periods
min_updown_2(t, i)$(ord(t) gt L_up_min(i))..
        sum(tt$((ord(tt) ge ord(t) - g_up(i) + 1)
                and (ord(tt) le ord(t))), y(tt, i)) =l= v(t, i);

** Minimum down time constraints for the rest of the periods
min_updown_3(t, i)$(ord(t) gt L_down_min(i))..
        sum(tt$((ord(tt) ge ord(t) - g_down(i) + 1)
                and (ord(tt) le ord(t))), z(tt, i)) =l= 1 - v(t, i);

** Total power output as the sum of the blocks
gen_sum(t, i)..
        gbis(t, i)
      + deltag_plus(t, i)
      - deltag_minus(t, i) =e= sum(b, glin_bis(t, i, b)
                                    + deltag_lin_plus(t, i, b)
                                    - deltag_lin_minus(t, i, b));

** Minimum bound for the power output of conventional thermal units
gen_min(t, i)..
        gbis(t, i)
      + deltag_plus(t, i)
      - deltag_minus(t, i) =g= g_min(i)*v(t, i);

** Maximum bounds for the power output of each block
block_output(t, i, b)..
        glin_bis(t, i, b)
      + deltag_lin_plus(t, i, b)
      - deltag_lin_minus(t, i, b) =l= g_max(i, b)*v(t, i);

** Ramp down constraints for the initial period
ramp_limit_min_1(t, i)$(ord(t) eq 1)..
        -ramp_down(i) =l= (gbis(t, i)
                         + deltag_plus(t, i)
                         - deltag_minus(t, i)) - g_0(i);

** Ramp down constraints for all periods except the first
ramp_limit_min(t, i)$(ord(t) gt 1)..
        -ramp_down(i) =l= (gbis(t, i)
                         + deltag_plus(t, i)
                         - deltag_minus(t, i)) - (gbis(t-1, i)
                                                + deltag_plus(t-1, i)
                                                - deltag_minus(t-1, i));

** Ramp up constraints for the initial period
ramp_limit_max_1(t, i)$(ord(t) eq 1)..
        ramp_up(i) =g= (gbis(t, i)
                      + deltag_plus(t, i)
                      - deltag_minus(t, i)) - g_0(i);

** Ramp up constraints for all periods except the first
ramp_limit_max(t, i)$(ord(t) gt 1)..
        ramp_up(i) =g= (gbis(t, i)
                      + deltag_plus(t, i)
                      - deltag_minus(t, i)) - (gbis(t-1, i)
                                             + deltag_plus(t-1, i)
                                             - deltag_minus(t-1, i));

** Nodal power balance equations
power_balance(t, s)..
    demand(s, t)
        + sum(d$(storage_map(d) eq ord(s)),
                 ch_total(t, d) - dis_total(t, d)) =e= 
          sum(i$(gen_map(i) eq ord(s)), gbis(t, i)
                                      + deltag_plus(t, i)
                                      - deltag_minus(t, i))
        + sum(f$(fix_map(f) eq ord(s)), fix_deterministic(f, t)
                                      - slack_fixed_bis(f, t)
                                      - slack_fixed_plus(t, f)
                                      + slack_fixed_minus(t, f))
        + sum(r$(sol_map(r) eq ord(s)), sol_deterministic(t, r)
                                      - slack_solar_bis(r, t)
                                      - slack_solar_plus(t, r)
                                      + slack_solar_minus(t, r))
        + sum(w$(win_map(w) eq ord(s)), wind_deterministic(t, w)
                                      - slack_wind_bis(w, t)
                                      - slack_wind_plus(t, w)
                                      + slack_wind_minus(t, w))
        - sum(l$(line_map(l, 'from') eq ord(s)), pf(t, l))
        + sum(l$(line_map(l, 'to') eq ord(s)), pf(t, l))
;

** Definition of the line flows in terms of the voltage phase angles
line_flow(t, l)..
        pf(t, l) =e= (sum(s$(line_map(l, 'from') eq ord(s)), theta(t, s))
                    - sum(s$(line_map(l, 'to') eq ord(s)), theta(t, s)))
                    * admittance(l);

** Minimum voltage phase angle limits
voltage_angles_min(t, s)..
        theta(t, s) =g= -pi;

** Maximum voltage phase angle limits
voltage_angles_max(t, s)..
        theta(t, s) =l= pi;

** Transmission capacity constraint
line_capacity_min(t, l)..
        pf(t, l) =g= -l_max(l);

** Transmission capacity constraint
line_capacity_max(t, l)..
        pf(t, l) =l= l_max(l);

** Maximum spillage for solar generation
slack_solar_constr(t, r)..
        sol_deterministic(t, r) =g= slack_solar_bis(r, t)
                                  + slack_solar_plus(t, r)
                                  - slack_solar_minus(t, r);

** Minimum spillage for solar generation
slack_solar_constr2(t, r)..
        0 =l= slack_solar_bis(r, t)
            + slack_solar_plus(t, r)
            - slack_solar_minus(t, r);

** Maximum spillage for wind generation
slack_wind_constr(t, w)..
        wind_deterministic(t, w) =g= slack_wind_bis(w, t)
                                   + slack_wind_plus(t, w)
                                   - slack_wind_minus(t, w);

** Minimum spillage for wind generation
slack_wind_constr2(t, w)..
        0 =l= slack_wind_bis(w, t)
            + slack_wind_plus(t, w)
            - slack_wind_minus(t, w);

** Maximum spillage for fixed resources
slack_fixed_constr(t, f)..
        fix_deterministic(f, t) =g= slack_fixed_bis(f, t)
                                  + slack_fixed_plus(t, f)
                                  - slack_fixed_minus(t, f);

** Minimum spillage for fixed resources
slack_fixed_constr2(t, f)..
        0 =l= slack_fixed_bis(f, t)
            + slack_fixed_plus(t, f)
            - slack_fixed_minus(t, f);

** Energy storage charging limit
ch_total_limit(t, d)..
        ch_total(t, d) =l= ES_power_max(d)*v_ch(t, d);

** Energy storage discharging limit
dis_total_limit(t, d)..
        dis_total(t, d) =l= ES_power_max(d)*(1 - v_ch(t, d));

** Initial energy storage state of charge trajectory
eq_storage_init(t, d)$(ord(t) eq 1)..
        soc(t, d) =e= E_initial(d) + ch_total(t, d)*alef_ch(d)
                                   - dis_total(t, d)/alef_dis(d);

** Energy storage state of charge trajectory in periods greater than 1
eq_storage(t, d)$(ord(t) gt 1)..
        soc(t, d) =e= soc(t-1, d) + ch_total(t, d)*alef_ch(d)
                                  - dis_total(t, d)/alef_dis(d);

** Energy storage state of charge limit
soc_limit(t, d)..
        soc(t, d) =l= Emax(d);

** Final enery storage state of charge
eq_soc_final(t, d)$(ord(t) eq card(t))..
        soc(t, d) =e= E_final(d);

*******************************************************************************
*** SOLVER CALL                                                               *
*******************************************************************************

model TEPO_CR /all/;

option reslim = 1000000;
option optcr = 0.01;
option threads = 1;

solve TEPO_CR using mip minimizing obj;

*******************************************************************************
*** CSV OUTPUT FILES                                                          *
*******************************************************************************

** Power extracted from the energy storage device
file pext_output /'C:\BPA_project\Test_connect_DA_new_ok\Data\pext.csv'/;
put pext_output;
put "** Power extracted from energy storage device **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(d,
    put d.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (ch_total.l(t, d) - dis_total.l(t, d)):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (ch_total.l(t, d) - dis_total.l(t, d)):0:4,
    );
    put /;
);

** Defining the ESS actions based on the power extracted
loop((s, d)$(storage_map(d) eq ord(s)),
    action_aux(t, s)$((M_cong(t) eq 1)
                 and ((ch_total.l(t, d) - dis_total.l(t, d)) gt 0)) = 1 + eps;

    action_aux(t, s)$((M_cong(t) eq 1)
                 and ((ch_total.l(t, d) - dis_total.l(t, d)) lt 0)) = -1 + eps;

    action_aux(t, s)$((M_cong(t) eq 1)
                 and ((ch_total.l(t, d) - dis_total.l(t, d)) eq 0)) = 0 + eps;

);

loop((s, d)$(storage_map(d) eq ord(s)),
    action(t, d) = action_aux(t, s);
);

display soc.l, ch_total.l, dis_total.l;

*******************************************************************************
*** OUTPUT FILES FROM TEPO TO DEPO                                            *
*******************************************************************************

option decimals = 6;

file ES_information_output /'C:\BPA_project\Test_connect_DA_new_ok\DEPO\ES_information.csv'/;
put ES_information_output;
put "** MAP ENERGY STORAGE - BUS, AREA, ZONE **"/;
put "** AREA = 122 ----> BPA AREA "/;
put "** ZONE = 468 ----> SNOPUD ZONE "/;
put "BUS, AREA, ZONE",

put /;
loop(d,
    put d.tl:0:0, ","
    put (storage_map(d)):0:0, ",", (storage_area(d)):0:0, ",", (storage_zone(d)):0:0,
put /;
);

** Action required for congestion relief
file Action_output /'C:\BPA_project\Test_connect_DA_new_ok\DEPO\Action.csv'/;
put Action_output;
put "** Action required for congestion relief **"/;
put_utility 'ren' / 'C:\BPA_project\Test_connect_DA_new_ok\DEPO\Action_':0 N:1:0 '.csv':0;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(d,
    put d.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (action(t, d)):0:0, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (action(t, d)):0:0,
    );
    put /;
);

** Load forecast at buses where storage devices are located
file Load_forecast_output /'C:\BPA_project\Test_connect_DA_new_ok\DEPO\Load_forecast.csv'/;
put Load_forecast_output;
put "** Load forecast at storage buses **"/;
put_utility 'ren' / 'C:\BPA_project\Test_connect_DA_new_ok\DEPO\Load_forecast_':0 N:1:0 '.csv':0;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(d,
    put d.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (sum(s$(storage_map(d) eq ord(s)), demand(s, t)*s_base)):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (sum(s$(storage_map(d) eq ord(s)), demand(s, t)*s_base)):0:4,
    );
    put /;
);

