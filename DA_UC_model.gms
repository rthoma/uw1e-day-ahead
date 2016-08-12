** a_DA_UC_model.gms

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

*******************************************************************************
*** VARIABLES                                                                 *
*******************************************************************************

variables
        obj                objective function
        pf(t, l)           line flows
        theta(t, s)        voltage angles
        g(t, i)            generator power output
;

positive variables
        g_lin(t, i, b)           generator block outputs
        slack_solar(r, t)        solar spillage
        slack_wind(w, t)         wind spillage
        slack_fixed(f, t)        fixed spillage
;

binary variables
        v(t, i)        commitment variables
        y(t, i)        start up variables
        z(t, i)        shut down variables
;

equations
        cost                            objective function
        bin_set1(t, i)                  binary logic constraint 1
        bin_set10(t, i)                 binary logic constraint 1_2
        bin_set2(t, i)                  binary logic constraint 2
        min_updown_1(t, i)              initial statuses
        min_updown_2(t, i)              minimum up time constraint
        min_updown_3(t, i)              minimum down time constraint
        slack_wind_constr(t, w)         maximum wind spillage constraint
        slack_solar_constr(t, r)        maximum solar spillage constraint
        slack_fixed_constr(t, f)        maximum fixed spillage constraint
        gen_sum(t, i)                   summation over all blocks
        gen_min(t, i)                   minimum power output of generators
        block_output(t, i, b)           maximum power output of each block
        ramp_limit_min(t, i)            ramp dn constraint
        ramp_limit_max(t, i)            ramp up constraint
        ramp_limit_min_1(t, i)          ramp dn constraint for initial period
        ramp_limit_max_1(t, i)          ramp up constraint for initial period
        line_flow(t, l)                 power flow
        power_balance(t, s)             power balance equation
        voltage_angles_min(t, s)        minimum voltage phase angle limits
        voltage_angles_max(t, s)        maximum voltage phase angle limits
;

alias (t, tt);

*******************************************************************************
*** CONSTRAINTS                                                               *
*******************************************************************************

** Objective function
cost..
    obj =e= sum((t, i), suc_sw(i)*y(t, i) + a(i)*v(t, i)
                + sum(b, g_lin(t, i, b)*k(i, b)))
          + sum((t, r), slack_solar(r, t))*VoRS
          + sum((t, w), slack_wind(w, t))*VoRS
          + sum((t, f), slack_fixed(f, t))*VoFS
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
        g(t, i) =e= sum(b, g_lin(t, i, b));

** Minimum bound for the power output of conventional thermal units
gen_min(t, i)..
        g(t, i) =g= g_min(i)*v(t, i);

** Maximum bounds for the power output of each block
block_output(t, i, b)..
        g_lin(t, i, b) =l= g_max(i, b)*v(t, i);

** Ramp down constraints for the initial period
ramp_limit_min_1(t, i)$(ord(t) eq 1)..
        -ramp_down(i) =l= g(t, i) - g_0(i);

** Ramp down constraints for all periods except the first
ramp_limit_min(t, i)$(ord(t) gt 1)..
        -ramp_down(i) =l= g(t, i) - g(t-1, i);

** Ramp up constraints for the initial period
ramp_limit_max_1(t, i)$(ord(t) eq 1)..
        ramp_up(i) =g= g(t, i) - g_0(i);

** Ramp up constraints for all periods except the first
ramp_limit_max(t, i)$(ord(t) gt 1)..
        ramp_up(i) =g= g(t, i) - g(t-1, i);

** Nodal power balance equations
power_balance(t, s)..
    demand(s, t) =e= 
          sum(i$(gen_map(i) eq ord(s)), g(t, i))
        + sum(f$(fix_map(f) eq ord(s)), fix_deterministic(f, t)
                                      - slack_fixed(f, t))
        + sum(r$(sol_map(r) eq ord(s)), sol_deterministic(t, r)
                                      - slack_solar(r, t))
        + sum(w$(win_map(w) eq ord(s)), wind_deterministic(t, w)
                                      - slack_wind(w, t))
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

** NO LINE FLOW LIMITS IN UC STAGE 1

** Maximum spillage for solar generation
slack_solar_constr(t, r)..
        sol_deterministic(t, r) =g= slack_solar(r, t);

** Maximum spillage for wind generation
slack_wind_constr(t, w)..
        wind_deterministic(t, w) =g= slack_wind(w, t);

** Maximum spillage for fixed resources
slack_fixed_constr(t, f)..
        fix_deterministic(f, t) =g= slack_fixed(f, t);

** NO ENERGY STORAGE CONSTRAINTS IN UC STAGES

*******************************************************************************
*** SOLVER CALL                                                               *
*******************************************************************************

model TEPO_UC /all/;

option reslim = 1000000;
option optcr = 0.01;
option threads = 1;

solve TEPO_UC using mip minimizing obj;

*******************************************************************************
*** CSV OUTPUT FILES                                                          *
*******************************************************************************

** Conventional generator power output
file output1 /'C:\BPA_project\Test_connect_DA_new_ok\Data\gbis.csv'/;
put output1;
put "** Conventional generator power output **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(i,
    put i.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (g.l(t, i)):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (g.l(t, i)):0:4,
    );
    put /;
);

** Conventional generator power output of first block
file output2A /'C:\BPA_project\Test_connect_DA_new_ok\Data\glin_bisA.csv'/;
put output2A;
put "** Power output of block 1 **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(i,
    put i.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (g_lin.l(t, i, 'b1')):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (g_lin.l(t, i, 'b1')):0:4,
    );
    put /;
);

** Conventional generator power output of second block
file output2B /'C:\BPA_project\Test_connect_DA_new_ok\Data\glin_bisB.csv'/;
put output2B;
put "** Power output of block 2 **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(i,
    put i.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (g_lin.l(t, i, 'b2')):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (g_lin.l(t, i, 'b2')):0:4,
    );
    put /;
);

** Conventional generator power output of third block
file output2C /'C:\BPA_project\Test_connect_DA_new_ok\Data\glin_bisC.csv'/;
put output2C;
put "** Power output of block 3 **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(i,
    put i.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (g_lin.l(t, i, 'b3')):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (g_lin.l(t, i, 'b3')):0:4,
    );
    put /;
);

** Wind spillage output
file output3 /'C:\BPA_project\Test_connect_DA_new_ok\Data\slackwindbis.csv'/;
put output3;
put "** Wind spillage **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(w,
    put w.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (slack_wind.l(w, t)):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (slack_wind.l(w, t)):0:4,
    );
    put /;
);

** Solar spillage output
file output4 /'C:\BPA_project\Test_connect_DA_new_ok\Data\slacksolarbis.csv'/;
put output4;
put "** Solar spillage **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(r,
    put r.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (slack_solar.l(r, t)):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (slack_solar.l(r, t)):0:4,
    );
    put /;
);

** Fixed generation spillage output
file output5 /'C:\BPA_project\Test_connect_DA_new_ok\Data\slackfixedbis.csv'/;
put output5;
put "** Fixed generation spillage **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(f,
    put f.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (slack_fixed.l(f, t)):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (slack_fixed.l(f, t)):0:4,
    );
    put /;
);

** Power flow on each transmission line
file output6 /'C:\BPA_project\Test_connect_DA_new_ok\Data\powerflow.csv'/;
put output6;
put "** Transmission line power flows **"/;

loop(t,
    put ",", t.tl:0:0,
);

put /;
loop(l,
    put l.tl:0:0, ","
    loop(t$(ord(t) lt card(t)),
        put (pf.l(t, l)):0:4, ","
    );
    loop(t$(ord(t) eq card(t)),
        put (pf.l(t, l)):0:4,
    );
    put /;
);

