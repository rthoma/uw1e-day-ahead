********************************************************************************
** FIRST STAGE OF THE CONGESTION RELIEF PROBLEM. DAY-AHEAD SCHEDULE
********************************************************************************

$onempty
$offlisting
$offupper
$Offsymlist
$offsymxref
$offuellist
$offuelxref

Option limrow=0, limcol=0, solprint=off, sysout=off;

********************************************************************************
** READING INPUT DATA
********************************************************************************

$include C:\Users\idm\Desktop\BPA_project\Test_connect_DA\input_data.gms

********************************************************************************
** DECLARATION OF FREE VARIABLES, POSITIVE VARIABLES, BINARY VARIABLES
********************************************************************************

variables
         obj                     objective function of the unit commitment
         pf(t,l)                 power flow
         theta(t,s)              voltage angles
         g(t,i)                  power output of generators
;


positive variables
         g_lin(t,i,b)            generator block outputs
         slack_solar(r,t)        solar spillage
         slack_wind(w,t)         wind spillage
         slack_fixed(f,t)        fixed spillage
         slack_flow(l,t)         Relaxations for the transmission capacity constraints
         slack(s,t)              Relaxation of the power balance equation just in case the problem is infeasible
;


binary variables
         v(t,i)                  commitment variable
         y(t,i)                  start up variable
         z(t,i)                  shut down variable
;


equations
         cost                                    objective function
         bin_set1(t,i)                           binary logic constraint 1
         bin_set10(t,i)                          binary logic constraint 1_2
         bin_set2(t,i)                           binary logic constraint 2
         min_updown_1(t,i)                       Initial statuses
         min_updown_2(t,i)                       minimum up time constraint
         min_updown_3(t,i)                       minimum down time constraint
         slack_wind_constr(t,w)                  maximum wind spillage constraint
         slack_solar_constr(t,r)                 maximum solar spillage constraint
         slack_fixed_constr(t,f)                 maximum fixed "spillage" constraint
         gen_sum(t,i)                            summation over all blocks
         gen_min(t,i)                            minimum power output of generators
         block_output(t,i,b)                     maximum power output of each block
         ramp_limit_min(t,i)                     ramp down constraint
         ramp_limit_max(t,i)                     ramp up constraint
         ramp_limit_min_1(i)                     ramp down constraint t=1
         ramp_limit_max_1(i)                     ramp up constraint t=1
         line_flow(t,l)                          power flow
*         line_capacity_min(t,l)                  maximum power flow limits
*         line_capacity_max(t,l)                  minimum power flow limits
         power_balance(t,s)                      power balance equation
         voltage_angles_min(t,s)                 minimum voltage phase angle limits
         voltage_angles_max(t,s)                 maximum voltage phase angle limits

;

** We duplicate the set t, which is going to be needed in the minimum up and
** down time constraints
alias (t,tt);

********************************************************************************
** DEFINITION OF CONSTRAINTS FOR BOTH MODELS
********************************************************************************

** Objective function of the problem which comprises commitment and dispatch
** costs for conventional thermal units, costs of solar, wind and fixed spillage
** and penalization of the relaxations associated with the transmission
** capacity constraints. This last term can be removed if the capacity constraints
** are not taken into account. If not, it would be 0.
cost..
         obj =e= sum((t,i),suc_sw(i)*y(t,i)+a(i)*v(t,i) + sum(b,g_lin(t,i,b)*k(i,b)))
         + sum((t,r), slack_solar(r,t)) * VoRS
         + sum((t,w), slack_wind(w,t)) * VoRS
         + sum((f,t),slack_fixed(f,t))*10000000
*         + sum((s,t),slack(s,t))*100000000
         + sum((l,t),slack_flow(l,t))*100000000
;

** Binary logic between start-up, shutdown, and commitment variables for
** periods greater than 1
bin_set1(t,i)$(ord(t) gt 1)..
         y(t,i) - z(t,i) =e= v(t,i) - v(t-1,i);

** Binary logic between start-up, shutdown, and commitment variables for
** the first period of the optimization horizon
bin_set10(t,i)$(ord(t) = 1)..
         y(t,i) - z(t,i) =e= v(t,i) - onoff_t0(i);

** Relation between start-up and shudown variables in order to avoid simultaneous actions
bin_set2(t,i)..
         y(t,i) + z(t,i) =l= 1;

** Definition of the power output as the summation of the power output of each of the blocks
gen_sum(t,i)..
         g(t,i) =e= sum(b,g_lin(t,i,b));

** Minimum bound for the power output of conventional thermal units
gen_min(t,i)..
         g(t,i) =g= g_min(i)*v(t,i);

** Maximum bounds for the power output of each of the blocks of the conventional thermal units
block_output(t,i,b)..
         g_lin(t,i,b) =l= g_max(i,b)*v(t,i);

** Initial conditions for the minimum up and down time constraints
min_updown_1(t,i)$(L_up_min(i)+L_down_min(i) gt 0 and ord(t) le L_up_min(i)+L_down_min(i))..
         v(t,i) =e= onoff_t0(i);

** Minimum up time constraints for the rest of the periods
min_updown_2(t,i)$(ord(t) gt L_up_min(i))..
         sum(tt$(ord(tt) ge ord(t)-g_up(i)+1 and ord(tt) le ord(t)),y(tt,i)) =l= v(t,i);

** Minimum down time constraints for the rest of the periods
min_updown_3(t,i)$(ord(t) gt L_down_min(i))..
         sum(tt$(ord(tt) ge ord(t)-g_down(i)+1 and ord(tt) le ord(t)),z(tt,i)) =l= 1-v(t,i);

** Ramp down constraints for periods greater than 1
ramp_limit_min(t,i)$(ord(t) gt 1)..
         -ramp_down(i) =l= g(t,i) - g(t-1,i);

** Ramp up constraints for periods greater than 1
ramp_limit_max(t,i)$(ord(t) gt 1)..
         ramp_up(i) =g= g(t,i) - g(t-1,i);

** Ramp down constraints for the initial period
ramp_limit_min_1(i)..
         -ramp_down(i) =l= g('t1',i) - g_0(i);

** Ramp up constraints for the initial period
ramp_limit_max_1(i)..
         ramp_up(i) =g= g('t1',i) - g_0(i);

** Nodal power balance equations including the power output of conventional thermal units
** fixed generation, solar generation, wind generation, in and out flows, and the nodal demand
power_balance(t,s)..
         sum(i$(gen_map(i)=ord(s)),g(t,i))
        + sum(f$(fix_map(f)=ord(s)), fix_deterministic(f,t)-slack_fixed(f,t)) +
         sum(r$(sol_map(r)=ord(s)), sol_deterministic(t,r)-slack_solar(r,t)) +
         sum(w$(win_map(w)=ord(s)), wind_deterministic(t,w)-slack_wind(w,t))
         -sum(l$(line_map(l,'from') = ord(s)),pf(t,l)) +
         sum(l$(line_map(l,'to') = ord(s)),pf(t,l))
         =e= demand(s,t)
*-slack(s,t)
;

** Definition of the power flow of each line in terms of the voltage phase angles
line_flow(t,l)..
         pf(t,l) =e= admittance(l)*(sum(s$(line_map(l,'from')= ord(s)),theta(t,s))-sum(s$(line_map(l,'to')= ord(s)),theta(t,s)));

** Transmission capacity constraints
*line_capacity_min(t,l)..
*         pf(t,l) =g= -l_max(l)
*-slack_flow(l,t)
*;

** Transmission capacity constraints
*line_capacity_max(t,l)..
*         pf(t,l) =l= l_max(l)
*+slack_flow(l,t)
*;

** Minimum voltage phase angle limits
voltage_angles_min(t,s)..
         theta(t,s) =g= -pi;

** Maximum voltage phase angle limits
voltage_angles_max(t,s)..
         theta(t,s) =l= pi;

** Maximum spillage for solar generation
slack_solar_constr(t,r)..
         sol_deterministic(t,r)=g=slack_solar(r,t);

** Maximum spillage for wind generation
slack_wind_constr(t,w)..
         wind_deterministic(t,w)=g=slack_wind(w,t);

** Maximum spillage for fixed generation
slack_fixed_constr(t,f)..
         fix_deterministic(f,t)=g=slack_fixed(f,t);


** Definition of the model
model TEPO_UC /all/;

********************************************************************************
** OPTIONS FOR THE SIMULATIONS: TIME LIMITATION, GAP, NUMBER OF THREADS,
** INITIALIZATION, ...
********************************************************************************

option reslim = 1000000;
*option Savepoint=1;
option optcr=0.01;
option threads = 1;
*option optca=0;

********************************************************************************
** SOLVING THE UC PROBLEM FOR THE DAY-AHEAD OPERATION
********************************************************************************

** Defintion of parameters that we want to output
Parameter slack_solar_out_total,slack_wind_out_total,slack_fixed_out_total,slack_solar_out(r,t),slack_wind_out(w,t),slack_fixed_out(f,t),power_flow_out(iter,t,l),mst(iter), sst(iter),time_elapsed(iter),M_cong_aux(iter,t,l),M_cong_snpd_aux(iter,t,l),flow_cong_output(iter,l,t),total_cost(iter),generation_cost(iter);


** Solve the minimization problem by using mixed-integer linear programming
solve TEPO_UC using mip minimizing obj;

total_cost(iter)$(ord(iter) eq counter)   =  obj.L;
generation_cost(iter)$(ord(iter) eq counter) = sum((t,i),suc_sw(i)*y.l(t,i)+a(i)*v.l(t,i) + sum(b,g_lin.l(t,i,b)*k(i,b))) +eps;
time_elapsed(iter)$(ord(iter) eq counter)  = timeElapsed;
M_cong_aux(iter,t,l)$(abs(pf.l(t,l))-l_max(l) ge 0 and ord(iter) eq counter)=1+eps;
M_cong_snpd_aux(iter,t,l)$(abs(pf.l(t,l))-l_max(l) ge 0 and snpd_lines_map(l) eq 1 and ord(iter) eq counter)=1+eps;
*flow_cong_output(iter,l,t)$(M_cong_aux(iter,t,l) eq 1 and ord(iter) eq counter)=0+eps;
flow_cong_output(iter,l,t)$(ord(iter) eq counter)=pf.l(t,l)*s_base+eps;
mst(iter)$(ord(iter) eq counter)=TEPO_UC.modelstat;
sst(iter)$(ord(iter) eq counter)=TEPO_UC.solvestat;
power_flow_out(iter,t,l)$(ord(iter) eq counter)   = pf.l(t,l)*s_base+eps;
slack_solar_out(r,t) = slack_solar.l(r,t)*s_base+eps;
slack_wind_out(w,t) = slack_wind.l(w,t)*s_base+eps;
slack_fixed_out(f,t) =  slack_fixed.l(f,t) *s_base+eps;
slack_solar_out_total = sum((r,t),slack_solar.l(r,t))*s_base+eps;
slack_wind_out_total = sum((w,t),slack_wind.l(w,t))*s_base+eps;
slack_fixed_out_total = sum((f,t), slack_fixed.l(f,t)) *s_base+eps;


** Output of results in a gdx file
execute_unload "C:\Users\idm\Desktop\BPA_project\Test_connect_DA\uc_unconstrained_pre_day2_1ES.gdx" slack_solar_out_total,slack_wind_out_total,slack_fixed_out_total,slack_solar_out,slack_wind_out,slack_fixed_out,power_flow_out,mst, sst,total_cost,M_cong_snpd_aux, generation_cost, time_elapsed, M_cong_aux,flow_cong_output ;


** Output of the power from conventional thermal generators
FILE output1 /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\gbis.csv'/;
put output1
put "** Power output **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(i,
put i.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (g.l(t,i)):0:4,","
);
loop(t$(ord(t) eq card(t)),
put (g.l(t,i)):0:4,
);
put /;
);


** Output of the power for the first block of conventional thermal units
FILE output2A /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\glin_bisA.csv'/;
put output2A
put "** Power output of block 1 **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(i,
put i.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (g_lin.l(t,i,'b1')):0:4,","
);
loop(t$(ord(t) eq card(t)),
put (g_lin.l(t,i,'b1')):0:4,
);
put /;
);

** Output of the power for the second block of conventional thermal units
FILE output2B /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\glin_bisB.csv'/;
put output2B
put "** Power output of block 2 **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(i,
put i.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (g_lin.l(t,i,'b2')):0:4,","
);
loop(t$(ord(t) eq card(t)),
put (g_lin.l(t,i,'b2')):0:4,
);
put /;
);

** Output of the power for the third block of conventional thermal units
FILE output2C /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\glin_bisC.csv'/;
put output2C
put "** Power output of block 3 **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(i,
put i.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (g_lin.l(t,i,'b3')):0:4,","
);
loop(t$(ord(t) eq card(t)),
put (g_lin.l(t,i,'b3')):0:4,
);
put /;
);

** Output of the wind spillage
FILE output3 /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\slackwindbis.csv'/;
put output3
put "** Wind spillage **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(w,
put w.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (slack_wind.l(w,t)):0:4,","
);
loop(t$(ord(t) eq card(t)),
put (slack_wind.l(w,t)):0:4,
);
put /;
);

** Output of the solar spillage
FILE output4 /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\slacksolarbis.csv'/;
put output4
put "** Solar spillage **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(r,
put r.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (slack_solar.l(r,t)):0:4,","
);
loop(t$(ord(t) eq card(t)),
put (slack_solar.l(r,t)):0:4,
);
put /;
);

** Output of the fixed "spillage"
FILE output5 /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\slackfixedbis.csv'/;
put output5
put "** Fixed spillage **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(f,
put f.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (slack_fixed.l(f,t)):0:4,","
);
loop(t$(ord(t) eq card(t)),
put (slack_fixed.l(f,t)):0:4,
);
put /;
);

** Power flow of each transmission line
FILE output6 /'C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\powerflow.csv'/;
put output6
put "** Power flow **"/;
loop(t,
  put ",",t.tl:0:0,
);
put /;
loop(l,
put l.tl:0:0,","
loop(t$(ord(t) lt card(t)),
put (pf.l(t,l)):0:3,","
);
loop(t$(ord(t) eq card(t)),
put (pf.l(t,l)):0:3,
);
put /;
);
