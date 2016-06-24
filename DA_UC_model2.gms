********************************************************************************
** FOURTH STAGE OF THE CONGESTION RELIEF PROBLEM. DAY-AHEAD SCHEDULE
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

** We assume that the total injections/extractions from ES are accepted by DEPO
** In the general case, DEPO would provide a slightly different schedule or not
$include C:\Users\idm\Desktop\BPA_project\Test_connect_DA\input_data.gms

Table p_ext2(d,t)
$ondelim
$include C:\Users\idm\Desktop\BPA_project\Test_connect_DA\Data\pext_2round.csv
$offdelim
;

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
         slack_flow(l,t)         relaxation of the transmission capacity constraints
         slack(s,t)              relaxation of the power balance equations
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
         slack_fixed_constr(t,f)                 maximum fixed spillage constraint
         gen_sum(t,i)                            summation over all blocks
         gen_min(t,i)                            minimum power output of generators
         block_output(t,i,b)                     maximum power output of each block
         ramp_limit_min(t,i)                     ramp down constraint
         ramp_limit_max(t,i)                     ramp up constraint
         ramp_limit_min_1(i)                     ramp down constraint t=1
         ramp_limit_max_1(i)                     ramp up constraint t=1
         line_flow(t,l)                          power flow
         line_capacity_min(t,l)                  maximum power flow limits
         line_capacity_max(t,l)                  minimum power flow limits
         power_balance(t,s)                      power balance equation
         voltage_angles_min(t,s)                 minimum voltage phase angle limits
         voltage_angles_max(t,s)                 maximum voltage phase angle limits

;
alias (t,tt);

********************************************************************************
** DEFINITION OF CONSTRAINTS FOR BOTH MODELS
********************************************************************************

** The formulation is the same as that in stage 1 but we now include the transmission capacity constraints
cost..
         obj =e= sum((t,i),suc_sw(i)*y(t,i)+a(i)*v(t,i) + sum(b,g_lin(t,i,b)*k(i,b)))
         + sum((t,r), slack_solar(r,t)) * VoRS
         + sum((t,w), slack_wind(w,t)) * VoRS
         + sum((f,t),slack_fixed(f,t))*10000000
*         + sum((s,t),slack(s,t))*100000000
         + sum((l,t),slack_flow(l,t))*100000000
;

bin_set1(t,i)$(ord(t) gt 1)..
         y(t,i) - z(t,i) =e= v(t,i) - v(t-1,i);

bin_set10(t,i)$(ord(t) = 1)..
         y(t,i) - z(t,i) =e= v(t,i) - onoff_t0(i);

bin_set2(t,i)..
         y(t,i) + z(t,i) =l= 1;

gen_sum(t,i)..
         g(t,i) =e= sum(b,g_lin(t,i,b));

gen_min(t,i)..
         g(t,i) =g= g_min(i)*v(t,i);

block_output(t,i,b)..
         g_lin(t,i,b) =l= g_max(i,b)*v(t,i);

min_updown_1(t,i)$(L_up_min(i)+L_down_min(i) gt 0 and ord(t) le L_up_min(i)+L_down_min(i))..
         v(t,i) =e= onoff_t0(i);

min_updown_2(t,i)$(ord(t) gt L_up_min(i))..
         sum(tt$(ord(tt) ge ord(t)-g_up(i)+1 and ord(tt) le ord(t)),y(tt,i)) =l= v(t,i);

min_updown_3(t,i)$(ord(t) gt L_down_min(i))..
         sum(tt$(ord(tt) ge ord(t)-g_down(i)+1 and ord(tt) le ord(t)),z(tt,i)) =l= 1-v(t,i);

ramp_limit_min(t,i)$(ord(t) gt 1)..
         -ramp_down(i) =l= g(t,i) - g(t-1,i);

ramp_limit_max(t,i)$(ord(t) gt 1)..
         ramp_up(i) =g= g(t,i) - g(t-1,i);

ramp_limit_min_1(i)..
         -ramp_down(i) =l= g('t1',i) - g_0(i);

ramp_limit_max_1(i)..
         ramp_up(i) =g= g('t1',i) - g_0(i);

power_balance(t,s)..
         sum(i$(gen_map(i)=ord(s)),g(t,i))
        + sum(f$(fix_map(f)=ord(s)), fix_deterministic(f,t)-slack_fixed(f,t)) +
         sum(r$(sol_map(r)=ord(s)), sol_deterministic(t,r)-slack_solar(r,t)) +
         sum(w$(win_map(w)=ord(s)), wind_deterministic(t,w)-slack_wind(w,t))
         -sum(l$(line_map(l,'from') = ord(s)),pf(t,l)) +
         sum(l$(line_map(l,'to') = ord(s)),pf(t,l))
         =e= demand(s,t) +sum(d$(storage_map(d) eq ord(s)), p_ext2(d,t))
*  -slack(s,t)
;


line_flow(t,l)..
         pf(t,l) =e= admittance(l)*(sum(s$(line_map(l,'from')= ord(s)),theta(t,s))-sum(s$(line_map(l,'to')= ord(s)),theta(t,s)));

line_capacity_min(t,l)..
         pf(t,l) =g= -l_max(l)-slack_flow(l,t);
*-slack_flow(l,t)

line_capacity_max(t,l)..
         pf(t,l) =l= l_max(l)+slack_flow(l,t) ;
*+slack_flow(l,t)

voltage_angles_min(t,s)..
         theta(t,s) =g= -pi;

voltage_angles_max(t,s)..
         theta(t,s) =l= pi;

slack_solar_constr(t,r)..
         sol_deterministic(t,r)=g=slack_solar(r,t);

slack_wind_constr(t,w)..
         wind_deterministic(t,w)=g=slack_wind(w,t);

slack_fixed_constr(t,f)..
         fix_deterministic(f,t)=g=slack_fixed(f,t);


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

Parameter slack_flow_out(l,t),slack_solar_out_total,slack_wind_out_total,slack_fixed_out_total,power_output_out(t,i),slack_solar_out(r,t),slack_wind_out(w,t),slack_fixed_out(f,t),power_flow_out(t,l),mst, sst,time_elapsed,M_cong_aux(t,l),M_cong_snpd_aux(t,l),flow_cong_output(l,t),total_cost,generation_cost;


solve TEPO_UC using mip minimizing obj;

total_cost   =  obj.L;
generation_cost= sum((t,i),suc_sw(i)*y.l(t,i)+a(i)*v.l(t,i) + sum(b,g_lin.l(t,i,b)*k(i,b))) +eps;
time_elapsed  = timeElapsed;
M_cong_aux(t,l)$(abs(pf.l(t,l))-l_max(l) ge 0 )=1+eps;
M_cong_snpd_aux(t,l)$(abs(pf.l(t,l))-l_max(l) ge 0 and snpd_lines_map(l) eq 1)=1+eps;
flow_cong_output(l,t)$(M_cong_aux(t,l) eq 1 )=0+eps;
flow_cong_output(l,t)$(M_cong_aux(t,l) ne 0 )=pf.l(t,l)*s_base+eps;
mst=TEPO_UC.modelstat;
sst=TEPO_UC.solvestat;
power_flow_out(t,l)   = pf.l(t,l)*s_base+eps;
power_output_out(t,i) = g.l(t,i)*s_base+eps;
slack_solar_out(r,t) = slack_solar.l(r,t)*s_base+eps;
slack_wind_out(w,t) = slack_wind.l(w,t)*s_base+eps;
slack_fixed_out(f,t) =  slack_fixed.l(f,t) *s_base+eps;
slack_solar_out_total = sum((r,t),slack_solar.l(r,t))*s_base+eps;
slack_wind_out_total = sum((w,t),slack_wind.l(w,t))*s_base+eps;
slack_fixed_out_total = sum((f,t), slack_fixed.l(f,t)) *s_base+eps;
slack_flow_out(l,t)= slack_flow.l(l,t)*s_base+eps;


execute_unload "C:\Users\idm\Desktop\BPA_project\Test_connect_DA\uc_constrained_post_day2_relieved_1ES.gdx" slack_flow_out,slack_solar_out_total,slack_wind_out_total,slack_fixed_out_total,power_output_out,slack_solar_out,slack_wind_out,slack_fixed_out,power_flow_out,mst, sst,total_cost,M_cong_snpd_aux, generation_cost, time_elapsed, M_cong_aux,flow_cong_output ;



