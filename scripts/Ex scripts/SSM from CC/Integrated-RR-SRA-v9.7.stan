// Multi-stock state-space run reconstruction and spawner-recruitment model 
// Adapted from multi-stock run reconstruction by Hamachan Hamazaki, Alaska Department of Fish and Game
//  and state-space spawner-recruitment model by Steve Fleischman, retired Alaska Department of Fish and Game
// Nov 2023:  Removed input data unused in the model:  Hamachan Hamazaki 
//			  Standardized sd input to cv for eagel, rtmr, pilot 
//			  Removed internal harvest below above Pilot Calculation and added 
//			  Precalculated data



data {  
  // Data bounding integers
//  int fyear;		
//  int lyear;
  int n_year;
//  int years[n_year];
//  int nwkc;
//  int nwku;

  // Canada stock data 
  vector[n_year] s_eagle;         // Eagle Sonar Passage estimate
  vector[n_year] s_eagle_cv;      // Eagle Sonar Passage CV 
  vector[n_year] r_mr;            // Radio Telemetry mark-recapture estimate
  vector[n_year] r_mr_cv;         // Radio Telemetry mark-recapture CV
  vector[n_year] a_tinc;          // Tincup Aerial 
  vector[n_year] a_tinc_cv;       // Tincup Aerial CV
  vector[n_year] f_tatc;          // Tatchun Creek foot survey
  vector[n_year] f_tatc_cv;       // Tatchum Creek foot survey CV
  vector[n_year] a_lsal;          // Little Salmon Aerial
  vector[n_year] a_lsal_cv;       // Little Salmon Aerial CV
  vector[n_year] a_nstl;          // Nisutlin Aerial
  vector[n_year] a_nstl_cv;       // Nisultin Aerial CV 
  vector[n_year] a_ross;          // Ross Aerial
  vector[n_year] a_ross_cv;       // Ross Aerial CV
  vector[n_year] a_wolf;          // Wolf Aerial
  vector[n_year] a_wolf_cv;       // Wolf Aerial CV
  vector[n_year] w_blnd;          // Blind Creek Weir
  vector[n_year] w_blnd_cv;       // Blind Creek Weir CV
  vector[n_year] d_whte;          // Whitehorse fishway
  vector[n_year] d_whte_cv;       // Whitehorse fishway CV      
  vector[n_year] s_bsal;          // Big Salmon Sonar       
  vector[n_year] s_bsal_cv;       // Big Salmon Sonar CV
  vector[n_year] a_bsal;          // Big Salmon Sonar 
  vector[n_year] a_bsal_cv;       // Big Salmon Sonar CV
  vector[n_year] a_takh;          // Takhini Aerial 
  vector[n_year] a_takh_cv;       // Takhini Aerial CV
  vector[n_year] f_mr;            // Fishwheel mark-recapture CV
  vector[n_year] f_mr_cv;         // Fishwheel mark-recapture CV

  // US stocks data 
  matrix[3, n_year] pilot_p;      //  Pilot Genetic Stock proportion data

  // RT MR data
  vector[n_year] t_rtmr;          // Radio Telemetry Mark Recapture Total
  vector[n_year] t_rtmr_cv;       // Radio Telemetry Mark Recapture Total CV
//  vector[n_year] tan_rtmr;        // Radio Telemetry Mark Recapture Tanana
//  vector[n_year] tan_rtmr_sd;     // Radio Telemetry Mark Recapture Tanana

  // Pilot station sonar data
  vector[n_year] s_pilot;         // Pilot Station Sonar
  vector[n_year] s_pilot_cv;      // Pilot Station Sonar CV
  vector[n_year] wrunp;           // year-spp sample size at Pilot for stock prop

  // Middle stock escapement data
  vector[n_year] t_chen;          // Chena Tower
  vector[n_year] t_chen_cv;       // Chena Tower CV
  vector[n_year] t_salc;          // Salcha Tower
  vector[n_year] t_salc_cv;       // Salcha Tower CV
  vector[n_year] w_henw;          // Henshaw Creek Weir
  vector[n_year] w_henw_cv;       // Henshaw Creek Weir CV
  vector[n_year] a_chen;          // Chena Aerial
  vector[n_year] a_chen_cv;       // Chena Aerial CV
  vector[n_year] a_salc;          // Salcha Aerial
  vector[n_year] a_salc_cv;       // Salcha Aerial CV
  
  // Lower stock escapement data
  vector[n_year] a_anvk;          // Anvik Aerial
  vector[n_year] a_anvk_cv;       // Anvik Aerial CV
  vector[n_year] t_nult;          // Nulato Tower
  vector[n_year] t_nult_cv;       // Nulato Tower CV
  vector[n_year] a_nult_n;        // Nulato North Aerial
  vector[n_year] a_nult_n_cv;     // Nulato North Aerial CV
  vector[n_year] a_nult_s;        // Nulato South Aerial
  vector[n_year] a_nult_s_cv;     // Nulato South Aerial CV
  vector[n_year] w_gisa;          // Gisasa Weir
  vector[n_year] w_gisa_cv;       // Gisasa Weir CV
  vector[n_year] a_gisa;          // Gisasa Aerial
  vector[n_year] a_gisa_cv;       // Gisasa Aerial CV
  vector[n_year] w_andr;          // Andreafsky Weir
  vector[n_year] w_andr_cv;       // Andreafsky Weir CV
  vector[n_year] a_andr_w;        // Andreafsky West Aerial
  vector[n_year] a_andr_w_cv;     // Andreafsky West Aerial CV
  vector[n_year] a_andr_e;        // Andreafsky East Aerial
  vector[n_year] a_andr_e_cv;     // Andreafsky East Aeiral CV
  vector[n_year] w_toz;           // Tozitna Weir
  vector[n_year] w_toz_cv;        // Tozitna Weir CV
  vector[n_year] a_toz;           // Tozitna Aerial
  vector[n_year] a_toz_cv;        // Tozitna Aerial CV

  //  Harvest by stock data
  vector[n_year] h_can;     // Canada harvest
  vector[n_year] h_up;      // Above Pilot harvest by stock
  vector[n_year] h_low;     // Below Pilot harvest by stock
  matrix[3, n_year] ph_up;  // Above Pilot harvest stock prop
  matrix[3, n_year] ph_low; // Below Pilot harvest stock prop
  vector[n_year] p_n_up;          // year-spp sample size of upper river harvest for stock prop
  vector[n_year] p_n_low;         // year-spp sample size of lower river harvest for stock prop
  vector[n_year] hp_cv;     // Asssumed Harvest CV

  // Control parameters 
  int nyp[2];                    // These are no longer years, but year references
  int nye[2];

  
  // SRA data 
//  int nyrs;                     // Number of years in SRA model
  int nRyrs;                    // Number of recruitment years in SRA model
  int A;                        // Number of (total) age classes
  int a_min;                    // Minimum total age
  int a_max;                    // Maximum total age
  matrix [n_year, A] H_comps_bp;  // Below pilot CDN harvest observed age composition in proportion by age class
  matrix [n_year, A] H_comps_ap;  // Above pilot CDN harvest observed age composition in proportion by age class
//  matrix [nyrs, A] H_comps_cdn; // CDN harvest observed age composition in proportion by age class
  matrix [n_year, A] S_comps;     // Border observed age composition in proportion by age class
  vector [n_year] ess_age_comp;   // Effective input sample size for age comp "observations"
  
}

parameters {
  vector<lower=9.0, upper=13.0>[n_year] log_can_run;  // log Cananda stock Run
  vector<lower=9.0, upper=13.0>[n_year] log_mid_run;  // log Mid stock Run
  vector<lower=9.0, upper=13.0>[n_year] log_low_run;  // log Low stock Run

  // Canada escapement parameters slope
  real<lower=0.0, upper=6.0> log_atinc;   // log transformed slope for Tincap Aerial
  real<lower=0.0, upper=6.0> log_ftatc;   // log transformed slope for Tatchun Creek foot survey
  real<lower=0.0, upper=6.0> log_alsal;   // log transformed slope for Little Salmon Aerial
  real<lower=0.0, upper=6.0> log_anstl;   // log transformed slope for Nisutlin Aerial
  real<lower=0.0, upper=6.0> log_aross;   // log transformed slope for Ross Aerial
  real<lower=0.0, upper=6.0> log_awolf;   // log transformed slope for Wolf Aerial
  real<lower=0.0, upper=6.0> log_wblnd;   // log transformed slope for Blind Creek Weir
  real<lower=0.0, upper=6.0> log_dwhte;   // log transformed slope for Whitehorse Fishway passage
  real<lower=0.0, upper=6.0> log_sbsal;   // log transformed slope for Big Salmon Sonar
  real<lower=0.0, upper=6.0> log_absal;   // log transformed slope for Big Salmon Aerial
  real<lower=0.0, upper=6.0> log_atakh;   // log transformed slope for Takhini Aerial
  real<lower=-5.0, upper=0.0> log_qfwmr;  // survey q Border Mark Recapture
  
  // Canada escapement parameters dispersion   
  real<lower=-5.0, upper=5.0> log_rcanair;  // log transformed slope for Tincap Aerial
  real<lower=-5.0, upper=5.0> log_rcang;    // log transformed slope for Blind Creek Weir
  vector<lower=-15.0, upper=2.0>[2] log_rfwmr;   // log transformed slope for Border Mark Recapture
  
  // US Escapement Parameters Slope 
  real<lower=0.0, upper=6.0> log_tchen;  // log transformed slope for Chena tower
  real<lower=0.0, upper=6.0> log_tsalc;  // log transformed slope for Salcha tower
  real<lower=0.0, upper=6.0> log_whenw;  // log transformed slope for Henshaw Weir
  real<lower=0.0, upper=6.0> log_achen;       // log transformed slope for Chena Aerial
  real<lower=0.0, upper=6.0> log_asalc;       // log transformed slope for Salcha Aerial
  real<lower=0.0, upper=6.0> log_wgisa;       // log transformed slope for Gisasa Weir
  real<lower=0.0, upper=6.0> log_aanvk;       // log transformed slope for Anvik Aerial
  real<lower=0.0, upper=6.0> log_tnult;       // log transformed slope for Nulato Tower
  real<lower=0.0, upper=6.0> log_anult;       // log transformed slope for Nulato Aerial
  real<lower=0.0, upper=6.0> log_agisa;       // log transformed slope for Gisasa Aerial
  real<lower=0.0, upper=6.0> log_wandr;       // log transformed slope for Andreafsky Weir
  real<lower=0.0, upper=6.0> log_aandr;       // log transformed slope for Andreafsky Aerial
  real<lower=0.0, upper=6.0> log_wtoz;        // log transformed slope for Tozitna Weir
  real<lower=0.0, upper=6.0> log_atoz;        // log transformed slope for Tozitna Aerial
  
  real<lower=-5.0, upper=0.0> log_nultp;      // proportion of Nulato North
  real<lower=-5.0, upper=0.0> log_andrep;     // proporiotn of Andreafsky West
  vector<lower=-10.0, upper=1.0>[3] log_qplt;  // Survey q Pilot Sonar

  // US Escapement parameters dispersion 
  real<lower=-5.0, upper=1.0> log_rtower;       // log transformed dispersion for Chena tower
  real<lower=-10.0, upper=1.0> log_raerial;     // log transformed dispersion for Chena Aerial
  vector<lower=-10.0, upper=10.0>[4] log_rspilt; // log transformed dispersion for Pilot Sonar
  
  // Fishing mortality by stock   
  vector<lower=0.0, upper=4.0>[n_year] log_fcu;    // Fishing mortality upriver: Canada stock
  vector<lower=0.0, upper=4.0>[n_year] log_fmu;    // Fishing mortality upriver: Middle stock
  vector<lower=0.0, upper=4.0>[n_year] log_flu;    // Fishing mortality upriver: Lower stock
  vector<lower=0.0, upper=4.0>[n_year] log_fcl;    // Fishing mortality lower river: Canada stock
  vector<lower=0.0, upper=4.0>[n_year] log_fml;    // Fishing mortality lower river: Middle stock
  vector<lower=0.0, upper=4.0>[n_year] log_fll;    // Fishing mortality lower river: Lower stock
  vector<lower=0.0, upper=4.0>[n_year] log_fcc;    // Fishing mortality lower river: Lower stock
  
  // SRA parameters 
  vector<lower=0>[nRyrs] lnR;        // log recruitment states
  real<lower=0> lnalpha;             // log Ricker a
  real<lower=0> beta;                // Ricker b
  real<lower=0> sigma_R;             // Process error
  real<lower=0> sigma_R0;            // Process error for first a.max years with no spawner link
  real<lower=-1,upper=1> phi;        // lag-1 correlation in process error
  real lnresid_0;                    // First residual for lag-1 correlation in process error
  real<lower=0> mean_ln_R0;          // "true" mean log recruitment in first a.max years with no spawner link
  vector<lower=0,upper=1>[3] prob;   // Maturity schedule probs
  real<lower=0,upper=1> D_scale;     // Variability of age proportion vectors across cohorts
  matrix<lower=0.01> [nRyrs, A] g;    // Individual year/age class gamma variates for generating age at maturity proportions
  real<lower=0> sigma_can_run;       // Lognormal standard deviation for comparison of can_run with sum(N_ta)
}

transformed parameters {
  //  Transformed numbers   
  vector[n_year] can_run;	//Total Canada stock run
  vector[n_year] mid_run; //Total Millde stock run
  vector[n_year] low_run;	//Total Lower stock run 

  // Canada escapement parameters slope
  real atinc;       //  Slope for Tincap Aerial
  real ftatc;       //  Slope for Tincap foot survey
  real alsal;       //  Slope for Little Salmon Aerial
  real absal;       //  Slope for Big Salmon Aerial
  real anstl;       //  Slope for Nisutlin Aerial
  real aross;       //  Slope for Ross Aerial
  real awolf;       //  Slope for Wolf Aerial
  real wblnd;       //  Slope for Blind Creek Weir
  real sbsal;       //  Slope for Big Salmon Sonar
  real atakh;       //  Slope for Takhini Aerial
  real dwhte;       //  Slope for Whitehorse Fishway passage
  real qfwmr;

  // Canada escapement parameters Dispersion 
  real rcanair;     // Additional var for Canada Aerial
  real rcang;       // Additional var for Canada ground
  vector[2] rfwmr;  // Additional var for Border Mark Recapture

  // US escapement parameters slope
  real tchen;       //  Slope for Chena tower
  real tsalc;       //  Slope for Salcha tower
  real whenw;       //  Slope for Henshaw Weir
  real tnult;       //  Slope for Nulato Tower
  real wgisa;       //  Slope for Gisasa Weir
  real wandr;       //  Slope for Andreafsky Weir
  real achen;       //  Slope for Chena Aerial
  real asalc;       //  Slope for Salcha Aerial
  real anult;       //  Slope for Nulato Aerial
  real agisa;       //  Slope for Gisasa Aerial
  real aanvk;       //  Slope for Anvik Aerial
  real aandr;       //  Slope for Andreafsky Aerial
  real wtoz;        //  Slope for Tozitna Weir
  real atoz;        //  Slope for Tozitna Aerial
  vector[3] qplt;   // Survey q Pilot Sonar
  real nultp;       // proportion of Nulato North
  real andrep;      // proportion of Andrefsky E

  // US escapement parameters dispersion 
  real rtower;       //  Additional var for tower
  real raerial;      //  Additional var for Aerial
  vector[4] rspilt;  //  Additional var for Pilot Sonar
 
  // Define working variables for calculation of objective functions
  vector[n_year] t_run;	    //Total run
  vector[n_year] t_esc;	    //Total esc
  vector[n_year] canesc;	  //Canada Stock Escapement
  vector[n_year] midesc;	  //Middle Stock Escapement
  vector[n_year] lowesc;	  //Lower Stock Escapement

  vector[n_year] andre_e;   // Andreafsky escapement
  vector[n_year] lowesc_u;  // Lower stock escapement up river Pilot
  vector[n_year] canpass;   // Canada border passage

  vector[n_year] eh_can;    // Est Canada Canada stock harvest
  matrix[3, n_year] eh_up;  // Est Above Pilot Canada stock harvest
  matrix[3, n_year] eh_low; // Est Below Pilot Canada stock harvest
  matrix[3, n_year] plt_et; //Est Pilot run
  matrix[3, n_year] plt_et_p;    //Est Pilot stock proportion
  vector[n_year] plt_t;

  matrix[3, n_year] eph_up; // Est stock proportion above Pilot
  matrix[3, n_year] eph_low;  // Est stock proportion below Pilot
  
// Define likelihood variables  
  vector[2] offset;
 
// Define SRA Transformed parameters ============================
  vector[n_year] lnS;                      // log spawner states
  vector<lower=0>[nRyrs] R;              // Recruitment states
  vector[nRyrs] lnresid;                 // log recruitment residuals
  vector[nRyrs] lnRm_1;                  // log recruitment states in absence of lag-one correlation
  vector[nRyrs] lnRm_2;                  // log recruitment states after accounting for lag-one correlation
  matrix<lower=0>[n_year, A] N_ta;         // Returns by age matrix
  matrix<lower=0, upper=1>[nRyrs, A] p;  // Age at maturity proportions
  vector<lower=0,upper=1>[4] pi;         // Maturity schedule probs
  real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
  vector<lower=0>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
  matrix<lower=0, upper=1>[n_year, A] q;   // Age composition by year/age classr matrix
  matrix<lower=0>[n_year, A] o_run;        // Observed run size by age class
  matrix<lower=0, upper=1>[n_year, A] o_run_comp; // Observed age composition by year
  
  // Convert parameters into rates  
  //  Transformed Run Numbers   
   can_run = exp(log_can_run);       // Canada stock Run
   mid_run = exp(log_mid_run);       // Mid stock Run
   low_run = exp(log_low_run);       // Low stock Run 
  
  // Canada escapement parameters slope
   atinc = exp(log_atinc);       //  Slope for Tincap Aerial
   ftatc = exp(log_ftatc);       //  Slope for Tincap foot survey 
   alsal = exp(log_alsal);       //  Slope for Little Salmon Aerial
   absal = exp(log_absal);       //  Slope for Big Salmon Aerial
   anstl = exp(log_anstl);       //  Slope for Nisutlin Aerial
   aross = exp(log_aross);       //  Slope for Ross Aerial
   awolf = exp(log_awolf);       //  Slope for Wolf Aerial
   wblnd = exp(log_wblnd);       //  Slope for Blind Creek Weir    
   sbsal = exp(log_sbsal);       //  Slope for Big Salmon Sonar
   dwhte = exp(log_dwhte);       //  Slope for Whitehorse Fishway passage
   atakh = exp(log_atakh);       //  Slope for Takhini Aerial
   qfwmr = exp(log_qfwmr);
  
  // Canada escapement parameters dispersion   
   rcanair = exp(log_rcanair);   //  Dispersion for Tincap Aerial
   rcang = exp(log_rcang);       //  Dispersion for Blind Creek Weir    
   rfwmr = exp(log_rfwmr);       //  Dispersion for Border Mark Recapture

  // US escapement parameters slope
   tchen = exp(log_tchen);       //  Slope for Chena tower
   tsalc = exp(log_tsalc);       //  Slope for Salcha tower 
   tnult = exp(log_tnult);       //  Slope for Nulato Tower
   wgisa = exp(log_wgisa);       //  Slope for Gisasa Weir  
   whenw = exp(log_whenw);       //  Slope for Henshaw Weir
   wandr = exp(log_wandr);       //  Slope for Andreafsky Weir 
   achen = exp(log_achen);       //  Slope for Chena Aerial
   asalc = exp(log_asalc);       //  Slope for Salcha Aerial   
   anult = exp(log_anult);       //  Slope for Nulato Aerial
   agisa = exp(log_agisa);       //  Slope for Gisasa Aerial  
   aanvk = exp(log_aanvk);       //  Slope for Anvik Aerial 
   aandr = exp(log_aandr);       //  Slope for Andreafsky Aerial
   wtoz = exp(log_wtoz);         //  Slope for Tozitna Weir
   atoz = exp(log_atoz);         //  Slope for Tozitna Aerial
   qplt = exp(log_qplt);         //  Survey q Pilot Sonar 
   nultp = exp(log_nultp);       //  Proportion of Nulato North   
   andrep = exp(log_andrep);

  // US escapement parameters dispersion 
   rtower = exp(log_rtower);     //  Dispersion for Chena tower
   raerial = exp(log_raerial);   //  Dispersion for Chena Aerial
   rspilt = exp(log_rspilt);     //  Dispersion for Pilot Sonar 

  // Harvests 	
//  for(i in 1:n_year) {
//    h_can[i] = h_stock_c[6,i];        	                                                         // Canada harvest  
//    h_up[1,i] = h_stock_c[5,i]+h_stock_c[4,i]+h_stock_c[3,i]+0.2*h_stock_c[2,i];                 // Above Pilot Canada stock harvest
//    h_up[2,i] = h_stock_m[6,i]+h_stock_m[5,i]+h_stock_m[4,i]+h_stock_m[3,i]+0.2*h_stock_m[2,i];
//    h_up[3,i] = h_stock_l[5,i]+h_stock_l[4,i]+h_stock_l[3,i]+0.2*h_stock_l[2,i];  
//    h_low[1,i] = h_stock_c[1,i]+0.8*h_stock_c[2,i];                                              //Below Pilot Canada stock harvest
//    h_low[2,i] = h_stock_m[1,i]+0.8*h_stock_m[2,i];                                              //Below Pilot Middle stock harvest
//    h_low[3,i] = h_stock_l[1,i]+0.8*h_stock_l[2,i];                                              //Below Pilot Lower stock harvest

  // Harvest Stock Proportion  
//    ph_up[1,i] = h_up[1,i]/sum(h_up[,i]);      // Proportion of Canada stock harvest: Upper
//    ph_up[2,i] = h_up[2,i]/sum(h_up[,i]);      // Proportion of Middle stock harvest: Upper
//    ph_up[3,i] = h_up[3,i]/sum(h_up[,i]);      // Proportion of Lower stock harvest: Upper
//    ph_low[1,i] = h_low[1,i]/sum(h_low[,i]);   // Proportion of Canada stock harvest: Lower
//    ph_low[2,i] = h_low[2,i]/sum(h_low[,i]);   // Proportion of Middle stock harvest: Lower
//    ph_low[3,i] = h_low[3,i]/sum(h_low[,i]);   // Proportion of Lower stock harvest: Lower
//  } // next i - year
  
 // Harvest passage and escapement 
  for(i in 1:n_year) {
    
    // Below Pilot harvest by stock 
    eh_low[1,i] = (1-exp(-log_fcl[i]))*can_run[i];  // Below Pilot Canada stock harvest
    eh_low[2,i] = (1-exp(-log_fml[i]))*mid_run[i];  // Below Pilot Middle stock harvest
    eh_low[3,i] = (1-exp(-log_fll[i]))*low_run[i];  // Below Pilot Lower stock harvest 
 
    // Below Pilot Harvest proportion by stock 
    eph_low[1,i] = eh_low[1,i]/sum(eh_low[,i]);   //elem_div(eh_low(1),colsum(eh_low));
    eph_low[2,i] = eh_low[2,i]/sum(eh_low[,i]);
    eph_low[3,i] = eh_low[3,i]/sum(eh_low[,i]);
    
    // Andreafsky escapement is a fraction of lower stock total run size 
    andre_e[i] = (low_run[i]- eh_low[3,i])/wandr; 
  
    // Pilot run by stock is a run minus below pilot harvest, and  Andreafsky Escapement   
    plt_et[1,i] = can_run[i] - eh_low[1,i];    //Canada stock run at Pilot
    plt_et[2,i] = mid_run[i] - eh_low[2,i];    //Middle stock run at Pilot
    plt_et[3,i] = low_run[i] - eh_low[3,i] - andre_e[i];        //Total Lower stock run at Pilot
    // Pilot station total run;  
    plt_t[i] = sum(plt_et[,i]);    
    
    // Pilot station stock proportion  
    plt_et_p[1,i] = plt_et[1,i]/plt_t[i];        //Est Pilot stock proportion
    plt_et_p[2,i] = plt_et[2,i]/plt_t[i];        //Est Pilot stock proportion
    plt_et_p[3,i] = plt_et[3,i]/plt_t[i];        //Est Pilot stock proportion
  
    // Above Pilot Harvest by stock   
    eh_up[1,i] = (1-exp(-log_fcu[i]))*plt_et[1,i];   // Above Pilot Canada stock harvest
    eh_up[2,i] = (1-exp(-log_fmu[i]))*plt_et[2,i];   // Above Pilot Middle stock harvest
    eh_up[3,i] = (1-exp(-log_flu[i]))*plt_et[3,i];   // Above Pilot Lower stock harvest 

    // Above Pilot Harvest proportoin by stock 
    eph_up[1,i] = eh_up[1,i]/sum(eh_up[,i]);
    eph_up[2,i] = eh_up[2,i]/sum(eh_up[,i]);
    eph_up[3,i] = eh_up[3,i]/sum(eh_up[,i]);
    	
    // Escacapement = Pilot run minus upper Pilot harvest 
    canpass[i] = plt_et[1,i] - eh_up[1,i];     //Total Canada border Escapement 
    midesc[i] = plt_et[2,i] - eh_up[2,i];      //Total Middle Escapement
    lowesc_u[i] = plt_et[3,i] - eh_up[3,i];    //Total Lower upriver Escapement
    lowesc[i] = lowesc_u[i] + andre_e[i];      // Total lower stock harvest is Andreafsky + above pilot

    // Canada stock escapement is Canada passage minus harvest in Canada  
    eh_can[i] = canpass[i]*(1-exp(-log_fcc[i]));
    canesc[i] = canpass[i] - eh_can[i];

    // Outing parameters 
	  t_run[i] = can_run[i] + mid_run[i] + low_run[i];  // Total run size 
	  t_esc[i] = canesc[i] + midesc[i] + lowesc[i];     // Total escapement   
  }// next i - year
  
  // Offsets 
  offset[1] = 0.0;
  offset[2] = 0.0;
  for(i in 1:n_year) {
    for(j in 1:3){
      offset[1] += ph_up[j,i]*log(ph_up[j,i] + 1e-3);
      offset[2] += ph_low[j,i]*log(ph_low[j,i] + 1e-3);
    } // next j
  } // next i

  // Calculations SRA Transformed Parameter  ============================
  // Maturity schedule: use a common maturation schedule to draw the brood year specific schedules
  pi[1] = prob[1];
  pi[2] = prob[2] * (1 - pi[1]);
  pi[3] = prob[3] * (1 - pi[1] - pi[2]);
  pi[4] = 1 - pi[1] - pi[2] - pi[3];
  D_sum = 1/D_scale^2;

  for (a in 1:A) {
    Dir_alpha[a] = D_sum * pi[a];
    for (y in 1:nRyrs) {
      p[y,a] = g[y,a]/sum(g[y,1:A]);
    }
  }

  R = exp(lnR);

  // Calculate the numbers at age matrix as brood year recruits at age (proportion that matured that year)
  for (t in 1:n_year) {
    for(a in 1:A){
      N_ta[t,a] = R[t+A-a] * p[t+A-a,a];
    }
  }

  // Calculate returns, spawners and catch by return year
  for(t in 1:n_year) {
    lnS[t] = log(canesc[t]); 
   }

  // Calculate age proportions by return year
  for (t in 1:n_year) {
    for(a in 1:A){
      q[t,a] = N_ta[t,a]/sum(N_ta[t,1:A]);
    }
  }

  // Ricker SR with AR1 process on log recruitment residuals for years with brood year spawners
  for (i in 1:nRyrs) {
    lnresid[i] = 0.0;
    lnRm_1[i] = 0.0;
    lnRm_2[i] = 0.0;
  }

  for (y in (A+a_min):nRyrs) {
    lnRm_1[y] = lnS[y-a_max] + lnalpha - beta * canesc[y-a_max]; 
    lnresid[y] = lnR[y] - lnRm_1[y];
  }

  lnRm_2[A+a_min] =  lnRm_1[A+a_min] + phi * lnresid_0;

  for (y in (A+a_min+1):nRyrs) {
    lnRm_2[y] =  lnRm_1[y] + phi * lnresid[y-1];
  }
  
  // Observed age proportions by return year
  for(y in 1:n_year){
    for(a in 1:A){
      o_run[y,a] = (H_comps_bp[y,a]*eh_low[1, y]) + (H_comps_ap[y,a]*eh_up[1, y]) + (S_comps[y,a]*canpass[y]); // run by age
    } // next a
    o_run_comp[y,] = o_run[y,]/(sum(o_run[y,])+1e-3);// run comps by age
  } // next y
}

model {
  // Priors
  
  // Run sizes
  log_can_run ~ uniform(9.0, 13.0); // log Cananda stock Run
  log_mid_run ~ uniform(9.0, 13.0); // log Mid stock Run
  log_low_run ~ uniform(9.0, 13.0); // log Low stock Run
  
  // Slope parameters
  log_atinc ~ uniform(0.0,6.0);   // log transformed slope for Tincap Aerial
  log_ftatc ~ uniform(0.0,6.0);   // log transformed slope for Tatchun Creek foot survey
  log_alsal ~ uniform(0.0,6.0);   // log transformed slope for Little Salmon Aerial
  log_anstl ~ uniform(0.0,6.0);   // log transformed slope for Nisutlin Aerial
  log_aross ~ uniform(0.0,6.0);   // log transformed slope for Ross Aerial
  log_awolf ~ uniform(0.0,6.0);   // log transformed slope for Wolf Aerial
  log_wblnd ~ uniform(0.0,6.0);   // log transformed slope for Blind Creek Weir    
  log_dwhte ~ uniform(0.0,6.0);   // log transformed slope for Whitehorse Fishway passage
  log_sbsal ~ uniform(0.0,6.0);   // log transformed slope for Big Salmon Sonar
  log_absal ~ uniform(0.0,6.0);   // log transformed slope for Big Salmon Aerial
  log_atakh ~ uniform(0.0,6.0);   // log transformed slope for Takhini Aerial
  log_qfwmr ~ uniform(-5.0,0.0);  // survey q Border Mark Recapture
  
  // Canada escapement parameters dispersion   
  log_rcanair ~ uniform(-5.0,5.0);  // log transformed slope for Tincap Aerial
  log_rcang ~ uniform(-5.0,5.0);    // log transformed slope for Blind Creek Weir    
  log_rfwmr ~ uniform(-15.0,2.0);   // log transformed slope for Border Mark Recapture
  
  // US escapement parameters slope 
  log_tchen ~ uniform(0.0,6.0);       // log transformed slope for Chena tower
  log_tsalc ~ uniform(0.0,6.0);       // log transformed slope for Salcha tower
  log_whenw ~ uniform(0.0,6.0);       // log transformed slope for Henshaw Weir
  log_achen ~ uniform(0.0,6.0);       // log transformed slope for Chena Aerial
  log_asalc ~ uniform(0.0,6.0);       // log transformed slope for Salcha Aerial
  log_wgisa ~ uniform(0.0,6.0);       // log transformed slope for Gisasa Weir
  log_aanvk ~ uniform(0.0,6.0);       // log transformed slope for Anvik Aerial
  log_tnult ~ uniform(0.0,6.0);       // log transformed slope for Nulato Tower
  log_anult ~ uniform(0.0,6.0);       // log transformed slope for Nulato Aerial
  log_agisa ~ uniform(0.0,6.0);       // log transformed slope for Gisasa Aerial
  log_wandr ~ uniform(0.0,6.0);       // log transformed slope for Andreafsky Weir
  log_aandr ~ uniform(0.0,6.0);       // log transformed slope for Andreafsky Aerial
  log_wtoz ~ uniform(0.0,6.0);        // log transformed slope for Tozitna Weir
  log_atoz ~ uniform(0.0,6.0);        // log transformed slope for Tozitna Aerial
  
  log_nultp ~ uniform(-5.0,0.0);      // proportion of Nulato North
  log_andrep ~ uniform(-5.0,0.0);     // proporiotn of Andreafsky West
  log_qplt ~ uniform(-10.0,1.0);      // Survey q Pilot Sonar

  // US Escapement parameters dispersion 
  log_rtower ~ uniform(-5.0,1.0);       // log transformed dispersion for Chena tower
  log_raerial ~ uniform(-10.0,1.0);     // log transformed dispersion for Chena Aerial
  log_rspilt ~ uniform(-10.0,10.0);     // log transformed dispersion for Pilot Sonar

  // Fishing mortality by stock   
  log_fcu ~ normal(0.0,2.0);    // Fishing mortality upriver: Canada stock
  log_fmu ~ normal(0.0,2.0);    // Fishing mortality upriver: Middle stock
  log_flu ~ normal(0.0,2.0);    // Fishing mortality upriver: Lower stock
  log_fcl ~ normal(0.0,2.0);    // Fishing mortality lower river: Canada stock
  log_fml ~ normal(0.0,2.0);    // Fishing mortality lower river: Middle stock
  log_fll ~ normal(0.0,2.0);    // Fishing mortality lower river: Lower stock
  log_fcc ~ normal(0.0,2.0);    // Fishing mortality lower river: Lower stock

  // SRA priors
  lnalpha ~ normal(0,3);
  beta ~ normal(0,1);
  sigma_R ~ normal(0,2);
  lnresid_0 ~ normal(0,20);
  mean_ln_R0 ~ normal(0,20);
  sigma_R0 ~ inv_gamma(2,1); // made this an informative prior based on metanalysis of other AK chinook stocks (Fleischman et al. 2013), less informative priors resulted in divergent tranistions
  prob[1] ~ beta(1,1);
  prob[2] ~ beta(1,1);
  prob[3] ~ beta(1,1);
  D_scale ~ beta(1,1);
  sigma_can_run ~ normal(0,20);
  
  // RR Likelihoods
  
  for(i in 1:n_year) {
  // Lower River stock Andreafsky Escapement 

    //Andreafsky Weir East: Andreafsky River Esc x fraction to East fork    
    if (w_andr[i] >0) {
       log(w_andr[i]) ~ normal(log(andrep*andre_e[i]), sqrt(log(square(w_andr_cv[i])+1)+square(rtower)));
    } 
    //Andreafsky Aerial East: Fraction of East fork escapeent
    if (a_andr_e[i] >0) {
      log(a_andr_e[i]) ~ normal(log(andrep*andre_e[i]/aandr), sqrt(log(square(a_andr_e_cv[i])+1)+square(raerial)));
    }
    //Andreafsky Aerial West: Fraction of West fork escapement
    if (a_andr_w[i] >0) {
      log(a_andr_w[i]) ~ normal(log((1-andrep)*andre_e[i]/aandr), sqrt(log(square(a_andr_w_cv[i])+1)+square(raerial)));
    }

  // Pilot Sonar Passage

    //Pilot Station
    if (s_pilot[i] >0) {
      // Fit to experimental years
	    if (i < nyp[1]) {
        log(s_pilot[i]) ~ normal(log(qplt[1]*plt_t[i]), sqrt(log(1+square(s_pilot_cv[i]))+square(rspilt[1]))); 
	    }else {
      // Pilot Staion in production
        if (i < nyp[2]) {
          log(s_pilot[i]) ~ normal(log(qplt[2]*plt_t[i]), sqrt(log(square(s_pilot_cv[i])+1)+square(rspilt[2]))); 
        }else {
      // Pilot Station with DIDSON
          log(s_pilot[i]) ~ normal(log(plt_t[i]), sqrt(log(square(s_pilot_cv[i])+1))); 
        } 
	    }
    }
    
    // RTMR Passage at Pilot Station
    if (t_rtmr[i] >0) {
      // Fit to 2000-2001
      if(i < nyp[2]){
      log(t_rtmr[i]) ~ normal(log(plt_t[i]), sqrt(log(square(t_rtmr_cv[i])+1)+square(rspilt[3])));
      }else {
        log(t_rtmr[i]) ~ normal(log(plt_t[i]), sqrt(log(square(t_rtmr_cv[i])+1)+square(rspilt[4])));
      }
    }

    // Stock proportion at Pilot Station
    if(sum(pilot_p[,i])>0) {
      target += wrunp[i]*(sum(pilot_p[,i].*log(plt_et_p[,i]))-sum(pilot_p[,i].*log(pilot_p[,i]))); 
	  }

    // US Middle Stock escapement Likelihood
    //Chena Tower
    if (t_chen[i] >0) {
		    log(t_chen[i]) ~ normal(log(midesc[i]/tchen), sqrt(log(square(t_chen_cv[i])+1)+square(rtower)));
      }
    //Salcha Tower
    if  (t_salc[i] >0) {
		    log(t_salc[i]) ~ normal(log(midesc[i]/tsalc), sqrt(log(square(t_salc_cv[i])+1)+square(rtower)));
    }
    //Henshaw Creek Weir
    if (w_henw[i] >0) {
        log(w_henw[i]) ~ normal(log(midesc[i]/whenw), sqrt(log(square(w_henw_cv[i])+1)+square(rtower)));
    }
    //Chena Aerial
    if (a_chen[i] >0) {
         log(a_chen[i]) ~ normal(log(midesc[i]/achen/tchen), sqrt(log(square(a_chen_cv[i])+1)+square(raerial)));
      }
    //Salcha Aerial
    if (a_salc[i] >0) {
         log(a_salc[i]) ~ normal(log(midesc[i]/asalc/tsalc), sqrt(log(square(a_salc_cv[i])+1)+square(raerial)));
	    }
 
    // US Lower Stock Escapement Likelihood 
    //Nulato Tower
    if (t_nult[i] >0) {
      log(t_nult[i]) ~ normal(log(lowesc_u[i]/tnult), sqrt(log(square(t_nult_cv[i])+1)+square(rtower)));
    }
    //Gisasa Weir
    if (w_gisa[i] >0) {
      log(w_gisa[i]) ~ normal(log(lowesc_u[i]/wgisa), sqrt(log(square(w_gisa_cv[i])+1)+square(rtower)));
    }
    //Nulato North Aerial
    if (a_nult_n[i] >0) {
      log(a_nult_n[i]) ~ normal(log(nultp*lowesc_u[i]/anult/tnult), sqrt(log(square(a_nult_n_cv[i])+1)+square(raerial)));
    }
    //Nulato South Aerial
    if (a_nult_s[i] >0) {
      log(a_nult_s[i]) ~ normal(log((1-nultp)*lowesc_u[i]/anult/tnult), sqrt(log(square(a_nult_s_cv[i])+1)+square(raerial)));
    }
    //Gisasa Aerial
    if (a_gisa[i] >0) {
      log(a_gisa[i]) ~ normal(log(lowesc_u[i]/agisa/wgisa), sqrt(log(square(a_gisa_cv[i])+1)+square(raerial)));
    }
    //Anvik Aerial
    if (a_anvk[i] >0) {
	    log(a_anvk[i]) ~ normal(log((lowesc_u[i])/aanvk), sqrt(log(square(a_anvk_cv[i])+1)+square(raerial)));
    }
    //Tozitna Weir
    if (w_toz[i] >0) {
      log(w_toz[i]) ~ normal(log(lowesc_u[i]/wtoz), sqrt(log(square(w_toz_cv[i])+1)+square(rtower)));
    }
    //Tozitna Aerial
    if (a_toz[i] >0) {
      log(a_toz[i]) ~ normal(log(lowesc_u[i]/atoz/wtoz), sqrt(log(square(a_toz_cv[i])+1)+square(raerial)));
    }


    // Canada Border Passage 
    // Eagle Sonar 
    if (s_eagle[i] >0) {
      log(s_eagle[i]) ~ normal(log(canpass[i]), sqrt(log(square(s_eagle_cv[i])+1)));
    }
    // Radio Telemetry Mark Recapture
    if (r_mr[i] >0) {
      log(r_mr[i]) ~ normal(log(canpass[i]), sqrt(log(square(r_mr_cv[i])+1)));
    }
    //Fishwheel Mark Recapture
    if (f_mr[i] >0) {
      if (i < nye[1]){
        log(f_mr[i]) ~ normal(log(qfwmr*canpass[i]), sqrt(log(square(f_mr_cv[i])+1)+square(rfwmr[1])));
        }else {
          if (i < nye[2]){
            log(f_mr[i]) ~ normal(log(qfwmr*canpass[i]), sqrt(log(square(f_mr_cv[i])+1)+square(rfwmr[2])));
            }else {
              log(f_mr[i]) ~ normal(log(qfwmr*canpass[i]), sqrt(log(square(f_mr_cv[i])+1)+square(rfwmr[1])));
            }
        }
    }

    // Canada escapement Likelihood
    //Tincap Aerial
    if (a_tinc[i] >0) {
       log(a_tinc[i]) ~ normal(log(canesc[i]/atinc), sqrt(log(square(a_tinc_cv[i])+1)+square(rcanair)));
    }
    //Tincap Foot survey
    if (f_tatc[i] >0) {
       log(f_tatc[i]) ~ normal(log(canesc[i]/ftatc), sqrt(log(square(f_tatc_cv[i])+1)+square(rcanair)));
    }
    //Little Salmon Aerial
    if (a_lsal[i] >0) {
      log(a_lsal[i]) ~ normal(log(canesc[i]/alsal), sqrt(log(square(a_lsal_cv[i])+1)+square(rcanair)));
    }
    //Nisutlin Aerial
    if (a_nstl[i] > 0) {
      log(a_nstl[i]) ~ normal(log(canesc[i]/anstl), sqrt(log(square(a_nstl_cv[i])+1)+square(rcanair)));
    }
    //Ross Aerial
    if  (a_ross[i] >0) {
      log(a_ross[i]) ~ normal(log(canesc[i]/aross), sqrt(log(square(a_ross_cv[i])+1)+square(rcanair)));
    }
    //Wolf Aerial
    if (a_wolf[i] >0) {
      log(a_wolf[i]) ~ normal(log(canesc[i]/awolf), sqrt(log(square(a_wolf_cv[i])+1)+square(rcanair)));
    }
    //Blind Creek Weir
    if (w_blnd[i] >0) {
      log(w_blnd[i]) ~ normal(log(canesc[i]/wblnd), sqrt(log(square(w_blnd_cv[i])+1)+square(rcang)));
    }
    //Whitehorse Dam Fishway
    if (d_whte[i] >0) {
      log(d_whte[i]) ~ normal(log(canesc[i]/dwhte), sqrt(log(square(d_whte_cv[i])+1)+square(rcanair)));
    }
    //Big Salmon Sonar
    if (s_bsal[i] >0) {
      log(s_bsal[i]) ~ normal(log(canesc[i]/sbsal), sqrt(log(square(s_bsal_cv[i])+1)+square(rcang)));
    }
    //Big Salmon Aerial
    if (a_bsal[i] > 0) {
      log(a_bsal[i]) ~ normal(log(canesc[i]/sbsal/absal), sqrt(log(square(a_bsal_cv[i])+1)+square(rcanair)));
    }
    //Takhini Aerial
    if (a_takh[i] >0) {
      log(a_takh[i]) ~ normal(log(canesc[i]/atakh), sqrt(log(square(a_takh_cv[i])+1)+square(rcanair)));
    }
  } // next i - year

  // Likelihood Harvest
  for(i in 1:n_year) {
    // Canada harvest - SSQ
    target += normal_lpdf(log(h_can[i]) | log(eh_can[i]), sqrt(log(square(hp_cv[i])+1))); 
    
    // Total harvests upriver - SSQ
    target += normal_lpdf(log(h_up[i]) | log(sum(eh_up[,i])), sqrt(log(square(hp_cv[i])+1)));  
    // Total harvests Lower river - SSQ
    target += normal_lpdf(log(h_low[i]) | log(sum(eh_low[,i])), sqrt(log(square(hp_cv[i])+1)));  

    // Harvest stock proportion upriver
     target += p_n_up[i]*(sum(ph_up[,i].*log(eph_up[,i]+1e-3)) - offset[1]);  
    
    // Harvest stock proportion downriver
    target += p_n_low[i]*(sum(ph_low[,i].*log(eph_low[,i]+1e-3)) - offset[2]);  

  }// next i - year
  
  // SRA likelihood
  // Gamma variates for each year and age class which are used to determine age at maturity proportions
  for (y in 1:nRyrs) {
    for (a in 1:A) {
      //g[y,a] ~ gamma(Dir_alpha[a],1);
      target += gamma_lpdf(g[y,a]|Dir_alpha[a],1);
    }
  }

  // First `a.max` years of recruits, for which there is no spawner link
  lnR[1:a_max] ~ normal(mean_ln_R0, sigma_R0);

  // State model
  lnR[(A+a_min):nRyrs] ~ normal(lnRm_2[(A+a_min):nRyrs], sigma_R);

  // Observation model
  for(t in 1:n_year){
    if(sum(S_comps[t])>0){
    target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // time varying ESS for age comp likelihood for ONLY years with data
    }
    target += normal_lpdf(log(can_run[t]) | log(sum(N_ta[t,1:A])), sigma_can_run);
  }

}


generated quantities {
  real<lower=0> S_max;      // Spawner abundance that produces maximum recruitment
  real<lower=0> S_eq;       // Equilibrium spawner abundance
  vector[nRyrs] lnalpha_y;  // Time trend in intrinsic productivity
  real<lower=0> lnalpha_c;  // Log-normal bias corrected log alpha
  real<lower=0> S_eq_c;     // Log-normal bias corrected equilibrium spawner abundance
  real Smsy_Sch;            // Spawner abundance that produces maximum yield (Scheurell 2016 solution)
  real MSY_Sch;             // Maximum yield (Scheurell 2016 solution)
  real Umsy_Sch;            // Harvest rate associated with maximum yield (Scheurell 2016 solution)
  real z;
  real w;
  real Smsy_Sch_corr;
  real Umsy_Sch_corr;
  real z_corr;
  real w_corr;

  S_max = 1/beta;
  S_eq = lnalpha * S_max;
  lnalpha_y = lnalpha + lnresid;
  lnalpha_c = lnalpha + (sigma_R * sigma_R)/2/(1-phi * phi);
  S_eq_c = lnalpha_c * S_max;

  // Scheuerell (2016) Smsy and Umsy Approximation: UNCORRECTED
  // Get initial w0 and z
  z = exp(1.0-lnalpha);
  w = (3.0/4.0)*log(z+1);
    
  // Iterations
  Smsy_Sch = (1-w)/beta;
  for(i in 2:5) {
    w = w - ((w*exp(w)-z)/
                         (exp(w)*(w+1)-(((w+2)*(w*
                            exp(w)-z))/(2*w+2))));
    Smsy_Sch = (1-w)/beta;
    Umsy_Sch = 1-w;
  } //next i

  // Calculate MSY
  MSY_Sch = Smsy_Sch*exp(lnalpha - Smsy_Sch*beta) - Smsy_Sch;
  
  // Scheurell (2016) Smsy Approximation: CORRECTED
  // Get initial w0 and z
  z_corr = exp(1.0-lnalpha_c);
  w_corr = (3.0/4.0)*log(z_corr+1);
    
  // Iterations
  Smsy_Sch_corr = (1-w_corr)/beta;
  Umsy_Sch_corr = 1-w_corr;
  for(i in 2:5) {
    w_corr = w_corr - ((w_corr*exp(w_corr)-z_corr)/
                         (exp(w_corr)*(w_corr+1)-(((w_corr+2)*(w_corr*
                            exp(w_corr)-z_corr))/(2*w_corr+2))));
    Smsy_Sch_corr = (1-w_corr)/beta;
    Umsy_Sch_corr = 1-w_corr;
  } //next i

}

