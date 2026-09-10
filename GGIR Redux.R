library(GGIR)
library(GGIRread)
library(lubridate)
library(parallel)

raw_data_fp <- "/Users/levkolinski/Library/CloudStorage/Box-Box/NSF Data/Survey Data/Lev Kolinski Data/Axivity/Raw Ax3 Exports/Late Return"  
output_data_fp <- "/Users/levkolinski/Library/CloudStorage/Box-Box/NSF Data/Survey Data/Lev Kolinski Data/Axivity/GGIR Outputs/GGIR Outputs Aug 2026"


nCoresToUse <-2

list.files(raw_data_fp)

GGIR(
  # computational parameters
  do.parallel = T,
  maxNcores = nCoresToUse,
  verbose = T,
  
  
  # --- run control ---
  mode      = c(1,2,3,4,5,6),                 #
  do.report = c(1,2,3,4,5,6),
  datadir   =raw_data_fp,
  outputdir = output_data_fp,
  studyname = "malagasy",                           
  # --- params_247 ---
  qwindow = c(0,24), qlevels = c(), qwindow_dateformat = "%d-%m-%Y",
  ilevels = c(),
  IVIS_windowsize_minutes = 60, IVIS_epochsize_seconds = c(),
  IVIS.activity.metric = 2, IVIS_acc_threshold = 20,
  qM5L5 = c(), MX.ig.min.dur = 10, M5L5res = 10, winhr = 5,
  iglevels = seq(0, 4000, by = 25),   
  LUXthresholds = c(0,100,500,1000,3000,5000,10000),
  LUX_cal_constant = c(), LUX_cal_exponent = c(), LUX_day_segments = c(),
  L5M5window = c(0,24),
  cosinor = FALSE, part6CR = FALSE, part6HCA = FALSE,
  part6Window = c("start","end"), part6DFA = FALSE,
  clevels = c(30,150), SRI2_WASOmin = 30,
  
  # --- params_cleaning ---
  includedaycrit = 16, ndayswindow = 7, strategy = 1,
  data_masking_strategy = 1, maxdur = 9,
  hrs.del.start = 0, hrs.del.end = 0,
  includedaycrit.part5 = 0.666666667, excludefirstlast.part5 = FALSE,
  TimeSegments2ZeroFile = c(), do.imp = TRUE, data_cleaning_file = c(),
  minimum_MM_length.part5 = 23, excludefirstlast = FALSE,
  includenightcrit = 16, excludefirst.part4 = FALSE, excludelast.part4 = FALSE,
  max_calendar_days = 0, nonWearEdgeCorrection = TRUE, nonwear_approach = 2023,
  segmentWEARcrit.part5 = 0.5, segmentDAYSPTcrit.part5 = c(0.9,0),
  study_dates_file = c(),                     
  study_dates_dateformat = "%d-%m-%Y",
  includecrit.part6 = c(2/3, 2/3), includenightcrit.part5 = 0,
  nonwearFiltermaxHours = c(), nonwearFilterWindow = c(),
  
  # --- params_general ---
  overwrite = FALSE, acc.metric = "ENMO", 
  print.filename = FALSE, 
  windowsizes = c(5,900,3600),
  desiredtz = "Africa/Nairobi",         
  configtz = c(), idloc = 1, dayborder = 0,
  part5_agg2_60seconds = FALSE, sensor.location = "wrist",
  expand_tail_max_hours = c(), recordingEndSleepHour = c(), dataFormat = "raw",
  maxRecordingInterval = c(), extEpochData_timeformat = "%d-%m-%Y %H:%M:%S",
  recording_split_times = c(), recording_split_timeformat = "%d/%m/%Y %H:%M",
  recording_split_overlap = 0, recording_split_ignore_edges = FALSE,
  
  # --- params_metrics ---
  do.anglex = FALSE, do.angley = FALSE, do.anglez = TRUE,
  do.zcx = FALSE, do.zcy = FALSE, do.zcz = FALSE,
  do.enmo = TRUE, do.lfenmo = FALSE, do.en = FALSE, do.mad = FALSE, do.enmoa = FALSE,
  do.roll_med_acc_x = FALSE, do.roll_med_acc_y = FALSE, do.roll_med_acc_z = FALSE,
  do.dev_roll_med_acc_x = FALSE, do.dev_roll_med_acc_y = FALSE, do.dev_roll_med_acc_z = FALSE,
  do.bfen = FALSE, do.hfen = FALSE, do.hfenplus = FALSE, do.lfen = FALSE,
  do.lfx = FALSE, do.lfy = FALSE, do.lfz = FALSE,
  do.hfx = FALSE, do.hfy = FALSE, do.hfz = FALSE,
  do.bfx = FALSE, do.bfy = FALSE, do.bfz = FALSE,
  do.brondcounts = FALSE, do.neishabouricounts = FALSE,
  hb = 15, lb = 0.2, n = 4,
  zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01, zc.order = 2, zc.scale = 1,
  actilife_LFE = FALSE,
  
  # --- params_output ---
  epochvalues2csv = FALSE, save_ms5rawlevels = TRUE, save_ms5raw_format = "RData",
  save_ms5raw_without_invalid = FALSE, storefolderstructure = FALSE,
  timewindow = c("WW","MM"), viewingwindow = 1, dofirstpage = TRUE,
  visualreport = FALSE, week_weekend_aggregate.part5 = FALSE,
  do.part3.pdf = FALSE, outliers.only = FALSE, criterror = 3,
  do.visual = TRUE, do.sibreport = TRUE, do.part2.pdf = TRUE,
  sep_reports = ",", sep_config = ",", dec_reports = ".", dec_config = ".",
  visualreport_without_invalid = TRUE, old_visualreport = TRUE,
  visualreport_hrsPerRow = 36, visualreport_focus = "day",
  visualreport_validcrit = 0, require_complete_lastnight_part5 = FALSE,
  method_research_vars = c(),
  
  # --- params_phyact ---
  mvpathreshold = 100, boutcriter = 0.8, mvpadur = c(1,5,10),
  boutcriter.in = 0.9, boutcriter.lig = 0.8, boutcriter.mvpa = 0.8,
  threshold.lig = 30, threshold.mod = 100, threshold.vig = 400,
  boutdur.mvpa = 1, boutdur.in = c(1,10,30), boutdur.lig = c(1,10),
  frag.metrics = c(), part6_threshold_combi = "30_100_400",
  
  # --- params_rawdata ---
  chunksize = 1, spherecrit = 0.3, minloadcrit = 168, printsummary = FALSE,
  do.cal = TRUE, backup.cal.coef = "retrieve", dynrange = c(),
  minimumFileSizeMB = 2, rmc.dec = ".", rmc.unit.acc = "g", rmc.unit.temp = "C",
  rmc.unit.time = "POSIX", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
  rmc.unsignedbit = TRUE, rmc.origin = "1970-01-01",
  rmc.check4timegaps = FALSE, rmc.noise = 13, nonwear_range_threshold = 150,
  rmc.doresample = FALSE, interpolationType = 1, imputeTimegaps = TRUE,
  frequency_tol = 0.1, rmc.scalefactor.acc = 1,
  
  # --- params_sleep ---
  anglethreshold = 5, timethreshold = 5, ignorenonwear = TRUE,
  HASPT.algo = "HDCZA", HASIB.algo = "vanHees2015",
  longitudinal_axis = c(), HASPT.ignore.invalid = FALSE,
  loglocation = c(), colid = 1, coln1 = 2, nnights = c(),
  relyonguider = FALSE, def.noc.sleep = 1, sleeplogsep = c(),
  sleepwindowType = "SPT", possible_nap_window = c(), possible_nap_dur = c(),
  possible_nap_gap = 0, possible_nap_edge_acc = Inf, nap_model = c(),
  sleepefficiency.metric = 1, HDCZA_threshold = c(), oakley_threshold = 20,
  consider_marker_button = FALSE, impute_marker_button = FALSE,
  sib_must_fully_overlap_with_TimeInBed = c(TRUE,TRUE), nap_markerbutton_method = 0
)

