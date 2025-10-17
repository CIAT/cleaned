# =============================================================================
# Title: Batch enteric emissions calculator for Venture37 farm surveys (Kenya)
# Purpose:
#   - Automate CLEANED-based calculations of enteric methane and related
#     environmental indicators for many farms at once (230+), using survey data
#     provided by Venture37 across multiple Excel sheets.
#   - Track how emissions change from baseline following adoption of improved
#     feed baskets (scenario comparison).
#
# Background:
#   CLEANED (Comprehensive Livestock Environmental Assessment for improved
#   Nutrition, a secured Environment and sustainable Development) quantifies
#   the environmental footprint of livestock systems—including land/water
#   needs, soil impacts (erosion, N balance), carbon/biomass, and GHG emissions.
#   It is designed for rapid, ex-ante assessment with minimal inputs and has an
#   R package plus a desktop interface; this script leverages the R pathway for
#   scalable, repeatable analysis.  (See CLEANED how-it-works brief and input
#   object structure references.)
#
# Inputs:
#   - Excel workbooks from Venture37 (baseline and follow-up), with sheets:
#       * "milk-bodyweight" (herd data)
#       * "feed_items" and "feed_type" (feed catalogue & metadata)
#       * "Feedproportions" (feed basket per herd type)
#       * "simple_fields" (site-level single-value fields)
#   - CLEANED package parameter files (energy & GHG/IPCC parameters).
#   - Lookups from CIAT/icleaned (livestock types, manure management).
#   - Optional feed composition backfills from ERA and ILRI feed DBs (S3).
#
# Outputs (per input Excel file):
#   - <input file name> emissions.xlsx: multi-sheet workbook with merged
#     results (one sheet per CLEANED output table).
#   - <input file name> emissions.json: JSON with the same content.
#   - Returned R object (ef table) with per-herd emission factors plus
#     farm/herd identifiers and source file tag.
#
# Scope & Notes:
#   - Focus is enteric methane but the pipeline computes other CLEANED outputs
#     (energy, land, soil, nitrogen balance, biomass, productivity) needed to
#     parameterize GHG calculations.
#   - This automation generalizes the workflow and positions it for possible
#     ingestion from common survey tools (e.g., KoboToolbox) later.
#   - Designed for Kenya (country="Kenya"), but country-specific lookups can be
#     adapted.
#   - Run from within the cleaned R project, you will need to include the v37 data
#     in the R/data folder of the project.
#
# Funding & Credits:
#   - Script development funded under CGIAR Sustainable Animal and Aquatic
#     Foodsystem Initiative (2025).
#   - Lead developer:    Pete Steward <p.steward@cgiar.org>
#   - Co-developer:      Emmanuel Mwema <e.mwema@cgiar.org>
#
# Dependencies:
#   - R packages: readxl, cleaned, jsonlite, tidyr, dplyr, miceadds, data.table,
#                 openxlsx, s3fs, pacman
#   - Internet access for remote lookups (GitHub CSV/JSON; S3 read-only).
#
# Repro & QA:
#   - Tested with >230 farms (Venture37). Keep an eye on feed basket
#     proportions (should sum to 100 per herd & season) and missing
#     feed_type mappings (warning emitted).
#   - Script emits informative warnings for common data issues.
#
# References:
#   - CLEANED overview and scope (multi-impact, rapid assessment)   [oai_citation:0‡HowItWorksBrief_dec.pdf](file-service://file-2R3Pp9ZMcZJUix9VAX8nwf)
#   - Expected CLEANED input object structure (JSON/list, key tables)   [oai_citation:1‡explore_qt_json.pdf](file-service://file-KqxuYyfNo7bSVyZrk2Q3b2)
# =============================================================================

# 0) Load packages and source functions ####
# Pacman streamlines conditional package loading.
pacman::p_load(
  readxl, cleaned, jsonlite, tidyr, dplyr, miceadds, data.table, openxlsx, s3fs
)

# Source CLEANED functions from development branch of github
base <- "https://raw.githubusercontent.com/CIAT/cleaned/refs/heads/cleaned_dev_ps/R/"

# Order roughly by common dependencies (core first)
files <- c(
  "data.R",
  "global.R",
  "feed_quality.R",
  "energy_requirement.R",
  "land_requirement.R",
  "soil_health.R",
  "nitrogen_balance.R",
  "land_productivity.R",
  "biomass_calculation.R",
  "water_requirement.R",
  "season_length.R",
  "soc.R",
  "ghg_emission.R",
  "ghg_emission_v2.R",
  "compare_scenario.R",
  "differences.R",
  "merge_outputs.R",
  "plotting.R"
)

for (f in files) {
  u <- paste0(base, f)
  message("Sourcing: ", f, " …")
  tryCatch(
    source(u, local = TRUE, encoding = "UTF-8"),
    error = function(e) stop("Failed to source ", f, ": ", conditionMessage(e), call. = FALSE)
  )
}

  ## 0.1) Configure input files ####

  # We operate on multiple Venture37 Excel workbooks. Name them for scenario tags.
  # Set the location of the folder that contains the v37 input data
  input_files <- file.path("data/v37", c("Kenya NPA data.xlsx", "NPA_Kenya_April2025.xlsx"))
  names(input_files) <- c("baseline", "apr_2025")

  # Toggle verbose progress (TRUE prints farm/herd progress lines)
  messages <- F

# Master lapply over input files ####
  # For each workbook: ingest -> harmonize -> build CLEANED input objects per herd
  # -> run the CLEANED pipeline -> write Excel & JSON -> return EF table.

ef<-lapply(1:length(input_files),function(ii){
  cat("Running file",input_files[ii],"\n")
  # 1) Load Venture37 & parameter data ####
    ## 1.1) V37 data ####
    file<-input_files[ii]
    save_file<-gsub(".xlsx"," emissions.xlsx",file)

      ### 1.1.1) Herd ####
      herd <- data.table(readxl::read_excel(file,sheet="milk-bodyweight"))
      herd[,livetype_code:=as.character(livetype_code)]
      setnames(herd,"livetype_desc","livetype_desc_v37")

      herd[is.na(annual_milk),annual_milk:=0]

      ### 1.1.2) Feed items ####
      v37_feed_items<-data.table(readxl::read_excel(file,sheet="feed_items"))
      v37_feed_items$intercrop<-as.numeric(0)

      ### 1.1.3) Feed types ####
      v37_feed_type<-unique(data.table(readxl::read_excel(file,sheet="feed_type")))[order(feed_type_code)]

      # Deal with duplicate entry for feed_type_code == 5 (Brachiaria)
      x<-v37_feed_type[feed_type_code==5][, feed_type_name := "Brachiaria"]
      by_cols<-colnames(x)[1:5]
      num_cols<-colnames(x)[!colnames(x) %in% by_cols]

      x <- x[, lapply(.SD, mean), by = by_cols, .SDcols = num_cols]

      v37_feed_type<-rbind(v37_feed_type[feed_type_code!=5],x)

      # V37 - Merge feed_items & feed type
      v37_feed_items_merge<-merge(v37_feed_items,v37_feed_type,by="feed_type_code",all.x=T)

      # Warn on feed items without category (likely missing feed_type_code)
      check<-v37_feed_items_merge[(category==0|is.na(category)) & feed_type_name!="Purchased",.(feed_item_name,feed_item_code,feed_type_name,feed_type_code)]

      if(nrow(check)>0){
        warning("Check merge of feed items and feed type, is there a missing feed type code for:")
        print(check)
      }


      ### 1.1.4) Feed basket ####

      v37_feed_basket <- data.table(readxl::read_excel(file,sheet="Feedproportions"))

      v37_feed_basket[,livetype_code:=tolower(livetype_code)]

      # Remove duplicates
      v37_feed_basket<-unique(v37_feed_basket)

      if(is.null(v37_feed_basket$v37_livestock_type)){
        v37_feed_basket$v37_livestock_type<-"Not Included"
      }

      # Reshape wide feed proportions -> long (one row per feed_item_code allocation)
      v37_feed_basket<-melt(v37_feed_basket,
                            id.vars = c("Ids","v37_livestock_type","livetype_code","livetype_desc"),
                            variable.name = "feed_item_code",
                            value.name = "allocation")

      v37_feed_basket<-v37_feed_basket[allocation!=0][order(Ids,livetype_code)
                                                      ][,total:=sum(allocation),by=.(Ids,livetype_code,livetype_desc)]

      # Check that per-herd allocations are sensible (should sum to ~1 if proportion)
      check<-unique(v37_feed_basket[total==2,.(Ids,livetype_code,total)])
      if(nrow(check)>0){
        warning("Proportions of feed items sum to >1 for the following:")
        print(check)
      }

      # Normalize types
      v37_feed_basket<-v37_feed_basket[,.(Ids,livetype_code,livetype_desc,feed_item_code,allocation)
                                         ][,feed_item_code:=as.numeric(as.character(feed_item_code))]

      # If survey captured proportions as 0–1, scale to percentages (0–100)
      if(v37_feed_basket[,!max(allocation>10)]){
        cat("Feed allocations assumed to be proportions so multiplying by 100.\n")
        v37_feed_basket[,allocation:=allocation*100]
      }

      # Add feed type code
      v37_feed_basket<-merge(v37_feed_basket,v37_feed_items[,.(feed_item_code,feed_type_code)],by="feed_item_code",all.x=T)

      check<-v37_feed_basket[is.na(feed_type_code)]
      if(nrow(check)>0){
        warning("Feed item type missing after feed basket merge:")
        print(check)
      }

      ### 1.1.5) Simple field parameters ####
      v37_simple_fields<-data.table(readxl::read_excel(file,sheet="simple_fields"))

      ### 1.1.6) Set country ####
      country<-"Kenya"
      v37_feed_items$country<-country
    ## 1.2) Read in parameter tables #####
    lkp_livetype<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_livetype.csv")
    lkp_manureman<-fread("https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/primary_database/lkp_manureman.csv")

    energy_parameters <- fromJSON(
      system.file("extdata", "energy_parameters.json", package = "cleaned"),
      flatten = TRUE
    )

    # Read static parameters directory and files
    ghg_ipcc_data <- fromJSON(
      system.file("extdata", "ghg_parameters.json", package = "cleaned"),
      flatten = TRUE
    )

    ## 1.3) Load input template #####
    file<-"https://raw.githubusercontent.com/CIAT/icleaned/refs/heads/staging/data/shared_folder/study_objects/Study_2.json"
    template <- fromJSON(file)
    colnames(template$livestock)

    feed_items<-data.table(template$feed_items)
    fertilizer<-data.table(template$fertilizer)[0,]
    fertilizer<-NULL

    ## 1.4) ERA feed tables ####
    # List files in the specified S3 bucket and prefix

    s3<-s3fs::S3FileSystem$new(anonymous = T)

    files_s3<-suppressWarnings(s3$dir_ls("s3://digital-atlas/era/data"))
    files_s3<-files_s3[grepl("ERA_nutrition_library",files_s3)]
    files_local<-file.path("data",basename(files_s3))

      if(!file.exists(files_local)){
        s3$file_download(files_s3,files_local)
      }

    era_feeds <- data.table(readxl::read_excel(files_local,sheet=2))
    era_feeds <- era_feeds[!is.na(DC.Value) & DC.Value!="NA" &
                             DC.Variable %in% c("DM",'CP',"ME") &
                             !nutrition_source %in% c("feedipedia","ilri")]

     ### 1.5.1) (Not Run) Map ERA feeds to feed_items ####
    # Not Run - in development
    if(F){
      for(i in 1:nrow(v37_feed_items_merge)){
        era_code<-v37_feed_items_merge[i,era_code]
        is_dry<-v37_feed_items_merge[i,feed_is_dm]
        if(!is.na(era_code)){
          era_code<-unlist(strsplit(era_code,";"))
          era_feed<-era_feeds[D.Item.AOM %in% era_code]
          era_feed[,dm_check:=DC.Value[DC.Variable=="DM"],by=.(B.Code,Country,D.Item)]
        }else{

        }
      }
    }


    ## 1.5) ILRI feed tables ####

    s3<-s3fs::S3FileSystem$new(anonymous = T)

    files_s3<-suppressWarnings(s3$dir_ls("s3://digital-atlas/era/ancillary_datasets/ilri_feed_db"))
    files_s3<-files_s3[grepl("ilri_feed_db.xlsx.zip",files_s3)]
    files_local_zip<-file.path("data",basename(files_s3))
    files_local<-gsub(".zip","",files_local_zip)

    if(!file.exists(files_local)){
        s3$file_download(files_s3,files_local_zip)

        unzip(files_local_zip, exdir = dirname(files_local))

        if (file.exists(files_local)) {
          file.remove(files_local_zip)
        }
      }

    ilri_feeds <- as.data.table(read_excel(files_local, sheet = "ilri_feed_db"))

    if(F){
    # Code to explore data
    ilri_feeds[grep("panicum",ilri_feedname,ignore.case = T),
               .(N=.N,
                 DM_mean=mean(DM,na.rm=T),
                 ME_mean=mean(ME,na.rm=T),
                 CP_mean=mean(CP,na.rm=T),
                 DM_min=min(DM,na.rm = T),
                 DM_max=max(DM,na.rm=T),
                 ME_min=min(ME,na.rm = T),
                 ME_max=max(ME,na.rm = T),
                 CP_min=min(CP,na.rm = T),
                 CP_max=max(CP,na.rm=T)),
               by=.(ilri_feedname,ilri_feedcode)][order(N,decreasing = T)]
    }

     ### 1.5.1) Map ILRI feeds to feed_items ####
    v37_fdb_codes<-unique(v37_feed_items_merge[!is.na(ilri_fdb_code),.(country,ilri_fdb_code)])

    min_samples<-1

    ilri_fdb_subset<-rbindlist(lapply(1:nrow(v37_fdb_codes),function(i){
      feed_code<-unlist(strsplit(v37_fdb_codes$ilri_fdb_code[i],";"))
      country<-v37_fdb_codes$country[i]

      result1<-ilri_feeds[Country==country & ilri_feedcode %in% feed_code,
                 .(N=.N,
                  DM_ifdb=mean(DM,na.rm=T),
                  ME_ifdb=mean(ME,na.rm=T),
                  CP_ifdb=mean(CP,na.rm=T)),by=Country]

      result2<-ilri_feeds[ilri_feedcode %in% feed_code,
                         .(N=.N,
                           DM_ifdb=mean(DM,na.rm=T),
                           ME_ifdb=mean(ME,na.rm=T),
                           CP_ifdb=mean(CP,na.rm=T))][,Country:="All Data"]

      result<-rbindlist(list(result1,result2),use.names=T)

      result$ilri_fdb_code<-v37_fdb_codes$ilri_fdb_code[i]

      return(result)
    }))

    ilri_fdb_subset<-ilri_fdb_subset[N>=min_samples]
    ilri_fdb_subset<-ilri_fdb_subset[,N2:=.N,by=ilri_fdb_code
                                     ][!(N2==2 & Country=="All Data")
                                       ][,.(ilri_fdb_code,DM_ifdb,ME_ifdb,CP_ifdb)]

    v37_feed_items_merge<-merge(v37_feed_items_merge,
                                ilri_fdb_subset,
                                by="ilri_fdb_code",all.x=T)

    v37_feed_items_merge[!is.na(DM_ifdb) & feed_is_dm==T,c("dm_content"):=.(DM_ifdb)]
    v37_feed_items_merge[!is.na(CP_ifdb) & feed_is_dm==T,c("cp_content"):=.(CP_ifdb)]
    v37_feed_items_merge[!is.na(ME_ifdb) & feed_is_dm==T,c("me_content"):=.(ME_ifdb)]

    v37_feed_items_merge[,c("ilri_fdb_code","feed_is_dm","DM_ifdb","CP_ifdb","ME_ifdb"):=NULL]

    # 2) Set "fixed" parameters ####

      ## 2.0) "Simple" fields #####
      template_names<-data.table(field_name=names(template),
                             class=sapply(template,base::class),
                             length=sapply(template,length))

      simple_field_names<-template_names[length==1,field_name]

      simple_fields<-template[simple_field_names]

      for(i in 1:length(simple_fields)){
        field<-names(simple_fields)[i]
        simple_fields[field]<-v37_simple_fields[field_name==field,value]
      }


      ## 2.1) Livestock #####
      mm_code<-"storage"
      mm_des<-lkp_manureman[manureman_code==mm_code,manureman_desc]

      mm_code2<-"pasture"
      mm_des2<-lkp_manureman[manureman_code==mm_code2,manureman_desc]

      livestock_fixed<-data.frame(
        manureman_stable=mm_des,
        manureman_onfarm_grazing=mm_des2,
        manureman_non_roofed_enclosure=0,
        manureman_offfarm_grazing=mm_des2,
        annual_growth=0,
        annual_wool=0,
        manure_in_stable=1,
        manure_in_non_roofed_enclosure=0,
        manure_in_field=0,
        manure_onfarm_fraction=0,
        manure_sales_fraction=0,
        body_weight_weaning=0,
        body_weight_year_one=0,
        adult_weight=600,
        work_hour=0,
        piglets_relying_on_milk=0
      )

      ## 2.2) Feed_Items #####
      feed_fixed<-c(
        slope=1,
        slope_desc="Flat (0-5%)",
        slope_p_factor=0.11,
        slope_length=15,
        water_regime=0,
        n_content=0,
        cultivation_period=0,
        ecosystem_type=0,
        organic_amendment=0,
        cut_carry_fraction=0,
        fraction_as_fertilizer=0, # Needs to be distributed by crop
        fraction_as_manure=0, # Needs to be distributed by crop
        n_fertilizer=0,
        urea=0,
        npk=0,
        dap=0,
        ammonium_nitrate=0,
        ammonium_sulfate=0,
        n_solutions=0,
        ammonia=0,
        time_horizon=0
      )

      fi_cols<-colnames(feed_items)
      zero_cols<-fi_cols[grep("trees|dbh|diameter_",fi_cols)]
      names(zero_cols)<-zero_cols
      zero_cols[1:length(zero_cols)]<-0
      feed_fixed<-c(feed_fixed,zero_cols)

        ### 2.2.1)  Add fixed items to input template #####
        for(k in 1:length(feed_fixed)){
          variable<-names(feed_fixed)[k]
          v37_feed_items_merge[,(variable):=feed_fixed[k]]
        }

        # Check all cols in template are present
        colnames(feed_items)[!colnames(feed_items) %in% colnames(v37_feed_items_merge)]

        # Enforce numeric class
        num_cols<-c("slope_p_factor","slope_length")
        v37_feed_items_merge[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

      ## 2.3) Seasons #####
      season_dat<-data.frame(season_length=180,season_name="season_x")

  # 3) Loop through farms ####
  # unique farm ids
  farms<-na.omit(unique(herd$Ids))

  # Remove farms with no feed basket information ####
  if(basename(input_files[ii])=="NPA_Kenya_April2025.xlsx"){
    farms<-farms[!farms %in% c("660094058","660413255")]
  }

  # Remove farms with duplicate livetypes in the feed basket
  unique(v37_feed_basket[,.(N=.N),by=.(Ids,livetype_code,livetype_desc,feed_type_code)][N>1][order(Ids),!"feed_type_code"])

  farm_paras<-lapply(1:length(farms),FUN=function(i){
    farm<-farms[i]

    ## 3.1) Herds #####
    # Make a blank herd template to populate
    livestock<-data.table(template$livestock[1,])
    livestock[1,1:ncol(livestock)]<-NA

    # Subset v37 data to selected farm
    livestock_37<-herd[Ids==farm & !grepl("calve",livetype_desc_v37,ignore.case = T)]

    # Loop through herds on the farm
    paras<-lapply(1:nrow(livestock_37),FUN=function(j){

      # Subset herds
      herd_1<-livestock_37[j]

      cat("\r farm id", farm, "i =", i, "/", length(farms), "herd (j) =",herd_1$livetype_code,herd_1$livetype_desc_v37, j, "/", nrow(livestock_37))

      # Merge lkp_livetype
      l_code<-herd_1$livetype_code
      livestock$livetype_code<-l_code
      livestock$livetype_code<-as.integer(livestock$livetype_code)

      mergedat<-lkp_livetype[livetype_code==l_code]

      cols_m<-colnames(mergedat)
      cols_m<-cols_m[cols_m != "livetype_code"]
      cols_l<-c(colnames(livestock)[!colnames(livestock) %in% cols_m])
      livestock<-livestock[,cols_l,with=F]

      livestock<-merge(livestock,mergedat,by="livetype_code",all.x = T)

      # Insert farm specific V37 herd data
      cols_m<-colnames(herd_1)
      livestock$livetype_desc_v37<-herd_1$livetype_desc_v37
      livestock$herd_composition<-herd_1$number
      livestock$body_weight<-herd_1$body_weight
      livestock$annual_milk<-herd_1$annual_milk
      livestock$time_in_stable<-herd_1$time_in_stable
      livestock$time_in_onfarm_grazing<-herd_1$time_in_onfarm_grazing
      livestock$time_in_offfarm_grazing<-herd_1$time_in_offfarm_grazing
      livestock$time_in_non_roofed_enclosure<-herd_1$time_in_non_roofed_enclosure
      livestock$distance_to_pasture<-herd_1$distance_to_pasture

      # Add fixed parameters
      for(k in 1:ncol(livestock_fixed)){
        variable<-colnames(livestock_fixed)[k]
        livestock[,(variable):=livestock_fixed[1,k]]
      }

      # Rename ipcc parameter columns
      ipcc_new<-c(
      "ipcc_ef_category_t1",
      "ipcc_ef_category_t2",
      "ipcc_meth_man_category",
      "ipcc_n_exc_category"
      )

      ipcc_old<-c(
      "ipcc_meth_ef_t1",
      "ipcc_meth_ef_t2",
      "ipcc_meth_man",
      "ipcc_meth_exc"
      )

      livestock<-livestock[,!ipcc_new,with = F]
      setnames(livestock,ipcc_old,ipcc_new)

    # Enforce codes to be character
    livestock[,livetype_code:=as.character(livetype_code)]

    ## 3.2) Fertilizer #####
      # Blank table created in section 1.3
    # 3.3) Feed basket ####
      ### 3.3.1) Seasons #####
       # Created in section 2.3
      ### 3.3.2) Feeds #####
        feeds<-v37_feed_basket[Ids==farm & tolower(livetype_desc)==tolower(livestock$livetype_desc_v37)]
        if(nrow(feeds)==0){
          stop("Feed table is blank, livetype_desc in feed basket not matching herd.")
        }

        # Average across duplicate rows
        #feeds<-feeds[,.(allocation=mean(allocation)),by=.(Ids,feed_type_code,feed_item_code,livetype_code)]

        feeds<-feeds[,.(livestock=list(data.table(livetype_code=livetype_code,allocation=allocation))),by=.(feed_item_code,feed_type_code)]
        feeds<-data.frame(feeds)
      ### 3.3.3) Combine Season and Feeds #####
        feed_basket <- data.table(
          feeds = list(feeds=feeds),
          season_name = season_dat$season_name[1]
        )
        feed_basket<-data.frame(feed_basket)

    ## 3.4) Feed Items #####
    # v37_feed_items_merge is creted in section 2.2, subset to items in the diet
    feed_items<-v37_feed_items_merge[feed_item_code %in% feeds$feed_item_code]
    ## 3.5) Join sections into list structure ####
    input_object<-c(simple_fields,list(livestock=livestock,
                                       feed_items=feed_items,
                                       fertilizer=fertilizer,
                                       seasons=season_dat,
                                       feed_basket=feed_basket))
    input_object$farm_name<-farm
    return(input_object)
    })
    names(paras)<-paste0("herd-",1:nrow(livestock_37))

    return(paras)
  })

  names(farm_paras)<-paste0("f",farms)

  # 4) Pass to cleaned functions ####

  ghg_emissions<-lapply(1:length(farm_paras),FUN=function(i){

    farm_para<-farm_paras[[i]]

    result<-lapply(1:length(farm_para),FUN=function(j){

      if(messages==F){
          cat("farm (i)",names(farm_paras)[i],i,"/",length(farm_paras),"herd (j)",j,"      \r")
        }else{
          cat("farm (i)",names(farm_paras)[i],i,"/",length(farm_paras),"herd (j)",j,"      \n")
        }

      para<-farm_para[[j]]

      ## 4.1) feed basket quality #####
      feed_basket_quality <- feed_quality(para)

      ## 4.2) energy #####
      # Table 10.4 is in list form this throws an error, convert to data.frame
      energy_required <- suppressMessages(energy_requirement(para,
                                                             feed_basket_quality,
                                                             energy_parameters = energy_parameters
                                                             ))

      ## 4.3) land #####
      para$feed_items$intercrop<-as.numeric(para$feed_items$intercrop)
      land_required <- land_requirement(feed_basket_quality, energy_required, para)

      ## 4.4) soil erosion #####
      para$feed_items$slope_p_factor<-as.numeric(para$feed_items$slope_p_factor)
      soil_erosion <- soil_health(para, land_required)

      ## 4.5) n balance #####
      para$feed_items$ammonia<-as.numeric(para$feed_items$ammonia)
      para$feed_items$ammonium_nitrate<-as.numeric(para$feed_items$ammonium_nitrate)
      para$feed_items$ammonium_sulfate<-as.numeric(para$feed_items$ammonium_sulfate)
      para$feed_items$dap<-as.numeric(para$feed_items$dap)
      para$feed_items$n_solutions<-as.numeric(para$feed_items$n_solutions)
      para$feed_items$npk<-as.numeric(para$feed_items$npk)
      para$feed_items$urea<-as.numeric(para$feed_items$urea)

      nitrogen_balance <- n_balance(para, land_required, soil_erosion,energy_required)

      ## 4.6) livestock productivity #####
      livestock_productivity <- land_productivity(para,energy_required)

      ## 4.7) biomass #####
      num_cols<-c("trees_ha_dbh25","trees_ha_dbh2550","trees_ha_dbh50","increase_dbh25","increase_dbh2550","increase_dbh50",
                  "time_horizon","average_dbh2550","average_dbh25","average_dbh50")
      para$feed_items<-para$feed_items[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

      biomass <- biomass_calculation(para, land_required)

      ## 4.8) ghg_emissions #####
      char_cols<-c("manureman_non_roofed_enclosure")
      para$livestock<-para$livestock[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]

      results<-suppressMessages(ghg_emission(para,energy_required,ghg_ipcc_data,land_required,nitrogen_balance,feed_basket_quality,ym_prod=F))

      # Unpack soil into 3 tables not a list
      results$soil_annual_N20N_soil_direct_emission<-results$ghg_soil$annual_N20N_soil_direct_emission
      results$soil_annual_N20N_soil_indirect_emission<-results$ghg_soil$annual_N20N_soil_indirect_emission
      results$soil_N20N_off_farm<-results$ghg_soil$N20N_off_farm
      results$ghg_soil<-NULL
      results$livestock_productivity<-livestock_productivity
      results
    })
    names(result)<-names(farm_para)
    return(result)
  })

  names(ghg_emissions)<-names(farm_paras)

  ## 4.9) Add farm and herd name to tables
  ghg_emissions_merge<-lapply(1:length(ghg_emissions),FUN=function(i){
    farm_name<-names(ghg_emissions)[i]
    ghg_farm<-ghg_emissions[[i]]

    ghg_farm_updated<-lapply(1:length(ghg_farm),FUN=function(j){
      if(messages==F){
      cat("i",i,"j",j,"      \r")
      }else{
        cat("i",i,"j",j,"      \n")
      }
      herd_name<-names(ghg_farm)[j]
      ghg_herd<-ghg_farm[[j]]
      ghg_herd_updated<-lapply(1:length(ghg_herd),FUN=function(k){
        data<-ghg_herd[[k]]
        data$farm<-farm_name
        data$herd<-herd_name
        return(data)
      })
      names(ghg_herd_updated)<-names(ghg_herd)
      return(ghg_herd_updated)
    })

    tab_names<-names(ghg_farm_updated[[1]])
    ghg_farm_merged<-lapply(tab_names,FUN=function(tab_name){
      x<-lapply(ghg_farm_updated,"[[",tab_name)
      x<-rbindlist(x)
      x
    })

    names(ghg_farm_merged)<-tab_names
    return(ghg_farm_merged)
  })
  names(ghg_emissions_merge)<-names(ghg_emissions)

    ## 4.1) Combine tables #####
  tab_names<-names(ghg_emissions_merge[[1]])

  ghg_emissions_merge_all<-lapply(tab_names,FUN=function(tab_name){
    x<-lapply(ghg_emissions_merge,"[[",tab_name)
    x<-rbindlist(x)
    x
  })
  names(ghg_emissions_merge_all)<-tab_names

  n<-nchar(names(ghg_emissions_merge_all))
  names(ghg_emissions_merge_all)[n>31]
  names(ghg_emissions_merge_all)[n>31]<-c("annual_N20N_soil_direct","annual_N20N_soil_indirect")

  result<-ghg_emissions_merge_all$ef

  result[is.na(enteric_methane_emissions)]

  # Compute enteric CH₄ intensity for milk production (kg CH₄ per kg FPCM)
  FPCM<-ghg_emissions_merge_all$livestock_productivity[,.(farm,herd,total_milk)]
  result<-merge(result,FPCM,all.x=T)
  result[total_milk==0,total_milk:=NA]
  result[,CH4_intensity_FPCM:=enteric_methane_emissions/total_milk]

  result_farm<-result[,.(enteric_methane_emissions=sum(enteric_methane_emissions,na.rm=T),
                         total_milk=sum(total_milk,na.rm=T)),by=farm
                      ][,CH4_intensity_FPCM:=enteric_methane_emissions/total_milk]


  ghg_emissions_merge_all$ef_farm<-result_farm

  # Create a new Excel workbook
  wb <- createWorkbook()

  # Loop through the list and add each data.frame as a sheet
  for (sheet_name in names(ghg_emissions_merge_all)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = ghg_emissions_merge_all[[sheet_name]])
  }

  ## 4.2) Save and return results ####
  saveWorkbook(wb, file = save_file, overwrite = TRUE)
  jsonlite::write_json(ghg_emissions_merge_all,gsub(".xlsx",".json",save_file),simplifyVector=T)

  cat("Output saved as:\n",save_file,"\n",gsub(".xlsx",".json",save_file),"\n")

  result$input_file<-basename(input_files[ii])
  result_farm$input_file<-basename(input_files[ii])
  return(list(result,result_farm))
})

names(ef)<-names(input_files)
