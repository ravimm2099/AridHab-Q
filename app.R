library(shiny)
if (!requireNamespace('jsonlite', quietly=TRUE)) stop('Please install jsonlite')

kumar <- read.csv('scorecard.csv', check.names=FALSE, stringsAsFactors=FALSE, fileEncoding='UTF-8')
eco <- read.csv('ecological_extension.csv', check.names=FALSE, stringsAsFactors=FALSE, fileEncoding='UTF-8')

# HQI scoring methodology v2.0: permissible-category-specific normalization.
# Original ordinal ratings are retained in JSON and CSV records.
SCORING_VERSION <- 'HQI-attribute-specific-v2.0-2026-10-27'
# Harmonize the source tables: the original source includes component/source_note,
# while the ecological extension uses caution. Only the shared rubric columns
# are needed for scoring; retain the original source tables for UI descriptions.
rating_columns <- paste0('rating_', 1:5)
rubric_columns <- c('id', 'attribute', rating_columns)
for (table_name in c('kumar', 'eco')) {
  missing <- setdiff(rubric_columns, names(get(table_name)))
  if (length(missing)) stop(paste(table_name, 'is missing:', paste(missing, collapse=', ')))
}
score_catalog <- rbind(
  kumar[, rubric_columns, drop=FALSE],
  eco[, rubric_columns, drop=FALSE]
)
score_catalog$id <- as.integer(score_catalog$id)
score_catalog <- score_catalog[order(score_catalog$id), , drop=FALSE]
stopifnot(identical(sort(as.integer(score_catalog$id)), 1:37))
allowed_ratings <- lapply(seq_len(nrow(score_catalog)), function(j) {
  labels <- unlist(score_catalog[j, paste0('rating_',1:5)], use.names=FALSE)
  which(!is.na(labels) & nzchar(trimws(as.character(labels))))
})
names(allowed_ratings) <- as.character(score_catalog$id)
stopifnot(all(vapply(allowed_ratings, length, integer(1)) >= 2L))
# For the source rubrics, ordinal category positions are treated as equally spaced
# in degradation severity; this is a declared modelling assumption, not field validation.
normalized_degradation <- function(id, rating) {
  n <- max(length(id), length(rating))
  if (!n) return(numeric(0))
  id <- rep_len(id, n); rating <- rep_len(rating, n)
  vapply(seq_len(n), function(j) {
    ii <- suppressWarnings(as.integer(as.character(id[j])))
    rr <- suppressWarnings(as.numeric(as.character(rating[j])))
    if (is.na(ii) || is.na(rr) || !(as.character(ii) %in% names(allowed_ratings))) return(NA_real_)
    opts <- allowed_ratings[[as.character(ii)]]
    pos <- match(rr, opts)
    if (is.na(pos)) return(NA_real_)
    100 * (pos - 1) / (length(opts) - 1)
  }, numeric(1))
}
component_degradation <- function(ids, ratings) {
  d <- normalized_degradation(ids, ratings)
  if (length(d) != length(ids) || anyNA(d)) return(NA_real_)
  mean(d)
}


# Revised HQI: higher score means greater habitat degradation. Site bands are provisional.
site_degradation_class <- function(hqi) {
  v <- suppressWarnings(as.numeric(hqi))
  vapply(v, function(x) {
    if(is.na(x) || !is.finite(x) || x < 0 || x > 100) return('Pending')
    d <- x
    if(d <= 25) 'None' else if(d <= 50) 'Slight' else if(d <= 70) 'Moderate' else if(d <= 90) 'Severe' else 'Very severe'
  }, character(1))
}


styles <- paste0('.hero{background:#e9f2eb;padding:22px;border-radius:12px;margin-bottom:15px}.note{background:#fff6e8;padding:12px;border-left:4px solid #c78c3d;margin:12px 0}.stat{background:#f1f5f3;padding:14px;border-radius:9px;margin-bottom:12px}.status{font-weight:600;font-size:18px}',
'.nav-tabs>li>a{font-weight:600;border-radius:7px 7px 0 0;margin-right:3px;color:#173e4a!important}',
'.nav-tabs>li:nth-child(1)>a{background:#e4edf8!important}',
'.nav-tabs>li:nth-child(2)>a{background:#d9f1e2!important}',
'.nav-tabs>li:nth-child(3)>a{background:#e7f0d5!important}',
'.nav-tabs>li:nth-child(4)>a{background:#f8e6d5!important}',
'.nav-tabs>li:nth-child(5)>a{background:#e8e3f8!important}',
'.nav-tabs>li:nth-child(6)>a{background:#d9edf6!important}',
'.nav-tabs>li:nth-child(7)>a{background:#f4e8d7!important}', '.nav-tabs>li:nth-child(8)>a{background:#e6edf5!important}', '.nav-tabs>li:nth-child(9)>a{background:#d9edf6!important}', '.nav-tabs>li:nth-child(10)>a{background:#e8e3f8!important}', '.nav-tabs>li:nth-child(11)>a{background:#d9f1e2!important}', '.nav-tabs>li:nth-child(12)>a{background:#e4edf8!important}',
'.nav-tabs>li.active>a{border-bottom:4px solid #216a75!important;font-weight:800}',
'.photo-gallery{display:flex;gap:12px;flex-wrap:wrap;margin:14px 0}',
'.photo-gallery figure{flex:1 1 220px;margin:0;min-width:180px}',
'.photo-gallery img{width:100%;height:170px;object-fit:cover;border-radius:8px}',
'.photo-gallery figcaption{font-size:12px;color:#53656c;margin-top:4px}',
'.eq{background:#edf5f7;padding:10px 14px;border-radius:7px;font-family:monospace;overflow-x:auto;margin:8px 0}',
'.author{font-size:19px;color:#163f35;text-align:center;margin:12px auto 20px;padding:18px 22px;background:linear-gradient(120deg,#edf7f0,#f6f9fd);border:1px solid #d9e8e0;border-radius:16px;max-width:1100px;box-shadow:0 4px 18px rgba(18,61,47,.08)}.author-name{font-size:27px;font-weight:800;color:#14502f;line-height:1.25}.author-affiliation{font-size:19px;margin:6px 0}.author-contact{font-size:16px}.author-photo{width:120px;height:120px;object-fit:cover;border-radius:50%;border:4px solid white;box-shadow:0 3px 14px rgba(0,0,0,.16);cursor:zoom-in;margin-bottom:8px}.author-photo:hover{transform:scale(1.04);transition:transform .15s}.inventory-help{font-size:14px;color:#52675f;margin:6px 0 14px}', '.app-banner{width:100%;max-height:240px;object-fit:cover;object-position:top left;border-radius:12px;margin-bottom:14px}', '.math-panel{background:#f0f7fa;border:1px solid #d2e3ea;border-radius:8px;padding:12px 16px;margin:10px 0;font-size:16px}', '.photo-pair{display:flex;gap:12px;flex-wrap:wrap;margin:8px 0}', '.photo-pair img{width:100%;height:180px;object-fit:cover;border-radius:9px}', '.photo-pair figure{flex:1 1 260px;margin:0}')

assessmentUI <- function(prefix, label, rows, original=FALSE) {
  tabPanel(label, value=prefix,
    fluidRow(
      column(4,
        selectInput(paste0(prefix,'_id'),'Select indicator',choices=setNames(rows$id,paste0(rows$id,' · ',rows$attribute))),
        uiOutput(paste0(prefix,'_rating')),
        fluidRow(column(4,actionButton(paste0(prefix,'_prev'),'Previous')),column(4,actionButton(paste0(prefix,'_next'),'Next')),column(4,actionButton(paste0(prefix,'_unrated'),'Next unrated'))),
        uiOutput(paste0(prefix,'_live')), 
        textInput(paste0(prefix,'_measurement'),'Observed measurement (optional; with units)'),
        textAreaInput(paste0(prefix,'_evidence'),'Field evidence and ecological justification',rows=3),
        actionButton(paste0(prefix,'_save'),'Save & next attribute',class='btn-primary'),
         div(class='note','Ratings are automatically saved when you use Next, Previous, Next unrated, or change the indicator dropdown. Completing a component opens the next assessment tab.'),
        actionButton(paste0(prefix,'_clear'),'Clear selected'),
        div(class='note',if(original) 'Published vegetation degradation rating descriptions; 1 indicates least degradation.' else 'Proposed ecological function rubric; 1 indicates minimal impairment and 5 indicates very severe impairment.')
      ),
      column(8,
        h3(textOutput(paste0(prefix,'_title'))),
        h4(if(original) 'Published degradation rating descriptions' else 'Proposed visual interpretation criteria'),
        tableOutput(paste0(prefix,'_table')),
        uiOutput(paste0(prefix,'_image')),
        hr(),h4('Current component status'),uiOutput(paste0(prefix,'_status'))
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$script(HTML("Shiny.addCustomMessageHandler('clearAridHabQFiles', function(ids) { ids.forEach(function(id) { var el=document.getElementById(id); if(el){el.value=''; $(el).trigger('change');} }); });")),tags$title('AridHab-Q'), tags$style(HTML(styles))),
  withMathJax(),
  tags$img(src='app_banner.jpg',class='app-banner',alt='AridHab-Q banner supplied by author'),
  div(class='author',
    tags$img(src='author_portrait.png',class='author-photo',alt='Portrait of Dr. Manish Mathur',title='Click to enlarge portrait',
      onclick="document.getElementById('author-photo-overlay').style.display='flex';"),
    tags$div(class='author-name','Dr. Manish Mathur, CTO'),
    tags$div(class='author-affiliation','ICAR-Central Arid Zone Research Institute, Jodhpur, India'),
    tags$div(class='author-contact',tags$a(href='mailto:eco5320@gmail.com','eco5320@gmail.com'),'  |  ',tags$a(href='mailto:manish.mathur@icar.org.in','manish.mathur@icar.org.in'))),
  tags$div(id='author-photo-overlay',style='display:none;position:fixed;inset:0;background:rgba(0,0,0,.78);z-index:99999;align-items:center;justify-content:center;cursor:zoom-out;',
    onclick="this.style.display='none';",tags$img(src='author_portrait.png',style='max-width:85vw;max-height:85vh;object-fit:contain;border-radius:12px;',alt='Enlarged author portrait')), 
  uiOutput('global_progress'),
  tabsetPanel(id='tabs',
    tabPanel('Start & site metadata',
      fluidRow(column(6,textInput('site','Site / habitat name'),textInput('site_id','Unique site ID (required for saved records)'),textInput('assessor','Assessor'),dateInput('date','Assessment date',value=Sys.Date())),
               column(6,textInput('habitat','Habitat / landform'),numericInput('latitude','Latitude (decimal degrees)',value=NA_real_,min=-90,max=90),numericInput('longitude','Longitude (decimal degrees)',value=NA_real_,min=-180,max=180),textAreaInput('site_note','Season, rainfall, land use and observation notes',rows=4))),
      fileInput('field_photo','Upload field photo (JPEG/PNG)',accept=c('.jpg','.jpeg','.png')),
      actionButton('save_site','Save / update this site',class='btn-primary'),
      tags$div(style='padding:12px 14px;margin:12px 0;border:1px solid #d4e3db;border-radius:12px;background:#f2f8f4;',
        tags$strong('Site-wise data management'),
        tags$p('Start a fresh site assessment, clear only the current site ratings, or permanently delete the active site and its saved results.'),
        actionButton('clear_current_site','Clear current site ratings',class='btn-warning'),
        actionButton('delete_active_site','Delete current site record and results',class='btn-danger')),
      selectInput('stored_site','Open a saved site',choices=c('Choose saved site'='')),actionButton('load_site','Load selected site'),actionButton('new_site','New blank assessment'),
      p('Site records are saved on the computer/server running Shiny in the records folder. Download a JSON backup for portability.'),
      div(class='note','The dashboard describes habitat status at the assessment date. It does not infer historical change from a single survey.'),
      h3('Field photographs'),
      div(class='photo-pair',tags$figure(tags$img(src='site_erosion.jpg',alt='Erosion and arid vegetation'),tags$figcaption('Erosion and arid vegetation')),tags$figure(tags$img(src='flowering_tree.jpg',alt='Flowering dryland tree'),tags$figcaption('Flowering tree'))),
      div(class='photo-gallery',
        tags$figure(tags$img(src='field_1.jpg',alt='Shrub vegetation and erosion features'),tags$figcaption('Shrub vegetation and soil exposure')),
        tags$figure(tags$img(src='field_2.jpg',alt='Quarry disturbance'),tags$figcaption('Rock extraction and disturbed terrain')),
        tags$figure(tags$img(src='field_3.jpg',alt='Livestock grazing'),tags$figcaption('Livestock grazing in arid rangeland')),
        tags$figure(tags$img(src='field_4.jpg',alt='Arid shrubland'),tags$figcaption('Shrubland vegetation and bare patches')))
    ),
    tabPanel('Data import, site inventory & multi-site validation',
      h3('Import, export and compare assessments'),
      downloadButton('template','Download blank attribute CSV template'),
      fileInput('import_csv','Import attribute CSV (id, rating, measurement, evidence)',accept='.csv'),
      actionButton('apply_csv','Apply imported CSV to current site'),
      fileInput('import_json','Restore a previously downloaded JSON assessment',accept='.json'),
      actionButton('apply_json','Restore JSON'),
      div(class='stat',style='border-left:5px solid #b4533b;',
        h4('Manage imported datasets and saved results'),
        p('Start a new dataset without mixing its results with the previous import. Download a backup before permanently deleting records.'),
        actionButton('reset_dataset','Clear current dataset / start new',class='btn-warning'),
        actionButton('delete_selected_site','Delete selected saved site',class='btn-default'),
        actionButton('delete_all_sites','Delete ALL saved site records and results',class='btn-danger'),
        p('Delete actions require confirmation. Deleting all sites also removes their stored field photographs.')),
      h4('Saved site inventory'),tableOutput('inventory'),downloadButton('inventory_csv','Download all-site inventory CSV'),
      h4('Saved-site component comparison'),plotOutput('inventory_plot',height='320px'),
      p('All-site inventory includes only sites explicitly saved on this installation. For cloud deployments use persistent storage; ephemeral hosting may erase local records.'),
      hr(),
      h3('Multi-site CSV & validation'),
      h3('Import multiple sites in one CSV'),
      p('One row per site; Site ID followed by 37 ratings in attribute order. The original 36-site CSV is supported. Invalid ratings are flagged and stored as missing, never silently changed.'),
      fileInput('bulk_csv','Upload wide-format multi-site CSV',accept='.csv'),
      checkboxInput('bulk_confirm','I have reviewed the validation report and want to save all sites',FALSE),
      actionButton('bulk_save','Import validated records to site inventory',class='btn-primary'),
      downloadButton('bulk_template','Download multi-site CSV template'),
      h4('Import preview'),tableOutput('bulk_preview'),
      h4('Invalid values requiring field verification'),tableOutput('bulk_issues'),
      downloadButton('bulk_issues_csv','Download issue report'),
      h4('Multi-site inventory'),p(class='inventory-help','Site IDs are sorted numerically (1, 2, 3 … 36), with non-numeric IDs sorted alphabetically. Use the site selector in Current habitat status to inspect a record.'),tableOutput('bulk_inventory'),downloadButton('bulk_inventory_csv','Export site inventory')),
    assessmentUI('entire','Entire vegetation (1–7)',subset(kumar,id>=1&id<=7),TRUE),
    assessmentUI('herb','Herbaceous component (8–17)',subset(kumar,id>=8&id<=17),TRUE),
    assessmentUI('woody','Woody perennial component (18–25)',subset(kumar,id>=18&id<=25),TRUE),
    assessmentUI('eco','Ecosystem functional integrity (26–37)',eco,FALSE),
    tabPanel('Intelligent scoring guidance',
      h3('Evidence-based consistency assistant'),
      p('Advisory only: no rating is prohibited based on another rating. Dryland mosaics, species composition and rainfall may produce valid contrasting scores.'),
      selectInput('guide_attribute','Attribute to assess next',choices=NULL),
      uiOutput('guide_current'),tableOutput('guide_related'),
      uiOutput('guide_suggestion'),
      p('Recommendations use related observed ratings and multi-site co-occurrence only when enough comparable complete records exist. They do not substitute for field evidence.')),
    tabPanel('Current habitat status & scorecard',value='status',
      h3('Current observed condition by assessment component'),
      selectInput('dashboard_site','View assessment / imported site',choices=c('Current working assessment'='__current__')),
      uiOutput('dashboard_site_notice'),
      h4('Equations used for final 37-attribute habitat-degradation assessment'),
      div(class='math-panel',HTML('\\[d_i=100\\frac{\\operatorname{rank}_{R_i}(r_i)-1}{k_i-1},\\quad r_i\\in R_i,\\quad i=1,\\ldots,37\\]')),
      div(class='math-panel',HTML('\\[D_g=\\frac{1}{n_g}\\sum_{i\\in I_g}d_i,\\quad n_g\\in\\{7,10,8,12\\}\\]')),
      div(class='math-panel',HTML('\\[HQI_{37}=\\frac{D_E+D_H+D_W+D_F}{4}\\]')),
      p('The final HQI is calculated from all 37 ratings, grouped into 7 entire-vegetation, 10 herbaceous, 8 woody-perennial and 12 ecosystem-functional attributes. Each component contributes 25%. Each attribute uses its own available ordinal categories, with higher permissible categories indicating greater degradation.'),
      uiOutput('equation_values'),
      uiOutput('final_degradation_class'),
      plotOutput('component_plot',height='260px'),hr(),tableOutput('scorecard'),
      downloadButton('download','Export complete assessment CSV'),downloadButton('download_json','Download assessment JSON'),
      div(class='note','Integrated HQI is an explicitly proposed, equal-component-weighted index. It is displayed only after all 37 ratings are complete. Interpretive degradation bands are provisional, not empirically calibrated.')
    ),
    tabPanel('Attribute statistics & frequency',
      p('Ratings are ordinal. Parametric t-tests are exploratory; Mann–Whitney tests are reported alongside. Select two groups with at least two sites each.'),
      selectInput('stat_attribute','Indicator',choices=NULL),
      selectInput('group_sites_a','Group A sites',choices=NULL,multiple=TRUE),
      selectInput('group_sites_b','Group B sites',choices=NULL,multiple=TRUE),
      tableOutput('frequency_table'),plotOutput('frequency_plot',height='300px'),
      tableOutput('distribution_statistics'),tableOutput('group_test'),
      downloadButton('all_stats_csv','Download all-attribute descriptive statistics')),
    tabPanel('PCA: sites and attributes',
      p('Exploratory PCA of attribute-specific ordinal ratings, standardized by attribute. Complete cases only; PCA axes are not validated habitat-quality gradients.'),
      selectInput('pca_color','Color sites by',choices=c('No grouping'='none','HQI class'='hqi')),
      checkboxInput('pca_show_arrows','Show attribute eigenvector arrows on biplot',TRUE),
      sliderInput('pca_arrow_scale','Arrow length',min=0.5,max=5,value=2,step=0.25),
      uiOutput('pca_interactive_ui'),
      plotOutput('pca_sites',height='460px'),plotOutput('pca_loadings',height='400px'),
      h4('Eigenvalues and explained variance'),tableOutput('pca_variance'),
      h4('Attribute loadings / eigenvectors'),tableOutput('pca_loading_table'),
      h4('Site principal-component scores'),tableOutput('pca_site_table'),
      h4('Variable contributions (%) to PC1 and PC2'),tableOutput('pca_contributions'),
      downloadButton('pca_scores_csv','Export PCA site scores'),downloadButton('pca_loadings_csv','Export PCA loadings'),downloadButton('pca_eigen_csv','Export eigenvalues')),
    tabPanel('Methodology & cautions',
      h3('Methodological attribution'),
      p(paste('HQI scoring version:', SCORING_VERSION)),
       p('Original 25 indicators and published categories: Suresh Kumar (1992), Assessment of Vegetation Degradation: Status of Methodological Research, Annals of Arid Zone 31(1):53–62, Table 7.'),
      p('Indicators 26–37 and their visual rubrics are proposed ecological-function extensions. They are not part of the 1992 publication and require independent ecological validation.'),
      p('Illustrations of original attributes are conceptual and not reproductions of the 1992 rating table.'),
      p('A single visit establishes current observed condition, not change through time. The integrated HQI requires all 37 ratings. Proposed direct-HQI degradation bands: 0–25 none, >25–50 slight, >50–70 moderate, >70–90 severe, >90–100 very severe. These bands are provisional.'),
      p('Attribute degradation is normalized by position within its own permissible rating categories: d_i = 100(rank_i - 1)/(k_i - 1). Domain score = mean of normalized scores. HQI37 is the equal mean of four domains. This is a proposed index, not a published formula from the source paper.')
    )
  )
)

server <- function(input,output,session){

  empty_saved <- function() data.frame(id=integer(),rating=integer(),measurement=character(),evidence=character(),stringsAsFactors=FALSE)
  saved <- reactiveVal(empty_saved())
  record_dir <- 'records'
  dir.create(record_dir,showWarnings=FALSE,recursive=TRUE)
  site_key <- function(x) { x <- gsub('[^A-Za-z0-9_-]','_',trimws(as.character(x))); substr(x,1,90) }
  record_path <- function(id) file.path(record_dir,paste0(site_key(id),'.json'))
  metadata <- function() list(site_id=input$site_id,site=input$site,assessor=input$assessor,date=as.character(input$date),habitat=input$habitat,latitude=input$latitude,longitude=input$longitude,site_note=input$site_note)
  clean_records <- function(x) {
    if (is.null(x) || !NROW(x)) return(empty_saved())
    x <- as.data.frame(x,stringsAsFactors=FALSE)
    if (!all(c('id','rating') %in% names(x))) stop('Missing id/rating fields')
    if (!'measurement' %in% names(x)) x$measurement <- ''
    if (!'evidence' %in% names(x)) x$evidence <- ''
    x <- x[,c('id','rating','measurement','evidence')]
    x$id <- suppressWarnings(as.integer(x$id));x$rating <- suppressWarnings(as.integer(x$rating))
    x$measurement[is.na(x$measurement)] <- ''; x$evidence[is.na(x$evidence)] <- ''
    if (anyNA(x$id) || anyDuplicated(x$id) || any(!x$id %in% 1:37)) stop('Indicator IDs must be unique integers from 1 to 37')
    for (j in seq_len(nrow(x))) {
      ref <- catalog[match(x$id[j],catalog$id),,drop=FALSE]
      opts <- unlist(ref[1,paste0('rating_',1:5)],use.names=FALSE)
      allowed <- which(!is.na(opts) & nzchar(trimws(opts)))
      if (!is.na(x$rating[j]) && !x$rating[j] %in% allowed) stop(paste('Invalid rating for attribute',x$id[j]))
    }
    x[order(x$id),,drop=FALSE]
  }
  write_site <- function() {
    key <- site_key(input$site_id)
    if (!nzchar(key)) {showNotification('Enter a unique site ID before saving.',type='error');return(FALSE)}
    lat <- input$latitude;lon <- input$longitude
    if ((!is.na(lat) && (lat < -90 || lat > 90)) || (!is.na(lon) && (lon < -180 || lon > 180))) {showNotification('Coordinates out of range',type='error');return(FALSE)}
    obj <- list(schema_version=1,scoring_version=SCORING_VERSION,metadata=metadata(),ratings=saved(),saved_at=as.character(Sys.time()))
    tmp <- tempfile(tmpdir=record_dir,fileext='.json')
    jsonlite::write_json(obj,tmp,auto_unbox=TRUE,pretty=TRUE,na='null')
    if (!file.rename(tmp,record_path(key))) {file.copy(tmp,record_path(key),overwrite=TRUE);unlink(tmp)}
    refresh_sites(); TRUE
  }
  refresh_sites <- function() {
    paths <- list.files(record_dir,pattern='\\.json$',full.names=FALSE)
    ids <- sub('\\.json$','',paths)
    updateSelectInput(session,'stored_site',choices=c('Choose saved site'='',setNames(ids,ids)))
    site_revision(isolate(site_revision()) + 1L)
  }
  site_revision <- reactiveVal(0L)
  restore_obj <- function(obj) {
    if (is.null(obj$metadata) || is.null(obj$ratings)) stop('JSON must contain metadata and ratings')
    m <- obj$metadata
    updateTextInput(session,'site_id',value=if(is.null(m$site_id)) '' else as.character(m$site_id))
    updateTextInput(session,'site',value=if(is.null(m$site)) '' else as.character(m$site))
    updateTextInput(session,'assessor',value=if(is.null(m$assessor)) '' else as.character(m$assessor))
    if (!is.null(m$date) && nzchar(as.character(m$date))) updateDateInput(session,'date',value=as.character(m$date))
    updateTextInput(session,'habitat',value=if(is.null(m$habitat)) '' else as.character(m$habitat))
    updateNumericInput(session,'latitude',value=if(is.null(m$latitude)) NA_real_ else as.numeric(m$latitude))
    updateNumericInput(session,'longitude',value=if(is.null(m$longitude)) NA_real_ else as.numeric(m$longitude))
    updateTextAreaInput(session,'site_note',value=if(is.null(m$site_note)) '' else as.character(m$site_note))
    saved(clean_records(obj$ratings))
  }
  observeEvent(input$save_site,{if(write_site()) showNotification('Site record saved on this installation.',type='message')})
  observeEvent(input$load_site,{req(nzchar(input$stored_site));tryCatch({restore_obj(jsonlite::read_json(record_path(input$stored_site),simplifyVector=TRUE));showNotification('Site loaded')},error=function(e)showNotification(conditionMessage(e),type='error'))})
  observeEvent(input$new_site,{saved(empty_saved());updateTextInput(session,'site_id',value='');updateTextInput(session,'site',value='');showNotification('New blank assessment. Previous saved sites remain on disk.')})
  observeEvent(input$apply_json,{req(input$import_json);tryCatch({restore_obj(jsonlite::read_json(input$import_json$datapath,simplifyVector=TRUE));showNotification('JSON restored; save site to persist it')},error=function(e)showNotification(conditionMessage(e),type='error'))})
  observeEvent(input$apply_csv,{req(input$import_csv);tryCatch({x<-read.csv(input$import_csv$datapath,stringsAsFactors=FALSE);saved(clean_records(x));showNotification('CSV imported; save site to persist it')},error=function(e)showNotification(conditionMessage(e),type='error'))})
  observeEvent(input$field_photo,{req(input$field_photo);if (!nzchar(site_key(input$site_id))) {showNotification('Enter site ID before uploading photos',type='error');return()};ext<-tolower(tools::file_ext(input$field_photo$name));if(!ext %in% c('jpg','jpeg','png'))return();folder<-file.path(record_dir,'photos',site_key(input$site_id));dir.create(folder,recursive=TRUE,showWarnings=FALSE);dest<-file.path(folder,paste0(format(Sys.time(),'%Y%m%d_%H%M%S'),'_',basename(gsub('[^A-Za-z0-9_.-]','_',input$field_photo$name))));file.copy(input$field_photo$datapath,dest);showNotification('Photo saved to site photo folder')})
  clear_workspace <- function() {
    saved(empty_saved())
    updateTextInput(session,'site_id',value='')
    updateTextInput(session,'site',value='')
    updateTextInput(session,'assessor',value='')
    updateTextInput(session,'habitat',value='')
    updateTextInput(session,'site_note',value='')
    updateNumericInput(session,'latitude',value=NA_real_)
    updateNumericInput(session,'longitude',value=NA_real_)
    updateSelectInput(session,'dashboard_site',selected='__current__')
    updateCheckboxInput(session,'bulk_confirm',value=FALSE)
    session$sendCustomMessage('clearAridHabQFiles',c('bulk_csv','import_csv','import_json','field_photo'))
  }
  observeEvent(input$clear_current_site,{
    showModal(modalDialog(title='Clear ratings for this site?',
      p('All 37 ratings and their current unsaved evidence will be cleared. Saved records on disk remain unchanged until you save the blank assessment.'),
      footer=tagList(modalButton('Cancel'),actionButton('confirm_clear_current_site','Clear current ratings',class='btn-warning')),easyClose=TRUE))
  })
  observeEvent(input$confirm_clear_current_site,{
    removeModal()
    saved(empty_saved())
    showNotification('Current site ratings cleared. The saved site record remains until explicitly deleted or overwritten.',type='message',duration=8)
  })
  observeEvent(input$delete_active_site,{
    id<-site_key(input$site_id)
    if(!nzchar(id)){showNotification('Enter or load a Site ID first.',type='error');return()}
    showModal(modalDialog(title='Permanently delete current site?',
      p(sprintf('Delete saved Site ID %s, its ratings, computed results and associated stored photographs?',id)),
      footer=tagList(modalButton('Cancel'),actionButton('confirm_delete_active_site','Delete this site permanently',class='btn-danger')),easyClose=TRUE))
  })
  observeEvent(input$confirm_delete_active_site,{
    removeModal()
    id<-site_key(input$site_id)
    if(!nzchar(id)){showNotification('Site ID is missing.',type='error');return()}
    path<-record_path(id)
    existed<-file.exists(path)
    if(existed)unlink(path)
    photos<-file.path(record_dir,'photos',id)
    if(dir.exists(photos))unlink(photos,recursive=TRUE)
    clear_workspace();refresh_sites()
    showNotification(if(existed)sprintf('Site %s and its saved results deleted.',id) else sprintf('No saved record for Site %s; current assessment cleared.',id),type='warning',duration=8)
  })
  observeEvent(input$reset_dataset,{
    showModal(modalDialog(title='Start a new dataset?',
      p('This clears the current working assessment and uploaded CSV selections. Previously saved site records will remain in the inventory unless you delete them separately.'),
      footer=tagList(modalButton('Cancel'),actionButton('confirm_reset_dataset','Clear working dataset',class='btn-warning')),easyClose=TRUE))
  })
  observeEvent(input$confirm_reset_dataset,{
    removeModal();clear_workspace()
    showNotification('Working dataset cleared. Existing saved site records remain until deleted.',type='message',duration=8)
  })
  observeEvent(input$delete_selected_site,{
    req(nzchar(input$stored_site))
    key<-site_key(input$stored_site)
    showModal(modalDialog(title='Delete selected site permanently?',
      p(sprintf('Site %s will be removed from saved inventory and statistical results. Its stored field photos will also be removed.',key)),
      footer=tagList(modalButton('Cancel'),actionButton('confirm_delete_selected','Delete selected site',class='btn-danger')),easyClose=TRUE))
  })
  observeEvent(input$confirm_delete_selected,{
    removeModal()
    key<-site_key(input$stored_site)
    path<-record_path(key)
    if(file.exists(path))unlink(path)
    photos<-file.path(record_dir,'photos',key)
    if(dir.exists(photos))unlink(photos,recursive=TRUE)
    clear_workspace();refresh_sites()
    showNotification(sprintf('Site %s deleted from saved records and results.',key),type='warning',duration=8)
  })
  observeEvent(input$delete_all_sites,{
    showModal(modalDialog(title='Delete ALL saved assessments?',
      p('This permanently deletes all saved site JSON records and stored field photographs in this application installation. Multi-site inventory, dashboard, statistics and PCA will become empty. This cannot be undone without a backup.'),
      checkboxInput('ack_delete_all','I understand this deletes all saved site data',FALSE),
      footer=tagList(modalButton('Cancel'),actionButton('confirm_delete_all','Permanently delete all sites',class='btn-danger')),easyClose=FALSE))
  })
  observeEvent(input$confirm_delete_all,{
    if(!isTRUE(input$ack_delete_all)){showNotification('Tick the confirmation box before deleting.',type='error');return()}
    removeModal()
    files<-list.files(record_dir,pattern='\\.json$',full.names=TRUE)
    if(length(files))unlink(files)
    photos<-file.path(record_dir,'photos')
    if(dir.exists(photos))unlink(photos,recursive=TRUE)
    clear_workspace();refresh_sites()
    showNotification('All saved sites and associated results deleted. You can now import a new dataset.',type='warning',duration=10)
  })
  refresh_sites()
  # Use the already validated, consistently shaped scoring catalogue.
  catalog <- score_catalog
  catalog$caution <- c(as.character(kumar$source_note), as.character(eco$caution))
  catalog$mode <- rep('rating', nrow(catalog))
  groups <- list(entire=1:7,herb=8:17,woody=18:25,eco=26:37)
  labels <- c(entire='Entire vegetation',herb='Herbaceous component',woody='Woody perennials',eco='Ecosystem functional integrity')
  for(p in names(groups)) local({
    prefix<-p
    ids<-groups[[prefix]]
    selected<-reactive({req(input[[paste0(prefix,'_id')]]);catalog[match(as.integer(input[[paste0(prefix,'_id')]]),catalog$id),,drop=FALSE]})
         # Track the indicator whose radio input is currently on screen. This is
     # essential: a dynamically rendered radio input has no per-indicator ID.
     active_id <- reactiveVal(NA_integer_)
     save_active <- function(show_errors=TRUE) {
       id <- isolate(active_id())
       if(length(id)!=1L || is.na(id))return(FALSE)
       r <- catalog[match(id,catalog$id),,drop=FALSE]
       rating <- suppressWarnings(as.integer(isolate(input[[paste0(prefix,'_score')]])))
       labs <- unlist(r[1,paste0('rating_',1:5)],use.names=FALSE)
       valid <- which(!is.na(labs)&nzchar(trimws(labs)))
       if(length(rating)!=1L || is.na(rating) || !(rating %in% valid)) {
         if(show_errors)showNotification('Choose a valid rating before continuing.',type='warning')
         return(FALSE)
       }
       x<-isolate(saved());x<-x[x$id!=id,,drop=FALSE]
       x<-rbind(x,data.frame(id=id,rating=rating,
         measurement=if(is.null(isolate(input[[paste0(prefix,'_measurement')]])))'' else isolate(input[[paste0(prefix,'_measurement')]]),
         evidence=if(is.null(isolate(input[[paste0(prefix,'_evidence')]])))'' else isolate(input[[paste0(prefix,'_evidence')]]),stringsAsFactors=FALSE))
       saved(x[order(x$id),,drop=FALSE])
       if(nzchar(site_key(isolate(input$site_id))))write_site()
       TRUE
     }
     move_to <- function(next_id) {
       if(next_id %in% ids)updateSelectInput(session,paste0(prefix,'_id'),selected=as.character(next_id))
     }
     next_component <- function() {
       destination<-switch(prefix,entire='herb',herb='woody',woody='eco',eco='status')
       if(!is.null(destination))updateTabsetPanel(session,'tabs',selected=destination)
     }
     observeEvent(input[[paste0(prefix,'_id')]],{
       new_id<-as.integer(input[[paste0(prefix,'_id')]])
       old_id<-isolate(active_id())
       if(length(old_id)==1L && !is.na(old_id) && old_id!=new_id)save_active(FALSE)
       active_id(new_id)
     },priority=100)
     observeEvent(input[[paste0(prefix,'_prev')]],{
       if(!save_active())return()
       v<-as.integer(isolate(input[[paste0(prefix,'_id')]]))
       move_to(max(min(ids),v-1L))
     })
     observeEvent(input[[paste0(prefix,'_next')]],{
       if(!save_active())return()
       v<-as.integer(isolate(input[[paste0(prefix,'_id')]]))
       if(v==max(ids))next_component() else move_to(v+1L)
     })
     observeEvent(input[[paste0(prefix,'_unrated')]],{
       save_active(FALSE)
       done<-isolate(saved())$id[!is.na(isolate(saved())$rating)]
       left<-setdiff(ids,done)
       if(length(left))move_to(left[1]) else next_component()
     })
output[[paste0(prefix,'_live')]]<-renderUI({r<-selected();rating<-suppressWarnings(as.integer(input[[paste0(prefix,'_score')]]));if(length(rating)==1&&!is.na(rating))div(class='stat',paste0('Selected degradation score: ',normalized_degradation(r$id, rating),' / 100 (press Save & next attribute to record)'))})
    observeEvent(input[[paste0(prefix,'_id')]],{r<-selected();x<-saved();j<-match(r$id,x$id);updateTextInput(session,paste0(prefix,'_measurement'),value=if(is.na(j))'' else x$measurement[j]);updateTextAreaInput(session,paste0(prefix,'_evidence'),value=if(is.na(j))'' else x$evidence[j])})
    output[[paste0(prefix,'_title')]]<-renderText(selected()$attribute)
    output[[paste0(prefix,'_rating')]]<-renderUI({
      r<-selected(); if(r$mode=='context')return(div(class='note','Contextual variable: no degradation rating assigned. Enter the observed measurement and evidence.'))
      labs<-unlist(r[1,paste0('rating_',1:5)],use.names=FALSE)
      valid<-which(!is.na(labs)&nzchar(trimws(labs)))
      old<-saved();pos<-match(r$id,old$id)
      chosen<-if(!is.na(pos)&&!is.na(old$rating[pos]))as.character(old$rating[pos])else character(0)
      radioButtons(paste0(prefix,'_score'),'Select degradation rating',choices=setNames(as.character(valid),paste0('Rating ',valid,' — ',labs[valid])),selected=chosen)
    })
    output[[paste0(prefix,'_table')]]<-renderTable({
      r<-selected();data.frame(Rating=1:5,Description=unlist(r[1,paste0('rating_',1:5)],use.names=FALSE))
    },striped=TRUE)
    output[[paste0(prefix,'_image')]]<-renderUI({
      id<-selected()$id
      guide <- sprintf('attribute_guides/attribute_%02d.png',id)
      # Query-string revision prevents browser/RStudio Viewer from showing cached older guides.
      guide_url <- paste0(guide, '?rev=20261024-refresh2')
      if(file.exists(file.path('www',guide))) {
        return(tagList(
          h4('Attribute-specific illustrated field guide'),
          p('Use the rating options and descriptions above for the official app score. Illustrations are explanatory; some graphic labels or thresholds may differ from the application.', style='color:#8a4c10;font-weight:600;'),
          tags$a(href=guide_url,target='_blank',
                 tags$img(src=guide_url,style='width:100%;height:auto;border:1px solid #cbd9d3;border-radius:8px;',
                          alt=paste('Visual guide for attribute',id))),
        ))
      }
      NULL
    })
         observeEvent(input[[paste0(prefix,'_save')]],{
       if(!save_active())return()
       current<-as.integer(isolate(input[[paste0(prefix,'_id')]]))
       if(current==max(ids))next_component() else move_to(current+1L)
       showNotification('Rating saved; moving to next indicator.',type='message',duration=3)
     })
observeEvent(input[[paste0(prefix,'_clear')]],{
      id<-selected()$id;x<-saved();saved(x[x$id!=id,,drop=FALSE]);if(nzchar(site_key(input$site_id)))write_site();showNotification('Selected assessment cleared')
    })
    output[[paste0(prefix,'_status')]]<-renderUI({
      x<-saved();vals<-x$rating[match(ids,x$id)];valid<-sum(!is.na(vals));applicable<-sum(catalog$mode[match(ids,catalog$id)]!='context')
      if(valid==0)return(div(class='stat','Unassessed: no rated indicators yet.'))
      div(class='stat',div(class='status',paste0(valid,' / ',applicable,' rated; observed component degradation ',sprintf('%.1f',mean(normalized_degradation(ids, vals),na.rm=TRUE)),' / 100')),
          p(if(valid<applicable)'Partial component assessment only; full habitat status remains pending.' else 'All component indicators assessed; degradation is an exploratory summary.'))
    })
  })
  combined<-reactive({
    merge(catalog[,c('id','attribute','mode')],saved(),by='id',all.x=TRUE,sort=TRUE)
  })
  # Select a saved site for the dashboard without overwriting current unsaved ratings.
  observe({
    site_revision()
    fs <- list.files(record_dir,pattern='\\.json$',full.names=TRUE)
    ids <- vapply(fs,function(f)tryCatch(as.character(jsonlite::read_json(f,simplifyVector=TRUE)$metadata$site_id),error=function(e)''),character(1))
    ids <- ids[nzchar(ids)]
    choices <- c('Current working assessment'='__current__',setNames(ids,ids))
    previous <- isolate(input$dashboard_site)
    if(is.null(previous)||!previous %in% unname(choices))previous <- '__current__'
    updateSelectInput(session,'dashboard_site',choices=choices,selected=previous)
  })
  dashboard_records <- reactive({
    id <- input$dashboard_site
    if(is.null(id)||identical(id,'__current__'))return(combined())
    path <- record_path(id)
    validate(need(file.exists(path),'Selected site record is unavailable.'))
    obj <- tryCatch(jsonlite::read_json(path,simplifyVector=TRUE),error=function(e)NULL)
    validate(need(!is.null(obj),'Unable to read selected site record.'))
    rr <- clean_records(obj$ratings)
    x <- catalog
    x$rating <- rr$rating[match(x$id,rr$id)]
    x$measurement <- rr$measurement[match(x$id,rr$id)]
    x$evidence <- rr$evidence[match(x$id,rr$id)]
    x
  })
  output$dashboard_site_notice <- renderUI({
    if(is.null(input$dashboard_site)||identical(input$dashboard_site,'__current__'))return(NULL)
    div(class='note',paste('Viewing saved site:',input$dashboard_site,'— select Current working assessment to return to the live field form.'))
  })
  output$equation_values <- renderUI({
    x <- dashboard_records()
    comp <- lapply(groups, function(ids) {
      r <- x$rating[match(ids,x$id)]
      if(anyNA(r)) return(NA_real_)
      component_degradation(ids, r)
    })
    n <- sum(!is.na(x$rating))
    if(n != 37L) return(div(class='note',paste0('Calculated result pending: ',n,' / 37 ratings recorded. Component equations are shown above.')))
    vals <- unlist(comp,use.names=TRUE)
    hqi <- mean(vals)
    div(class='stat',h4('Substituted final 37-attribute calculation'),
        div(class='math-panel',HTML(sprintf('\\[HQI_{37}=\\frac{D_E+D_H+D_W+D_F}{4}=\\frac{%.2f+%.2f+%.2f+%.2f}{4}=%.2f\\]',vals[1],vals[2],vals[3],vals[4],hqi))),
        p('Component order: entire vegetation (7), herbaceous vegetation (10), woody perennials (8), ecosystem functional integrity (12).'))
  })
  output$final_degradation_class <- renderUI({
    x <- dashboard_records()
    if(sum(!is.na(x$rating)) != 37L) return(NULL)
    q <- vapply(groups, function(ids) component_degradation(ids, x$rating[match(ids,x$id)]), numeric(1))
    if(any(!is.finite(q))) return(NULL)
    hqi <- mean(q)
    div(class='stat',
      h3('Final site class based directly on HQI'),
      tags$p(style='font-size:20px;',tags$strong(sprintf('HQI: %.2f / 100',hqi))),
      tags$p(style='font-size:23px;color:#185333;font-weight:800;',
             paste('Final degradation class:',site_degradation_class(hqi))),
      tags$p('Higher HQI directly denotes more severe degradation; no subtraction is applied.'),
      tags$table(class='table table-bordered table-condensed',
        tags$thead(tags$tr(tags$th('HQI score'),tags$th('Class'))),
        tags$tbody(
          tags$tr(tags$td('0–25'),tags$td('None')),
          tags$tr(tags$td('>25–50'),tags$td('Slight')),
          tags$tr(tags$td('>50–70'),tags$td('Moderate')),
          tags$tr(tags$td('>70–90'),tags$td('Severe')),
          tags$tr(tags$td('>90–100'),tags$td('Very severe'))
        )
      ),
      tags$small('Higher HQI now denotes greater degradation. The bands are user-specified and require ecological validation.')
    )
  })
  output$scorecard<-renderTable(dashboard_records(),striped=TRUE)
  output$dashboard<-renderUI({
    x<-combined();out<-lapply(names(groups),function(p){
      ids<-groups[[p]];r<-x[x$id%in%ids,,drop=FALSE];expected<-sum(r$mode!='context');rated<-sum(!is.na(r$rating));
      label<-if(rated==0)'Not assessed' else paste0(sprintf('%.1f',mean(normalized_degradation(r$id, r$rating),na.rm=TRUE)),' / 100 observed degradation (',rated,'/',expected,' assessed)')
      div(class='stat',h4(labels[[p]]),p(label))
    })
    if(all(!is.na(x$rating)) && nrow(x)==37){
      component_q <- vapply(groups,function(ids)component_degradation(ids, x$rating[match(ids,x$id)]),numeric(1))
      hqi <- mean(component_q)
      quality_class <- site_degradation_class(hqi)
      out<-c(out,list(div(class='stat',h3(sprintf('Integrated habitat degradation (HQI): %.1f / 100 | %s (provisional)',hqi,quality_class)),
        tags$div(style='background:#dce6e0;height:20px;border-radius:10px;overflow:hidden;',tags$div(style=paste0('height:20px;width:',sprintf('%.1f',hqi),'%;background:#347d64;'))),
        p('Four ecological components contribute equally (25% each). Interpretive bands require field validation.'))))
    }else{out<-c(out,list(div(class='note',paste0('Integrated habitat degradation HQI pending: ',sum(!is.na(x$rating)),' of 37 ratings completed. Component summaries above are partial where indicated.'))))}
    do.call(tagList,out)
  })
  output$global_progress <- renderUI({
    x<-saved();n<-sum(!is.na(x$rating));
    div(class='stat',strong(sprintf('Assessment progress: %d / 37 indicators (%.0f%%)',n,100*n/37)),
      tags$div(style='background:#d9e6df;height:14px;border-radius:7px;overflow:hidden;',tags$div(style=sprintf('width:%.2f%%;height:14px;background:#267d57',100*n/37))),
      tags$div(style='display:flex;gap:12px;flex-wrap:wrap;margin-top:8px;',lapply(names(groups),function(g){v<-x$rating[match(groups[[g]],x$id)];tags$span(sprintf('%s: %d/%d',labels[[g]],sum(!is.na(v)),length(v)))})))
  })
  output$component_plot <- renderPlot({
    x<-dashboard_records();vals<-vapply(groups,function(ids){v<-x$rating[match(ids,x$id)];if(anyNA(v))return(NA_real_);component_degradation(ids, v)},numeric(1));
    par(mar=c(7,4,2,1));barplot(replace(vals,is.na(vals),0),names.arg=c('Entire','Herbaceous','Woody','Functional'),las=2,ylim=c(0,100),col=c('#4d9270','#9eb65a','#b38d62','#668bb5'),ylab='Observed degradation / 100');abline(h=seq(0,100,20),col='#eeeeee',lty=3)
  })
  inventory_data <- reactive({
    site_revision();files<-list.files(record_dir,pattern='\\.json$',full.names=TRUE)
    if(!length(files))return(data.frame())
    rows<-lapply(files,function(f)tryCatch({obj<-jsonlite::read_json(f,simplifyVector=TRUE);m<-obj$metadata;r<-clean_records(obj$ratings);q<-vapply(groups,function(ids){v<-r$rating[match(ids,r$id)];if(anyNA(v))NA_real_ else component_degradation(ids, v)},numeric(1));data.frame(site_id=as.character(m$site_id),site=as.character(m$site),date=as.character(m$date),latitude=if(is.null(m$latitude))NA_real_ else as.numeric(m$latitude),longitude=if(is.null(m$longitude))NA_real_ else as.numeric(m$longitude),rated=sum(!is.na(r$rating)),entire=q[1],herbaceous=q[2],woody=q[3],functional=q[4],HQI=if(anyNA(q))NA_real_ else mean(q),stringsAsFactors=FALSE)},error=function(e)NULL))
    out<-Filter(Negate(is.null),rows)
    if(!length(out))return(data.frame())
    result<-do.call(rbind,out)
    ids<-trimws(as.character(result$site_id))
    numeric_id<-suppressWarnings(as.numeric(ids))
    result<-result[order(is.na(numeric_id),numeric_id,tolower(ids),na.last=TRUE),,drop=FALSE]
    rownames(result)<-NULL
    result$site_class <- site_degradation_class(result$HQI)
    result
  })
  output$inventory <- renderTable(inventory_data(),digits=1,striped=TRUE)
  output$inventory_plot <- renderPlot({d<-inventory_data();validate(need(nrow(d)>0,'Save a site to display comparisons'));m<-as.matrix(d[,c('entire','herbaceous','woody','functional'),drop=FALSE]);m[is.na(m)]<-0;par(mar=c(7,4,2,1));barplot(t(m),beside=TRUE,names.arg=d$site_id,las=2,col=c('#4d9270','#9eb65a','#b38d62','#668bb5'),ylim=c(0,100),ylab='Degradation / 100');legend('topright',legend=c('Entire','Herbaceous','Woody','Functional'),fill=c('#4d9270','#9eb65a','#b38d62','#668bb5'),cex=.8)})
  output$inventory_csv <- downloadHandler(filename=function()'AridHabQ_all_sites_inventory.csv',content=function(file)write.csv(inventory_data(),file,row.names=FALSE,na=''))
  output$template <- downloadHandler(filename=function()'AridHabQ_37_indicator_template.csv',content=function(file)write.csv(data.frame(id=catalog$id,attribute=catalog$attribute,rating=NA_integer_,measurement='',evidence=''),file,row.names=FALSE,na=''))
  output$download_json <- downloadHandler(filename=function()paste0('AridHabQ_',site_key(input$site_id),'_backup.json'),content=function(file)jsonlite::write_json(list(schema_version=1,scoring_version=SCORING_VERSION,metadata=metadata(),ratings=saved(),saved_at=as.character(Sys.time())),file,auto_unbox=TRUE,pretty=TRUE,na='null'))
  output$download<-downloadHandler(
    filename=function()paste0('AridHabQ_assessment_',Sys.Date(),'.csv'),
    content=function(file){
      x<-combined();x$site<-input$site;x$assessor<-input$assessor;x$date<-as.character(input$date)
      x$habitat<-input$habitat
      x$normalized_degradation<-normalized_degradation(x$id, x$rating)
      write.csv(x,file,row.names=FALSE,na='')
    }
  )

  # MULTI-SITE: retain original raw values for audit, reject unsupported categories.
  observe({
    opts <- setNames(as.character(catalog$id),paste0(catalog$id,' · ',catalog$attribute))
    updateSelectInput(session,'stat_attribute',choices=opts)
    updateSelectInput(session,'guide_attribute',choices=opts)
  })
  multi_parse <- reactive({
    req(input$bulk_csv)
    raw <- read.csv(input$bulk_csv$datapath,check.names=FALSE,stringsAsFactors=FALSE,
                    na.strings=c('','NA','N/A'))
    validate(need(ncol(raw)>=38,'Need Site ID plus 37 indicator columns.'),
             need(nrow(raw)>0,'CSV has no site rows.'))
    if(ncol(raw)>38) raw <- raw[,seq_len(38),drop=FALSE]
    validate(need(!anyNA(raw[[1]]) && all(nzchar(trimws(as.character(raw[[1]])))),
                  'Every row requires a nonempty Site ID.'),
             need(!anyDuplicated(as.character(raw[[1]])),'Duplicate Site IDs in uploaded CSV.'))
    ratings <- matrix(NA_integer_,nrow(raw),37)
    issues <- list()
    for(i in seq_len(nrow(raw))) for(j in 1:37){
      value <- trimws(as.character(raw[i,j+1]))
      if(is.na(raw[i,j+1]) || !nzchar(value)) next
      number <- suppressWarnings(as.numeric(value))
      opts <- unlist(catalog[match(j,catalog$id),paste0('rating_',1:5)],use.names=FALSE)
      allowed <- which(!is.na(opts) & nzchar(trimws(opts)))
      if(!is.finite(number) || number!=round(number) || !number %in% allowed){
        issues[[length(issues)+1L]] <- data.frame(site_id=as.character(raw[i,1]),
          attribute_id=j,attribute=catalog$attribute[match(j,catalog$id)],
          raw_value=value,allowed=paste(allowed,collapse=', '),stringsAsFactors=FALSE)
      }else ratings[i,j] <- as.integer(number)
    }
    issue_table <- if(length(issues))do.call(rbind,issues) else
      data.frame(site_id=character(),attribute_id=integer(),attribute=character(),raw_value=character(),allowed=character())
    rownames(ratings) <- as.character(raw[[1]])
    colnames(ratings) <- paste0('A',1:37)
    list(raw=raw,ratings=ratings,issues=issue_table)
  })
  output$bulk_preview <- renderTable({x<-multi_parse();data.frame(site_id=rownames(x$ratings),
    valid_ratings=rowSums(!is.na(x$ratings)),missing_or_invalid=37-rowSums(!is.na(x$ratings)))},
    striped=TRUE)
  output$bulk_issues <- renderTable({head(multi_parse()$issues,100)},striped=TRUE)
  output$bulk_issues_csv <- downloadHandler(filename=function()'AridHabQ_invalid_ratings.csv',
    content=function(file)write.csv(multi_parse()$issues,file,row.names=FALSE))
  output$bulk_template <- downloadHandler(filename=function()'AridHabQ_multisite_template.csv',
    content=function(file){x<-as.data.frame(matrix(NA_integer_,nrow=1,ncol=37));names(x)<-paste0('A',1:37);x<-cbind('Site ID'='EXAMPLE_SITE',x);write.csv(x,file,row.names=FALSE,na='')})
  observeEvent(input$bulk_save,{
    req(isTRUE(input$bulk_confirm))
    tryCatch({
      x<-multi_parse(); keys<-vapply(rownames(x$ratings),site_key,character(1))
      if(anyDuplicated(keys))stop('Site IDs collide after filename sanitization.')
      for(i in seq_len(nrow(x$ratings))){
        rr<-data.frame(id=1:37,rating=as.integer(x$ratings[i,]),measurement='',evidence='',stringsAsFactors=FALSE)
        obj<-list(schema_version=1,scoring_version=SCORING_VERSION,metadata=list(site_id=rownames(x$ratings)[i],site=rownames(x$ratings)[i],
          assessor='',date=as.character(Sys.Date()),habitat='',latitude=NA_real_,longitude=NA_real_,
          site_note='Bulk imported; unsupported ratings omitted. Review original field data.'),
          ratings=rr,saved_at=as.character(Sys.time()))
        dest<-record_path(keys[i]);tmp<-tempfile(tmpdir=record_dir,fileext='.json')
        jsonlite::write_json(obj,tmp,auto_unbox=TRUE,pretty=TRUE,na='null')
        if(!file.rename(tmp,dest)){file.copy(tmp,dest,overwrite=TRUE);unlink(tmp)}
      }
      refresh_sites()
      updateSelectInput(session,'dashboard_site',selected=rownames(x$ratings)[1])
      showNotification(sprintf('Imported %d sites; %d invalid ratings flagged as missing.',nrow(x$ratings),nrow(x$issues)),type='message',duration=10)
    },error=function(e)showNotification(conditionMessage(e),type='error',duration=10))
  })
  # Read current persisted site inventory, including records imported in previous visits.
  all_ratings <- reactive({
    site_revision()
    files<-list.files(record_dir,pattern='\\.json$',full.names=TRUE)
    if(!length(files))return(matrix(numeric(),nrow=0,ncol=37,dimnames=list(NULL,paste0('A',1:37))))
    result<-lapply(files,function(f)tryCatch({
      o<-jsonlite::read_json(f,simplifyVector=TRUE)
      rr<-clean_records(o$ratings)
      z<-rep(NA_real_,37);z[rr$id]<-rr$rating
      list(id=as.character(o$metadata$site_id),rating=z)
    },error=function(e)NULL))
    result<-Filter(Negate(is.null),result)
    if(!length(result))return(matrix(numeric(),nrow=0,ncol=37))
    mat<-do.call(rbind,lapply(result,`[[`,'rating'))
    rownames(mat)<-make.unique(vapply(result,`[[`,character(1),'id'))
    colnames(mat)<-paste0('A',1:37)
    mat
  })
  output$bulk_inventory<-renderTable(inventory_data(),striped=TRUE,digits=2)
  output$bulk_inventory_csv<-downloadHandler(filename=function()'AridHabQ_multisite_inventory.csv',
    content=function(file)write.csv(inventory_data(),file,row.names=FALSE,na=''))
  observe({
    x<-all_ratings(); ids<-rownames(x)
    updateSelectInput(session,'group_sites_a',choices=ids,selected=intersect(isolate(input$group_sites_a),ids))
    updateSelectInput(session,'group_sites_b',choices=ids,selected=intersect(isolate(input$group_sites_b),ids))
  })
  distribution_summary <- function(v){
    v<-v[is.finite(v)];n<-length(v)
    if(!n)return(data.frame(n=0,mean=NA_real_,sd=NA_real_,skewness=NA_real_,excess_kurtosis=NA_real_,KS_p=NA_real_,KS_alpha_0_05=NA_character_,Shapiro_p=NA_real_))
    mu<-mean(v);sdv<-if(n>1)sd(v) else NA_real_
    skew<-kurt<-NA_real_
    if(n>=3 && is.finite(sdv) && sdv>0){z<-(v-mu)/sdv;skew<-mean(z^3);kurt<-mean(z^4)-3}
    # KS with fitted parameters is descriptive only; Shapiro-Wilk preferred for small samples.
    ks<-if(n>=5 && is.finite(sdv) && sdv>0)tryCatch(suppressWarnings(ks.test(v,'pnorm',mean=mu,sd=sdv)$p.value),error=function(e)NA_real_) else NA_real_
    sh<-if(n>=3 && n<=5000 && length(unique(v))>1)tryCatch(shapiro.test(v)$p.value,error=function(e)NA_real_) else NA_real_
    data.frame(n=n,mean=mu,sd=sdv,skewness=skew,excess_kurtosis=kurt,KS_p=ks,KS_alpha_0_05=if(is.na(ks)) "Not testable" else if(ks<0.05) "Reject normality" else "Do not reject",Shapiro_p=sh)
  }
  output$frequency_table<-renderTable({
    x<-all_ratings();req(nrow(x)>0,input$stat_attribute)
    v<-x[,as.integer(input$stat_attribute)];z<-tabulate(as.integer(v[!is.na(v)]),nbins=5)
    data.frame(rating=1:5,frequency=z,percent=round(100*z/max(1,sum(z)),2))
  })
  output$frequency_plot<-renderPlot({
    x<-all_ratings();validate(need(nrow(x)>0,'Import or save sites first.'))
    v<-x[,as.integer(input$stat_attribute)];barplot(tabulate(as.integer(v[!is.na(v)]),nbins=5),names.arg=1:5,
      col=c('#39875a','#89b77a','#f4d46a','#ed9753','#c94e4e'),xlab='Degradation rating',ylab='Number of sites')
  })
  output$distribution_statistics<-renderTable({
    x<-all_ratings();req(nrow(x)>0,input$stat_attribute)
    cbind(attribute=as.integer(input$stat_attribute),distribution_summary(x[,as.integer(input$stat_attribute)]))
  },digits=4)
  output$group_test<-renderTable({
    x<-all_ratings();req(input$stat_attribute)
    a<-intersect(input$group_sites_a,rownames(x));b<-intersect(input$group_sites_b,rownames(x))
    validate(need(length(intersect(a,b))==0,'Groups must not overlap.'),
      need(length(a)>=2 && length(b)>=2,'Select at least two sites in each group.'))
    v1<-na.omit(x[a,as.integer(input$stat_attribute)]);v2<-na.omit(x[b,as.integer(input$stat_attribute)])
    validate(need(length(v1)>=2 && length(v2)>=2,'At least two valid ratings required per group.'))
    t_p<-tryCatch(t.test(v1,v2)$p.value,error=function(e)NA_real_)
    w_p<-tryCatch(wilcox.test(v1,v2,exact=FALSE)$p.value,error=function(e)NA_real_)
    data.frame(group_A_n=length(v1),group_B_n=length(v2),mean_A=mean(v1),mean_B=mean(v2),
      Welch_t_p=t_p,Mann_Whitney_p=w_p,alpha=0.05)
  },digits=4)
  output$all_stats_csv<-downloadHandler(filename=function()'AridHabQ_37_attribute_statistics.csv',content=function(file){
    x<-all_ratings();y<-do.call(rbind,lapply(1:37,function(j)cbind(attribute_id=j,attribute=catalog$attribute[j],distribution_summary(x[,j]))));write.csv(y,file,row.names=FALSE,na='')
  })
  pca_object<-reactive({
    x<-all_ratings();x<-x[complete.cases(x),,drop=FALSE]
    validate(need(nrow(x)>=4,'PCA requires at least four complete sites.'),
      need(ncol(x)==37,'37 attributes required.'))
    varying<-apply(x,2,sd)>0
    validate(need(sum(varying)>=2,'At least two varying attributes required.'))
    fit<-prcomp(x[,varying,drop=FALSE],center=TRUE,scale.=TRUE)
    list(fit=fit,sites=rownames(x),attributes=which(varying),ratings=x)
  })
  output$pca_interactive_ui <- renderUI({
    if(requireNamespace('plotly',quietly=TRUE))plotly::plotlyOutput('pca_biplot_interactive',height='620px')
    else div(class='note','Install plotly for interactive zoom, hover, and vector biplot: install.packages("plotly"). The static PCA plots remain available below.')
  })
  if(requireNamespace('plotly',quietly=TRUE)){
    output$pca_biplot_interactive <- plotly::renderPlotly({
      z<-pca_object();fit<-z$fit
      validate(need(ncol(fit$x)>=2,'At least two PCs required.'))
      v<-fit$sdev^2;pct<-100*v/sum(v)
      scores<-fit$x[,1:2,drop=FALSE]
      ld<-fit$rotation[,1:2,drop=FALSE]
      # Scale eigenvectors to the site-score coordinate system.
      base_scale<-min(diff(range(scores[,1]))/max(diff(range(ld[,1])),1e-9),diff(range(scores[,2]))/max(diff(range(ld[,2])),1e-9))*.38
      scale<-base_scale*input$pca_arrow_scale/2
      plot<-plotly::plot_ly(x=scores[,1],y=scores[,2],type='scatter',mode='markers+text',
        text=z$sites,textposition='top center',hovertext=paste('Site:',z$sites),hoverinfo='text',
        marker=list(size=9,color='#28778c'),name='Sites')
      if(isTRUE(input$pca_show_arrows)){
        for(k in seq_len(nrow(ld))){
          xend<-ld[k,1]*scale;yend<-ld[k,2]*scale
          plot<-plotly::add_trace(plot,x=c(0,xend),y=c(0,yend),type='scatter',mode='lines',
            line=list(color='#b05c39',width=1.5),name=paste0('A',z$attributes[k]),showlegend=FALSE,
            hovertext=paste0('A',z$attributes[k],': ',catalog$attribute[z$attributes[k]],
              '<br>PC1 loading: ',round(ld[k,1],3),'<br>PC2 loading: ',round(ld[k,2],3)),hoverinfo='text',inherit=FALSE)
          plot<-plotly::add_annotations(plot,x=xend,y=yend,text=paste0('A',z$attributes[k]),showarrow=FALSE,
            font=list(size=9,color='#a34b2a'))
        }
      }
      plotly::layout(plot,title='Interactive PCA biplot — site scores and attribute eigenvectors',
        xaxis=list(title=sprintf('PC1 (%.1f%% variance)',pct[1]),zeroline=TRUE),
        yaxis=list(title=sprintf('PC2 (%.1f%% variance)',pct[2]),zeroline=TRUE),
        hovermode='closest')
    })
  }
  pca_eigen_table <- reactive({z<-pca_object();ev<-z$fit$sdev^2;data.frame(PC=paste0('PC',seq_along(ev)),eigenvalue=ev,variance_percent=100*ev/sum(ev),cumulative_percent=cumsum(100*ev/sum(ev)))})
  pca_loading_data <- reactive({z<-pca_object();data.frame(attribute_id=z$attributes,attribute=catalog$attribute[z$attributes],z$fit$rotation,check.names=FALSE)})
  output$pca_loading_table<-renderTable(pca_loading_data(),digits=4,striped=TRUE)
  output$pca_site_table<-renderTable({z<-pca_object();data.frame(site_id=z$sites,z$fit$x[,1:min(5,ncol(z$fit$x)),drop=FALSE])},digits=3,striped=TRUE)
  output$pca_contributions<-renderTable({z<-pca_object();rot<-z$fit$rotation[,1:2,drop=FALSE];data.frame(attribute_id=z$attributes,attribute=catalog$attribute[z$attributes],PC1_contribution_percent=100*rot[,1]^2,PC2_contribution_percent=100*rot[,2]^2)},digits=2,striped=TRUE)
  output$pca_loadings_csv<-downloadHandler(filename=function()'AridHabQ_PCA_loadings.csv',content=function(file)write.csv(pca_loading_data(),file,row.names=FALSE))
  output$pca_eigen_csv<-downloadHandler(filename=function()'AridHabQ_PCA_eigenvalues.csv',content=function(file)write.csv(pca_eigen_table(),file,row.names=FALSE))
  output$pca_sites<-renderPlot({
    z<-pca_object();v<-z$fit$x
    validate(need(ncol(v)>=2,'Two PCs required.'))
    site_colors<-rep('#286f85',nrow(v))
    if(identical(input$pca_color,'hqi')){q<-apply(z$ratings,1,function(r)mean(vapply(list(1:7,8:17,18:25,26:37),function(ids)component_degradation(ids, r[ids]),numeric(1))));site_colors<-ifelse(q<=25,'#267d57',ifelse(q<=50,'#75a56a',ifelse(q<=70,'#d0ad3d',ifelse(q<=90,'#db8b47','#bd4c43'))))}
    plot(v[,1],v[,2],pch=19,col=site_colors,xlab=sprintf('PC1 (%.1f%%)',100*z$fit$sdev[1]^2/sum(z$fit$sdev^2)),ylab=sprintf('PC2 (%.1f%%)',100*z$fit$sdev[2]^2/sum(z$fit$sdev^2)),main='Site ordination (complete cases)')
    text(v[,1],v[,2],labels=z$sites,pos=3,cex=.65)
    abline(h=0,v=0,col='grey80',lty=2)
  })
  output$pca_loadings<-renderPlot({
    z<-pca_object();v<-z$fit$rotation
    plot(v[,1],v[,2],pch=19,col='#bb6633',xlab='PC1 loading',ylab='PC2 loading',main='Attribute loadings')
    text(v[,1],v[,2],labels=paste0('A',z$attributes),pos=3,cex=.7)
    abline(h=0,v=0,col='grey80',lty=2)
  })
  output$pca_variance<-renderTable(pca_eigen_table(),digits=3,striped=TRUE)
  output$pca_scores_csv<-downloadHandler(filename=function()'AridHabQ_PCA_sites.csv',content=function(file){z<-pca_object();write.csv(data.frame(site_id=z$sites,z$fit$x),file,row.names=FALSE)})
  related_ids <- function(j){
    if(j<=7) c(1:7,26,28,30,35)
    else if(j<=17)c(8:17,31,32,34,36)
    else if(j<=25)c(18:25,29,31,33,34)
    else c(26:37,2,7,8,18)
  }
  output$guide_current<-renderUI({
    req(input$guide_attribute);j<-as.integer(input$guide_attribute)
    x<-saved();r<-x$rating[match(j,x$id)]
    div(class='stat',h4(catalog$attribute[j]),p(paste('Current rating:',if(length(r)&&!is.na(r))r else 'not assessed')),
      p('Use the field illustration and direct observation to select the correct class.'))
  })
  output$guide_related<-renderTable({
    req(input$guide_attribute);j<-as.integer(input$guide_attribute)
    ids<-setdiff(related_ids(j),j);x<-saved();v<-x$rating[match(ids,x$id)]
    data.frame(attribute_id=ids,attribute=catalog$attribute[ids],current_rating=v)
  },striped=TRUE)
  output$guide_suggestion<-renderUI({
    req(input$guide_attribute);j<-as.integer(input$guide_attribute)
    ids<-setdiff(related_ids(j),j);x<-saved();v<-x$rating[match(ids,x$id)]
    valid<-v[!is.na(v)]
    if(length(valid)<2)return(div(class='note','Rate at least two related attributes to receive a consistency prompt. No class is excluded.'))
    mid<-median(valid);range_text<-paste0(max(1,mid-1),'–',min(5,mid+1))
    div(class='note',strong('Nonbinding consistency prompt'),
      p(sprintf('Related attributes have median degradation rating %.1f. Consider checking whether the field evidence for this indicator supports a similar severity (approximately %s), or explains a different score.',mid,range_text)),
      p('This is NOT a predicted rating or a restriction. Category limits, microhabitat differences, seasonal timing and species-specific responses take precedence.'))
  })

}
shinyApp(ui,server)
