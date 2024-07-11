# 0) Use p_load to install if not present and load the packages ####
pacman::p_load(jsonlite,data.table,rmarkdown,knitr,RSQLite)

# 1) Set location where field mappings will be saved ####
mappings_dir<-"data/mappings"
if(!dir.exists(mappings_dir)){
  dir.create(mappings_dir)
}

# 2) Read in example json file #####
json_data <- fromJSON("data/qt_example.json")

# 3) Extract all fields and append columns for mapping links #####
# 3.1) Simple fields #####
json_names<-data.table(field_name=names(json_data),
                       class=sapply(json_data,base::class),
                       length=sapply(json_data,length))

simple_fields<-json_names[class!="data.frame" & length==1,field_name]
simple_fields<-json_data[json_names[class!="data.frame" & length==1,field_name]]


focal_name<-"input_json"

field_connections<-list()
field_connections$simple_fields<-data.table(focal_file = focal_name,
                              focal_path =names(simple_fields),
                              focal_class=json_names[class!="data.frame" & length==1,class],
                              focal_example_value=unlist(simple_fields))
# 3.2) Feed Items #####
path<-"feed_items"
data<-json_data[[path]]

field_connections[[path]]<-data.table(focal_file = focal_name,
                                   focal_path =file.path(path,colnames(data)),
                                   focal_class=apply(data,2,class),
                                   focal_example_value=unlist(apply(data,2,FUN=function(x){paste(unique(x),collapse="|")})))
# 3.3) Livestock #####
path<-"livestock"
data<-json_data[[path]]

field_connections[[path]]<-data.table(focal_file = focal_name,
                                        focal_path =file.path(path,colnames(data)),
                                        focal_class=apply(data,2,class),
                                        focal_example_value=unlist(apply(data,2,FUN=function(x){paste(unique(x),collapse="|")})))
# 3.4) Seasons #####
path<-"seasons"
data<-json_data[[path]]

field_connections[[path]]<-data.table(focal_file = focal_name,
                                      focal_path =file.path(path,colnames(data)),
                                      focal_class=apply(data,2,class),
                                      focal_example_value=unlist(apply(data,2,FUN=function(x){paste(unique(x),collapse="|")})))
# 3.5.1) Feed basket - season #####
path<-"feed_basket/season_name"
data<-json_data[[dirname(path)]][[basename(path)]]

field_connections[[path]]<-data.table(focal_file = focal_name,
                                      focal_path =path,
                                      focal_class=class(data),
                                      focal_example_value=paste(unique(data),collapse = "|"))
# 3.5.2) Feed basket - feeds #####
path<-"feed_basket/feeds"
data<-rbindlist(json_data[[dirname(path)]][[basename(path)]])
data<-data[,!"livestock"]

field_connections[[path]]<-data.table(focal_file = focal_name,
                                      focal_path =file.path(path,colnames(data)),
                                      focal_class=apply(data,2,class),
                                      focal_example_value=unlist(apply(data,2,FUN=function(x){paste(unique(x),collapse="|")})))
# 3.5.3) Feed basket - feeds - livestock #####
path<-"feed_basket/feeds/livestock"
data<-rbindlist(rbindlist(json_data[[strsplit(path,"/")[[1]][1]]][[strsplit(path,"/")[[1]][2]]])[[basename(path)]])

field_connections[[path]]<-data.table(focal_file = focal_name,
                                      focal_path =file.path(path,colnames(data)),
                                      focal_class=apply(data,2,class),
                                      focal_example_value=unlist(apply(data,2,FUN=function(x){paste(unique(x),collapse="|")})))
# 3.6) Fertilizer #####
path<-"fertilizer"
data<-json_data[[path]]

field_connections[[path]]<-data.table(focal_file = focal_name,
                                      focal_path =file.path(path,colnames(data)),
                                      focal_class=apply(data,2,class),
                                      focal_example_value=unlist(apply(data,2,FUN=function(x){paste(unique(x),collapse="|")})))

# 3.7) Merge tables and add fields #####
field_connections<-rbindlist(field_connections)

field_connections[,focal_userselected:= logical(), # Is the field user-selected in the UI?
                       ][,focal_free:=logical(), # Is the field free-text? (i.e. it is entered by the user and there is no lookup field)
                        ][,connected_file:= character(), # The filename of the connected object (focal_free==F)
                          ][,connected_path:= character(), # The path to the field in the connected object (focal_free==F)
                            ][,focal_keyfield:= character(), # The key field in the focal table (focal_free==F & focal_userselected==F)
                              ][,connected_keyfield:= character()] # The key field in the connected table (focal_free==F & focal_userselected==F)
# 4) Save results
fwrite(field_connections,file.path(mappings_dir,"input_mappings.csv"))

# Define the meta file for the field_connections table
field_connections_meta <- data.table(
  field_name = c("focal_file", "focal_path", "focal_example_value", "focal_class", "focal_userselected",
                 "focal_free","connected_file", "connected_path", "focal_keyfield", "connected_keyfield"),
  field_type = c("character", "character", "character", "character", "logical","logical",
                 "character", "character", "character", "character"),
  description = c("The filename of the object containing the focal field",
                  "The path to the focal field within the focal object",
                  "Example value(s) from the focal field",
                  "The class of data in the focal field",
                  "Is the field user-selected in the UI (TRUE/FALSE)?",
                  "Is the field free-text (TRUE/FALSE)? (i.e. it is entered by the user and there is no lookup field)",
                  "The filename of the connected object  (focal_free==F)",
                  "The path to the field in the connected object (focal_free==F)",
                  "The key field in the focal table  (focal_free==F & focal_userselected==F)",
                  "The key field in the connected table (focal_free==F & focal_userselected==F)")
)

fwrite(field_connections_meta,file.path(mappings_dir,"input_mappings_metadata.csv"))
