setwd("~/Documents/MDS/Projekte/CORD")

#########################################################################
###                            See script.r                           ###
###               Crack FHIR Resources using fhircrackr               ###
#########################################################################



####################################################################################################################
# To calculate the aggregate of patients corresponding to O80 and Z37.0!
#(Mukoviszidose/CF Cystic Fibrosis and Birth)
#####################################################################################################################
library(fhircrackr)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
options(warn=-1)# to suppress warnings
###################################################################################################################################################################################################################################################################
#Condition resource with patient resource in a bundle for ICD-10 GM Diagnoses Code O80, O80 Z37.0! (associated birth)
###############################################################################################################################################################################################################################################################
#O80 und Z37.0!

search_request <- paste0('https://mii-agiop-cord.life.uni-leipzig.de/fhir/', # HAPI FHIR server end point
                        'Condition?',# der resource typ zu suchen, Condition resource typ beinhaltet diagnose
                        'code=O80,O80%20Z37.0%21',# suche nach ICD-10 GM Codes. E84.0, E84.1, E84.8, E84.80, E84.87, E84.88, E84.9 
                        '&_include=Condition:subject:Patient') # hier könnte die Patienten oder Encounter Ressourcen ausgewählt werden


condition_patient_bundle <- fhir_search(request=search_request, max_bundles=Inf)

#condition_patient_bundle<- fhir_search("https://mii-agiop-cord.life.uni-leipzig.de/fhir/Condition?code=O80,O80%20Z37.0%21&_include=Condition:subject:Patient")

#############################################################################################################################################################################################
# Specify the columns of interest in design parameter including condition and patient resource
#############################################################################################################################################################################################

design_cond <- list(
	Conditions = list(
		"//Condition",
		cols       = list(
			C.CID  = "id",#condition id
			C.PID  = "subject/reference",# patient id
			C.SECODE = "code/coding/code",#attribute to address rare disease codes from tracer diagnose list
			C.DiagText1 ="category/text",# diagnoses text 1
			C.DiagText2 ="code/text" # diagnoses text 2, it is here the description of diagnoses is captured and assigned
		),
		style = list(
			sep = "/",
			brackets = NULL,
			rm_empty_cols = FALSE
		)
	),
	Patients = list(
		"//Patient",
		list(
			P.PID        = "id",# patient id
			P.SOURCE="meta/source",#the  integration center id
			P.GESCHLECHT  = "gender",# patient gender to be replaced later
			P.GEBD   = "birthDate",# birth date to calculate age bins
			P.PLZ = "address/postalCode"# plz
		),
		style = list(
			sep = "/",
			brackets = NULL,
			rm_empty_cols = FALSE
		)
	)
)

# To flatten the O80 and Z37.0! XML object bundles from patients and conditions to a list
list_cdn <- fhir_crack(condition_patient_bundle, design_cond, verbose = 0)
#To remove the "Patient/" tag from patient id in condition resource use string remove all

list_cdn$Conditions$C.PID <- str_remove_all(list_cdn$Conditions$C.PID,"Patient/")#updated because the patient id tag included the house source

# function to calculate age and join the condition resource to patient resource based on Patient id
#merge conditions and patient resources based on patient id similar to join
join_processing <- function( jn ) {
	jn$ALL <-
		merge(
			jn$Conditions,
			jn$Patients,
			by.x = "C.PID",
			by.y = "P.PID",
			all = T
		)
	jn$ALL$AGE <- round( as.double( as.Date( Sys.time() ) - as.Date( jn$ALL$P.GEBD ) ) / 365.25, 2 )
	jn
}

# after joining
list_dfcp <- join_processing(list_cdn)
###############################################################################################################################################################################################################################################################
# from all the O80 and O80 Z37.0! combination select only Cystic fibrosis associated with birth. that is Diagnoses text "CF-Geburt"
###############################################################################################################################################################################################################################################################
#list_dfcp$ALL <- list_dfcp$ALL[list_dfcp$ALL$C.DiagText2 == "CF-Geburt",]




#data frame object
# selected columns included
#list with condition and patients are filtered with the selectedcolumns

list_f <- list_dfcp$ALL[,c('P.SOURCE','C.SECODE','C.DiagText1','C.DiagText2','P.GESCHLECHT','AGE')]
######################################################################################################################################################################################################################################################################
# set of formatting
###############################################################################################################################################################################################################################################################
#include the extra slash for gsub to detect back slash and remove them from secondary codes
list_f$C.SECODE <- str_replace_all(list_f$C.SECODE,"/","//")
#remove the codes after decimal
list_f$C.SECODE <- gsub("([/]/.*)","",list_f$C.SECODE)

#replace the decimal with a comma as indicated by Josef in the use case result for e.g., E84,-
list_f$C.SECODE <- gsub("\\..*","\\1,-",list_f$C.SECODE)
#replace male with m and female with f
list_f$P.GESCHLECHT [list_f$P.GESCHLECHT == "female"] <- "f"
list_f$P.GESCHLECHT [list_f$P.GESCHLECHT == "male"] <- "m"
list_f$P.GESCHLECHT [list_f$P.GESCHLECHT == ""] <- "NA"
# remove the contents after hash (#) symbol in source to input content for einrichtungs identifikator
list_f$P.SOURCE <- gsub("#.*","\\1",list_f$P.SOURCE)#

names(list_f)[names(list_f)== "P.SOURCE"] <- "Einrichtungsidentifikator"
names(list_f)[names(list_f)== "C.SECODE"] <- "AngabeDiagn2"
names(list_f)[names(list_f)== "P.GESCHLECHT"] <- "AngabeGeschlecht"
names(list_f)[names(list_f)== "AGE"] <- "AngabeAlter"
#names(list_f)[names(list_f)== "C.DiagText1"] <- "TextDiagnose1"
names(list_f)[names(list_f)== "C.DiagText2"] <- "TextDiagnose2"




#########################################################################
###                        Little Formatations                        ###
#########################################################################

Input_C <- data.frame(id = seq(1,nrow(list_f)), list_f)
#Relevant: TextDiagnose2 == "CF-Geburt"
Input_C$outcome <- ifelse(Input_C$TextDiagnose2 == "CF-Geburt",1,0)




#########################################################################
###                         Upload to Opal                            ###
#########################################################################

set.seed(2093)
resC <- split(Input_C, sample(rep(1:2, (nrow(Input_C)/2))))

#Server 101
upload_opal(resC[[2]], "administrator", "datashield_test&", "http://192.168.56.101:8080", "CORD4", "BlockC")

#Server 100
upload_opal(resC[[1]], "administrator", "datashield_test&", "http://192.168.56.100:8080", "CORD4", "BlockC")

save(resC, file="DatenBlockC.RData")
