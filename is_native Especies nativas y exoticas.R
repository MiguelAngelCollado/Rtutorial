#This is a demo performed for Sevilla R users group

#fix plants in clean data

#Elena asked about IUCN data. You can retrive this data using taxize: https://github.com/ropensci/taxize
#Elena also suggested vectorbase can be scrapped. Maybe for the next hackaton?

#package OriginR
library(originr)

#define species (don't worry about typos)
sp <- c("Phytophthora remorum", "Apis mellifera", "carpobrotus edulis", "Lavandula stoechas",
        "Anchusa arvensis", "Rhododendron ponticum", "caulerpa taxifolia",
        "Anopheles gambiae", "Lantana camara", "aedes aegipty", "misgurnus anguillicaudatus")

#we load a wrapper for taxize functions that check for synonims, misspellings, etc...
source("https://gist.githubusercontent.com/ibartomeus/b61be59e317d0a1e213814b2f1a1c776/raw/6a676ce854a15a7428133d30ffbfbb2d719e06b3/clean_species")

#and clean data
sp2 <- clean_species(sp)
sp2
sp <- as.character(sp2$matched_name2)

#Is one of the worst invaders?
?gisd
gisd(sp, simplify = FALSE)
gisd(sp, simplify = TRUE)

#Invasive lists are incomplete by nature. Ley's flip the question
?is_native
is_native(sp[1], where = "Continental US", region = "america")
is_native(sp[2], where = "Continental US", region = "america")
lapply(sp[c(5,8)], is_native, where = "Continental US", region = "america")

is_native(sp[6], where = "Spain", region = "europe")
is_native(sp[6], where = "Britain", region = "europe")
lapply(sp, is_native, where = "Spain", region = "europe")

#is_native calls flora europaea function for plants
flora_europaea(sp)
sapply(sp, flora_europaea, simplify = FALSE)
?flora_europaea
#EXPLORE a webscrapping FUNCTION:
sp = "Anopheles gambiae"
#the workload is done by packages httr and xml2
library(httr)
library(xml2)
flora_europaea
#---inside the function flora_europaea
    #First we get the url and the parameters needed
    genus <- strsplit(sp, " ")[[1]][1]
    species <- strsplit(sp, " ")[[1]][2]
    url <- "http://rbg-web2.rbge.org.uk/cgi-bin/nph-readbtree.pl/feout"
    args <- list(FAMILY_XREF = "", GENUS_XREF = genus, SPECIES_XREF = species,
                 TAXON_NAME_XREF = "", RANK = "")
    mssg(verbose, paste("Checking", sp))
    url_check <- GET(url, query = args)
    warn_for_status(url_check)
    #then we fetch the url html code
    doc <- xml2::read_html(content(url_check, "text", encoding = "UTF-8"),
                           encoding = "UTF-8")
    tables <- xml2::xml_find_all(doc, "//table")
    #here we already retrieved the html code (doc) and tables (tables) and
    #we know that the info we want is on the 3rd table
    if (length(tables) < 3) {
        mssg(verbose, "Species not found")
        NULL
    }
    else {
        text <- xml_text(tables[[3]], trim = FALSE)
        #here we use regular expresions to extract just the Distribution data
        #it's long because you need to separate native, alien,
        #doubtful and extinct distributions see ?grep for info on how
        #regular expresions work.
        if (!grepl("Distribution:", text, perl = TRUE)) {
            mssg(verbose, "Species with no distribution. Probably not native.")
        }
        else {
            m_nat <- regexpr("Distribution: [A-Za-z ()?*%,]*",
                             text, perl = TRUE)
            distr_nat <- regmatches(text, m_nat)
            distr_status <- regmatches(distr_nat, gregexpr("[*][A-Z][a-z]",
                                                           distr_nat, perl = TRUE))
            distr_occ <- regmatches(distr_nat, gregexpr("[?][A-Z][a-z]",
                                                        distr_nat, perl = TRUE))
            distr_ext <- regmatches(distr_nat, gregexpr("[%][A-Z][a-z]",
                                                        distr_nat, perl = TRUE))
            distr_nat <- gsub(",", " ", distr_nat)
            distr_nat <- gsub("(", " ", distr_nat, fixed = TRUE)
            distr_nat <- gsub(")", "", distr_nat, fixed = TRUE)
            distr_nat <- gsub("Distribution: ", "", distr_nat)
            nat = exo = stat = oc = ex = NA
            if (distr_nat != "") {
                native <- strsplit(distr_nat, " ")[[1]]
                delete <- which(!native %in% country$short)
                if (length(delete) > 0)
                    native <- native[-delete]
                nat <- sapply(native, function(x) {
                    country[which(x == country$short), "long"]
                  #This just converts short country names to long ones based on a tesaurus
                  #The tesaurus is not included in this gist, see the package for it.
                })
            }
            if (length(distr_status[[1]]) > 0) {
                status <- gsub("*", "", distr_status[[1]], fixed = TRUE)
                stat <- sapply(status, function(x) {
                    country[which(x == country$short), "long"]
                })
            }
            if (length(distr_occ[[1]]) > 0) {
                occ <- gsub("?", "", distr_occ[[1]], fixed = TRUE)
                oc <- sapply(occ, function(x) {
                    country[which(x == country$short), "long"]
                })
            }
            if (length(distr_ext[[1]]) > 0) {
                ext <- gsub("%", "", distr_ext[[1]], fixed = TRUE)
                ex <- sapply(ext, function(x) {
                    country[which(x == country$short), "long"]
                })
            }
            m_ex <- regexpr("[[][A-Za-z ()?*%,]*", text, perl = TRUE)
            distr_exot <- regmatches(text, m_ex)
            if (length(distr_exot) > 0) {
                exotic <- strsplit(gsub("[", "", distr_exot,
                                        fixed = TRUE), " ")[[1]]
                exo <- sapply(exotic, function(x) {
                    country[which(x == country$short), "long"]
                })
            }
            #return all values.
            list(native = as.character(nat), exotic = as.character(exo),
                 status_doubtful = as.character(stat), occurrence_doubtful = as.character(oc),
                 extinct = as.character(ex))
        }
    }
}


