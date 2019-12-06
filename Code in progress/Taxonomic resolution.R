require(tidyverse)
require(readxl)
require(taxize)

#Edit function to enable it to search for non-marine taxa
get_wormsid_edit<-function(query, searchtype = "scientific", accepted = FALSE, 
                      ask = TRUE, messages = TRUE, rows = NA, ...) 
{
  assert(query, c("character", "taxon_state"))
  assert(searchtype, "character")
  assert(accepted, "logical")
  assert(ask, "logical")
  assert(messages, "logical")
  assert_rows(rows)
  if (inherits(query, "character")) {
    tstate <- taxon_state$new(class = "wormsid", names = query)
    items <- query
  }
  else {
    assert_state(query, "wormsid")
    tstate <- query
    query <- tstate$taxa_remaining()
    items <- c(query, tstate$taxa_completed())
  }
  prog <- progressor$new(items = items, suppress = !messages)
  done <- tstate$get()
  for (i in seq_along(done)) prog$completed(names(done)[i], 
                                            done[[i]]$att)
  prog$prog_start()
  for (i in seq_along(query)) {
    direct <- FALSE
    mssg(messages, "\nRetrieving data for taxon '", 
         query[i], "'\n")
    if (!searchtype %in% c("scientific", "common")) {
      stop("'searchtype' must be one of 'scientific' or 'common'", 
           call. = FALSE)
    }
    wmdf <- switch(searchtype, scientific = worms_worker(query[i], 
                                                         function(x) worrms::wm_records_name(x, marine_only = FALSE), rows, ...), common = worms_worker(query[i], 
                                                                                                                    worrms::wm_records_common, rows, ...))
    mm <- NROW(wmdf) > 1
    if (!inherits(wmdf, "tbl_df") || NROW(wmdf) == 
        0) {
      wmid <- NA_character_
      att <- "not found"
      status<- NA_character_
    }
    else {
      wmdf <- suppressWarnings(data.frame(wmdf))
      wmdf <- wmdf[, c("AphiaID", "scientificname", 
                       "authority", "status")]
      names(wmdf)[1] <- "id"
      if (accepted) {
        wmdf <- wmdf[wmdf$status %in% "accepted", 
                     ]
      }
      wmdf <- sub_rows(wmdf, rows)
      if (nrow(wmdf) == 0) {
        mssg(messages, m_not_found_sp_altclass)
        wmid <- NA_character_
        att <- "not found"
        status<-NA_character_
      }
      if (nrow(wmdf) == 1) {
        wmid <- wmdf$id
        att <- "found"
        status<-wmdf$status
      }
      if (nrow(wmdf) > 1) {
        names(wmdf)[grep("scientificname", names(wmdf))] <- "target"
        matchtmp <- wmdf[tolower(wmdf$target) %in% tolower(query[i]), 
                         "id"]
        if (length(matchtmp) == 1) {
          wmid <- matchtmp
          direct <- TRUE
          att <- "found"
          status<-wmdf[tolower(wmdf$target) %in% tolower(query[i]), 
                       "status"]
        }
        else {
          wmid <- NA_character_
          att <- "not found"
          status<-NA_character_
        }
      }
      if (any(nrow(wmdf) > 1 && is.na(wmid) | nrow(wmdf) > 
              1 && att == "found" & length(wmid) > 1)) {
        if (ask) {
          names(wmdf)[grep("scientificname", names(wmdf))] <- "target"
          wmdf <- wmdf[order(wmdf$target), ]
          message("\n\n")
          print(wmdf)
          message("\nMore than one WORMS ID found for taxon '", 
                  query[i], "'!\n\n                  Enter rownumber of taxon (other inputs will return 'NA'):\n")
          take <- scan(n = 1, quiet = TRUE, what = "raw")
          if (length(take) == 0) {
            take <- "notake"
            att <- "nothing chosen"
          }
          if (take %in% seq_len(nrow(wmdf))) {
            take <- as.numeric(take)
            message("Input accepted, took taxon '", 
                    as.character(wmdf$target[take]), "'.\n")
            wmid <- wmdf$id[take]
            att <- "found"
            status<-wmdf$status[take]
          }
          else {
            wmid <- NA_character_
            mssg(messages, "\nReturned 'NA'!\n\n")
            att <- "not found"
            status<-NA_character_
          }
        }
        else {
          if (length(wmid) != 1) {
            warning(sprintf(m_more_than_one_found, "Worms ID", 
                            query[i]), call. = FALSE)
            wmid <- NA_character_
            status<-NA_character_
            att <- m_na_ask_false
          }
        }
      }
    }
    res <- list(id = as.character(wmid), att = att, status=status, multiple = mm, 
                direct = direct)
    prog$completed(query[i], att)
    prog$prog(att)
    tstate$add(query[i], res)
  }
  out <- tstate$get()
  ids <- structure(pluck_un(out, "id", ""), class = "wormsid", 
                   match = pluck_un(out, "att", ""), status = pluck_un(out, "status", ""), multiple_matches = pluck_un(out, 
                                                                                 "multiple", logical(1)), pattern_match = pluck_un(out, 
                                                                                                                                   "direct", logical(1)))
  on.exit(prog$prog_summary(), add = TRUE)
  on.exit(tstate$exit, add = TRUE)
  add_uri(ids, get_url_templates$worms)
}

environment(get_wormsid_edit)<-environment(get_wormsid)

crosswalk <- read_excel("Data/new_crosswalk.xlsx", sheet = "Hierarchy2")

WID<-get_wormsid_edit(unique(crosswalk$Taxname), ask=FALSE)

WID_data<-tibble(WID=WID, Status=attr(WID, "status"), Taxname=unique(crosswalk$Taxname))

unique(crosswalk$Taxname)[which(is.na(WID))]

CLASS<-classification(WID)

WID_data<-tibble(WID=WID, Status=attr(WID, "status"), Taxname=unique(crosswalk$Taxname))

Taxonomy<-rbind(CLASS)%>%
  select(-id)%>%
  filter(name!="Gnathostomata")%>%
  spread(key="rank", value="name")%>%
  select(query, Phylum, Class, Order, Family, Genus, Species)%>%
  left_join
