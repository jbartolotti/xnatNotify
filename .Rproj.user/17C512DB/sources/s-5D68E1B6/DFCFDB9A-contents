

basedir_prearch <- '/xnatdata/prearch'

PROCESS.getProjects <- function(prearch_dir){
  prearch <- list()
  prearch$all <- dir(prearch_dir)
  prearch$clinical <- prearch$all[unlist(lapply(prearch$all, function(x){length(grep('_',x))>0}))]
  prearch$studies <- prearch$all[!(prearch$all %in% prearch$clinical)]
  return(prearch)
}

PROCESS.generateReports <- function(prearch_dir, plist, contacts){
  reports <- list()
  for(pp in plist){
    all_lines <- c(sprintf('Hello, %s',contacts$NAME[contacts$PROJECT == pp]),'',
                   sprintf('PROJECT ID: %s',pp),
                   sprintf('REPORT GENERATED ON: %s',Sys.time(),'')
                   )
    all_lines2 <- as.character()
    scans <- dir(file.path(prearch_dir,pp))
    delta_days <- unlist(lapply(scans,
                                function(x)
                                {
                                  (as.numeric(Sys.time()) -
                                     as.numeric(file.info(file.path(prearch_dir,pp,x))$ctime)
                                  ) / 60 / 60 / 24
                                }))
    week_scans <- scans[delta_days <= 8]

    if (length(week_scans) > 0)
    {
      all_lines = c(all_lines, '', sprintf('===ALL SCANS FOR PROJECT %s IN PREARCHIVE THAT WERE ADDED IN THE LAST WEEK===',pp))
      all_lines = c(all_lines,paste('scan on',week_scans,'for',pp),'')
    }
    if (length(scans) > 0)
    {
      all_lines2 = c(all_lines2, '', sprintf('===ALL SCANS FOR PROJECT %s CURRENTLY IN PREARCHIVE===',pp))
      all_lines2 = c(all_lines2,paste('scan on',scans,'for',pp),'')
    }

    reports[[as.character(pp)]] <- c(all_lines, all_lines2,'.')
  }

  return(reports)
}
PROCESS.mailReports <- function(reports, contacts, master){
  for(pp in names(reports)){
    try(sendmailR::sendmail('xnat_reporter@hbic-synapse2.kumc.edu',contacts$EMAIL[contacts$PROJECT == pp],'XNAT Report',c(reports[[pp]])),silent = TRUE)
  }
  if(!is.na(master)){
    master_report <- do.call('c',reports)
    try(sendmailR::sendmail('xnat_reporter@hbic-synapse2.kumc.edu',master,'XNAT Report',c(master_report)),silent = TRUE)

    }
}
