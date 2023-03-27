

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
    all_lines <- c(sprintf('Hello, %s',contacts$CONTACT[contacts$PROJECT == pp]),'',
                   sprintf('PROJECT ID: %s',pp),
                   sprintf('REPORT GENERATED ON: %s',Sys.time(),'')
                   )
    all_lines2 <- as.character()
    scans <- dir(file.path(prearch_dir,pp))
    scan_reports <- list()
    for(s in scans){
      scan_reports[[s]] <- getDicomCounts(prearch_dir, pp, s)
      }
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
      for(s in week_scans){
        all_lines <- c(all_lines, sprintf('Scan %s on %s. Dicom counts:',scan_reports[[s]][1], s), scan_reports[[s]][2:length(scan_reports[[s]])],'')
        }
#      all_lines = c(all_lines,paste('scan on',week_scans,'for',pp),'')
    }
    if (length(scans) > 0)
    {
      all_lines2 = c(all_lines2, '', sprintf('===ALL SCANS FOR PROJECT %s CURRENTLY IN PREARCHIVE===',pp))
      for(s in scans){
        all_lines2 <- c(all_lines2, sprintf('Scan %s on %s. Dicom counts:',scan_reports[[s]][1],s), scan_reports[[s]][2:length(scan_reports[[s]])],'')
      }

#      all_lines2 = c(all_lines2,paste('scan on',scans,'for',pp),'')
    }

    reports[[as.character(pp)]] <- c(all_lines, all_lines2)
  }

  return(reports)
}

PROCESS.mailReports <- function(reports, contacts, master){
  for(pp in names(reports)){
    try(sendmailR::sendmail('xnat_reporter@hbic-synapse2.kumc.edu',contacts$EMAIL[contacts$PROJECT == pp],'XNAT Report',c(reports[[pp]],'.')),silent = TRUE)
  }
  if(!is.na(master)){
    master_report <- do.call('c',reports)
    try(sendmailR::sendmail('xnat_reporter@hbic-synapse2.kumc.edu',master,'XNAT Report',c(master_report,'.')),silent = TRUE)

    }
}

getDicomCounts <- function(prearch_dir, project, scan){
  dcm_report <- as.character()
  scandir <- file.path(prearch_dir,project,scan)
  scandir_contents <- dir(scandir)
  tmpdir <- file.info(file.path(scandir,scandir_contents))$isdir
  if(sum(tmpdir)>0){ # Continue if there is a directory within scan directory
    scan_name <- scandir_contents[tmpdir]
    dcm_report <- scan_name
    rundir <- file.path(scandir,scan_name,'SCANS')
    if( file.exists(rundir) ){ #continue if there is a folder SCANS
      run_names <- dir(rundir)
      runcounts <- list()
      for(r in run_names){
        dv_names <- dir(file.path(rundir,r))
        for(dv in dv_names){
          filenames <- dir(file.path(rundir,r,dv))
          dicom_list <- filenames[grepl('[.]dcm',filenames)]
          runcounts[[r]] <- list()
          runcounts[[r]][[dv]] <- list(dcm = length(dicom_list), all = length(filenames))
          }
      }

      runcounts_list <- unlist(runcounts)
      runcounts_list_dcm <- runcounts_list[grepl('dcm',names(runcounts_list))]

      numerical_order <- order(as.numeric(
        unlist(lapply(names(runcounts_list_dcm),
                      function(x){strsplit(x,'[.]')[[1]][1]}
                      ))
        ))
      for(i in names(runcounts_list_dcm)[numerical_order]){
        dcm_report <- c(dcm_report,sprintf('%s: %s',i,runcounts_list_dcm[i]))

        }

    }

  }
  return(dcm_report)
  }

allscans <- list()
prearch_base <- '/xnatdata/prearch'
project_list <- c('147614')
for(p in project_list){
  scanlist <- dir(file.path(prearch_base, p))
  for(s in scanlist){
    sdir <- dir(file.path(prearch_base,p,s))
    isdir <- file.info(dir(file.path(prearch_base,p,s),full.names = TRUE))$isdir
    ss <- sdir[isdir]
    if(length(ss)>0){
      thisdf <- list()
      if (any(dir(file.path(prearch_base,p,s,ss))=='SCANS')){
        runs <- dir(file.path(prearch_base,p,s,ss,'SCANS'))
        for(r in runs){
          d <- 0
          sec <- 0
          thisdir <- dir(file.path(prearch_base,p,s,ss,'SCANS',r))
          if(any(thisdir=='DICOM')){
          d <- system(sprintf('ls -f %s | grep \'[.]dcm\' | wc -l ',file.path(prearch_base, p, s, ss, 'SCANS',r,'DICOM')), intern = TRUE)
          }
          if(any(thisdir=='secondary')){
          sec <- system(sprintf('ls -f %s | grep \'[.]dcm\' | wc -l ',file.path(prearch_base, p, s, ss, 'SCANS',r,'secondary')), intern = TRUE)
          }
          thisdf[[r]] <- data.frame(project = p, sub = s, run = r, dicomcount = d, secondary_dicomcount = sec)

        }

      }
      allscans[[s]] <- do.call('rbind',thisdf)
    }

    }
}
allall <- do.call('rbind',allscans)

lines <- as.character()
myruns <- unique(allall$run)
for(r in myruns[order(as.numeric(myruns))]){
  subs <- allall$sub[allall$run == r]
  counts <- (as.numeric(allall$dicomcount[allall$run == r]))
  mymode <- which(tabulate(counts) == max(tabulate(counts)))
  notfull <- counts !=mymode
  if(any(notfull)){
    for(i in which(notfull)){
      lines <- c(lines,(sprintf('PROJ: %s, SUB: %s, RUN: %s, DICOM %s/%s ',147614, subs[i], r, counts[i], mymode)))
    }
    }
  }


a <- system(sprintf('ls -f %s | grep \'[.]dcm\' | wc -l ',file.path('/xnatdata/prearch','147614','20220718_191149864','147614_BB_1003_01','SCANS','1','DICOM')), intern = TRUE)

#########
#Get dicom counts from ku3T/rawdata
allscans <- list()
basedir <- '~/ku3T/rawdata'
project_list <- c('147614_BLUE')
subdirs <- list('147614_BLUE' = '147614')
for(p in project_list){
  scanlist <- dir(file.path(basedir, p))
  subscanlist <- scanlist[grepl(subdirs[[p]],scanlist)]
  for(s in subscanlist){
    sdir <- dir(file.path(basedir,p,s))

    if(any(sdir == 'ima')){
      thisdf <- list()
        runs <- dir(file.path(basedir,p,s,'ima'))
        tmp <- dir(file.path(basedir,p,s,'ima'),full.names = TRUE)
        runs <- runs[file.info(tmp)$isdir]
        for(r in runs){
          d <- 0
            d <- as.numeric(system(sprintf('ls -f %s | grep \'[.]dcm\' | wc -l ',file.path(basedir, p, s, 'ima',r)), intern = TRUE))

            thisdf[[r]] <- data.frame(project = p, sub = s, run = r, dicomcount = d)


      }
      allscans[[s]] <- do.call('rbind',thisdf)
    }

  }
}
allall <- do.call('rbind',allscans)

replace_list <- list(
  "tgse_pcasl_11c2_seg2x2use" = "tgse_pcasl_11c2_seg2x2_use")

for(i in names(replace_list)){
  allall$run[allall$run==i] = replace_list[[i]]
}

for(rr in unique(allall$run)){



  }


