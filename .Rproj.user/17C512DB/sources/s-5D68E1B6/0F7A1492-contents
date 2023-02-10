


#' Create Template
#'
#' This function creates a template Contacts.csv file in the selected directory
#' (default = current directory).
#' The file Contacts.csv contains the project IDs and contact names
#' and emails that will be messaged by the script.
#'
#' @param string directory to save Contacts.csv. Default is current directory.
#' @export
template <- function(writedir = getwd()){
  file.copy(
    system.file('extdata','Contacts.csv', package = 'xnatNotify'),
    file.path(writedir,'Contacts.csv'))
}


#' @export
loadContacts <- function(file = NA){
    myfile <- UTILS.getFile(file)
    dat <- read.csv(myfile$path, stringsAsFactors = FALSE)
    return(dat)
}

#' XNAT Notify
#'
#' This function takes a prearchive location and a contact list. It searches
#' the prearchive for all projects found in the contact list. Then it scans
#' each project folder. It compiles a list of all scans there and their
#' file counts, and sends this information in email reports to all contacts.
#' An optional master email address will receive a report on all projects.
#'
#' @param prearch_dir prearchive directory that contains project folders.
#'     Default is current directory.
#' @param contacts data.frame or string. Either a data frame containing columns
#'     PROJECT, NAME, EMAIL, DO_FILECOUNT
#'     or a path to a csv file containing that information. Default is to open
#'     an interactive file selector to get the csv file.
#' @param master Optional master email contact. If present, they will receive
#'     an email containing all reports sent to contacts.
#' @export
xnatNotify <- function(prearch_dir = getwd(), contacts = NA, master = NA){
  if(class(contacts) != 'data.frame' && (is.na(contacts) || class(contacts)=='character')){
    contacts <- loadContacts(contacts)
  }

  projects <- PROCESS.getProjects(prearch_dir)
  if(!all(contacts$PROJECT %in% projects$studies)){
      warning('WARNING: The following projects are present in the Contact data,\n
               But are not located in the prearchive:\n',
           paste(contacts$PROJECT[!contacts$PROJECT %in% projects$studies], collapse = ', '))
  }

  plist <- contacts$PROJECT[contacts$PROJECT %in% projects$studies]
  if(length(plist) == 0){
    warning('No projects in the Contact data are present in the prearchive. Quitting.')
  } else {
    reports <- PROCESS.generateReports(prearch_dir, plist, contacts)
    PROCESS.mailReports(reports, contacts,master)
    return(reports)
  }
}
