decodetable <- local({
     con <- textConnection(
       "\"colname\"	\"colvalue\"	\"description\"
\"STUSAB\"		\"State/U.S. Abbreviation (USPS)\"
\"SUMLEV\"		\"Summary level\"
\"SUMLEV\"	\"10\"	\"United States\"
\"SUMLEV\"	\"20\"	\"Region\"
\"SUMLEV\"	\"30\"	\"Division\"
\"SUMLEV\"	\"40\"	\"State\"
\"SUMLEV\"	\"50\"	\"State-County\"
\"SUMLEV\"	\"60\"	\"State-County-County Subdivision\"
\"SUMLEV\"	\"70\"	\"State-County-County Subdivision-Place/Remainder\"
\"SUMLEV\"	\"160\"	\"State-Place\"
\"SUMLEV\"	\"155\"	\"State-Place-County\"
\"SUMLEV\"	\"170\"	\"State-Consolidated City\"
\"SUMLEV\"	\"172\"	\"State-Consolidated City-Place Within Consolidated City\"
\"SUMLEV\"	\"500\"	\"State Congressional District\"
\"SUMLEV\"	\"610\"	\"State-State Legislative District (Upper Chamber)\"
\"SUMLEV\"	\"620\"	\"State-State Legislative District (Lower Chamber)\"
\"GEOCOMP\"		\"Geographic Component\"
\"GEOCOMP\"	\"0\"	\"Not a geographic component\"
\"GEOCOMP\"	\"1\"	\"Urban\"
\"GEOCOMP\"	\"4\"	\"Urban--in urbanized area\"
\"GEOCOMP\"	\"5\"	\"Urban--in urbanized area of 5,000,000 or more population\"
\"GEOCOMP\"	\"6\"	\"Urban--in urbanized area of 2,500,000 to 4,999,999 population\"
\"GEOCOMP\"	\"7\"	\"Urban--in urbanized area of 1,000,000 to 2,499,999 population\"
\"GEOCOMP\"	\"8\"	\"Urban--in urbanized area of 500,000 to 999,999 population\"
\"GEOCOMP\"	\"9\"	\"Urban--in urbanized area of 250,000 to 499,999 population\"
\"GEOCOMP\"	\"10\"	\"Urban--in urbanized area of 100,000 to 249,999 population\"
\"GEOCOMP\"	\"11\"	\"Urban--in urbanized area of 50,000 to 99,999 population\"
\"GEOCOMP\"	\"28\"	\"Urban--in urban cluster\"
\"GEOCOMP\"	\"29\"	\"Urban--in urban cluster of 25,000 to 49,999 population\"
\"GEOCOMP\"	\"30\"	\"Urban--in urban cluster of 10,000 to 24,999 population\"
\"GEOCOMP\"	\"31\"	\"Urban--in urban cluster of 5,000 to 9,999 population\"
\"GEOCOMP\"	\"32\"	\"Urban--in urban cluster of 2,500 to 4,999 population\"
\"GEOCOMP\"	\"43\"	\"Rural\"
\"GEOCOMP\"	\"44\"	\"Rural-place\"
\"GEOCOMP\"	\"45\"	\"Rural--place of 2,500 or more population\"
\"GEOCOMP\"	\"46\"	\"Rural--place of 1,000 to 2,499 population\"
\"GEOCOMP\"	\"47\"	\"Rural--place of less than 1,000 population\"
\"GEOCOMP\"	\"48\"	\"Rural--not in place\"
\"GEOCOMP\"	\"49\"	\"Rural--farm\"
\"GEOCOMP\"	\"50\"	\"Urban portion of extended place\"
\"GEOCOMP\"	\"51\"	\"Rural portion of extended place\"
\"GEOCOMP\"	\"89\"	\"American Indian Reservation and Trust Land--Federal\"
\"GEOCOMP\"	\"90\"	\"American Indian Reservation and Trust Land--State\"
\"GEOCOMP\"	\"91\"	\"Oklahoma Tribal Statistical Area\"
\"GEOCOMP\"	\"92\"	\"Tribal Designated Statistical Area\"
\"GEOCOMP\"	\"93\"	\"Alaska Native Village Statistical Area\"
\"GEOCOMP\"	\"94\"	\"State Designated Tribal Statistical Area\"
\"GEOCOMP\"	\"95\"	\"Hawaiian Home Land\"
\"GEOCOMP\"	\"A0\"	\"In metropolitan or micropolitan statistical area\"
\"GEOCOMP\"	\"A1\"	\"In metropolitan or micropolitan statistical area--in principal city\"
\"GEOCOMP\"	\"A2\"	\"In metropolitan or micropolitan statistical area--not in principal city\"
\"GEOCOMP\"	\"A3\"	\"In metropolitan or micropolitan statistical area--urban\"
\"GEOCOMP\"	\"A4\"	\"In metropolitan or micropolitan statistical area--urban--in urbanized area\"
\"GEOCOMP\"	\"A5\"	\"In metropolitan or micropolitan statistical area--urban--in urban cluster\"
\"GEOCOMP\"	\"A6\"	\"In metropolitan or micropolitan statistical area--rural\"
\"GEOCOMP\"	\"A7\"	\"In metropolitan or micropolitan statistical area of 5,000,000 or more population\"
\"GEOCOMP\"	\"A8\"	\"In metropolitan or micropolitan statistical area of 2,500,000 to 4,999,999 population\"
\"GEOCOMP\"	\"A9\"	\"In metropolitan or micropolitan statistical area of 1,000,000 to 2,499,999 population\"
\"GEOCOMP\"	\"AA\"	\"In metropolitan or micropolitan statistical area of 500,000 to 999,999 population\"
\"GEOCOMP\"	\"AB\"	\"In metropolitan or micropolitan statistical area of 250,000 to 499,999 population\"
\"GEOCOMP\"	\"AC\"	\"In metropolitan or micropolitan statistical area of 100,000 to 249,999 population\"
\"GEOCOMP\"	\"AD\"	\"In metropolitan or micropolitan statistical area of 50,000 to 99,999 population\"
\"GEOCOMP\"	\"AE\"	\"In metropolitan or micropolitan statistical area of 25,000 to 49,999 population\"
\"GEOCOMP\"	\"AF\"	\"In metropolitan or micropolitan statistical area of less than 25,000 population\"
\"GEOCOMP\"	\"C0\"	\"In metropolitan statistical area\"
\"GEOCOMP\"	\"C1\"	\"In metropolitan statistical area--in principal city\"
\"GEOCOMP\"	\"C2\"	\"In metropolitan statistical area--not in principal city\"
\"GEOCOMP\"	\"C3\"	\"In metropolitan statistical area--urban\"
\"GEOCOMP\"	\"C4\"	\"In metropolitan statistical area--urban--in urbanized area\"
\"GEOCOMP\"	\"C5\"	\"In metropolitan statistical area--urban--in urban cluster\"
\"GEOCOMP\"	\"C6\"	\"In metropolitan statistical area--rural\"
\"GEOCOMP\"	\"C7\"	\"In metropolitan statistical area of 5,000,000 or more population\"
\"GEOCOMP\"	\"C8\"	\"In metropolitan statistical area of 2,500,000 to 4,999,999 population\"
\"GEOCOMP\"	\"C9\"	\"In metropolitan statistical area of 1,000,000 to 2,499,999 population\"
\"GEOCOMP\"	\"CA\"	\"In metropolitan statistical area of 500,000 to 999,999 population\"
\"GEOCOMP\"	\"CB\"	\"In metropolitan statistical area of 250,000 to 499,999 population\"
\"GEOCOMP\"	\"CC\"	\"In metropolitan statistical area of 100,000 to 249,999 population\"
\"GEOCOMP\"	\"CD\"	\"In metropolitan statistical area of less than 100,000 population\"
\"GEOCOMP\"	\"CE\"	\"In metropolitan statistical area of 5,000,000 or more population--in principal city\"
\"GEOCOMP\"	\"CF\"	\"In metropolitan statistical area of 5,000,000 or more population--not in principal city\"
\"GEOCOMP\"	\"CG\"	\"In metropolitan statistical area of 2,500,000 to 4,999,999 population--in principal city\"
\"GEOCOMP\"	\"CH\"	\"In metropolitan statistical area of 2,500,000 to 4,999,999 population--not in principal city\"
\"GEOCOMP\"	\"CJ\"	\"In metropolitan statistical area of 1,000,000 to 2,499,999 population--in principal city\"
\"GEOCOMP\"	\"CK\"	\"In metropolitan statistical area of 1,000,000 to 2,499,999 population--not in principal city\"
\"GEOCOMP\"	\"CL\"	\"In metropolitan statistical area of 500,000 to 999,999 population--in principal city\"
\"GEOCOMP\"	\"CM\"	\"In metropolitan statistical area of 500,000 to 999,999 population--not in principal city\"
\"GEOCOMP\"	\"CN\"	\"In metropolitan statistical area of 250,000 to 499,999 population--in principal city\"
\"GEOCOMP\"	\"CP\"	\"In metropolitan statistical area of 250,000 to 499,999 population--not in principal city\"
\"GEOCOMP\"	\"CQ\"	\"In metropolitan statistical area of 100,000 to 249,999 population--in principal city\"
\"GEOCOMP\"	\"CR\"	\"In metropolitan statistical area of 100,000 to 249,999 population--not in principal city\"
\"GEOCOMP\"	\"CS\"	\"In metropolitan statistical area of less than 100,000 population--in principal city\"
\"GEOCOMP\"	\"CT\"	\"In metropolitan statistical area of less than 100,000 population--not in principal city\"
\"GEOCOMP\"	\"E0\"	\"In micropolitan statistical area\"
\"GEOCOMP\"	\"E1\"	\"In micropolitan statistical area--in principal city\"
\"GEOCOMP\"	\"E2\"	\"In micropolitan statistical area--not in principal city\"
\"GEOCOMP\"	\"E3\"	\"In micropolitan statistical area--urban\"
\"GEOCOMP\"	\"E4\"	\"In micropolitan statistical area--urban--in urbanized area\"
\"GEOCOMP\"	\"E5\"	\"In micropolitan statistical area--urban--in urban cluster\"
\"GEOCOMP\"	\"E6\"	\"In micropolitan statistical area--rural\"
\"GEOCOMP\"	\"E7\"	\"In micropolitan statistical area of 100,000 or more population\"
\"GEOCOMP\"	\"E8\"	\"In micropolitan statistical area of 50,000 to 99,999 population\"
\"GEOCOMP\"	\"E9\"	\"In micropolitan statistical area of 25,000 to 49,999 population\"
\"GEOCOMP\"	\"EA\"	\"In micropolitan statistical area of less than 25,000 population\"
\"GEOCOMP\"	\"EB\"	\"In micropolitan statistical area of 100,000 or more population--in principal city\"
\"GEOCOMP\"	\"EC\"	\"In micropolitan statistical area of 100,000 or more population--not in principal city\"
\"GEOCOMP\"	\"ED\"	\"In micropolitan statistical area of 50,000 to 99,999 population--in principal city\"
\"GEOCOMP\"	\"EE\"	\"In micropolitan statistical area of 50,000 to 99,999 population--not in principal city\"
\"GEOCOMP\"	\"EF\"	\"In micropolitan statistical area of 25,000 to 49,999 population--in principal city\"
\"GEOCOMP\"	\"EG\"	\"In micropolitan statistical area of 25,000 to 49,999 population--not in principal city\"
\"GEOCOMP\"	\"EH\"	\"In micropolitan statistical area of less than 25,000 population--in principal city\"
\"GEOCOMP\"	\"EJ\"	\"In micropolitan statistical area of less than 25,000 population--not in principal city\"
\"GEOCOMP\"	\"G0\"	\"Not in metropolitan or micropolitan statistical area\"
\"GEOCOMP\"	\"G1\"	\"Not in metropolitan or micropolitan statistical area--urban\"
\"GEOCOMP\"	\"G2\"	\"Not in metropolitan or micropolitan statistical area--urban--in urbanized area\"
\"GEOCOMP\"	\"G3\"	\"Not in metropolitan or micropolitan statistical area--urban--in urban cluster\"
\"GEOCOMP\"	\"G4\"	\"Not in metropolitan or micropolitan statistical area--rural\"
\"GEOCOMP\"	\"H0\"	\"Not in metropolitan statistical area\"
\"GEOCOMP\"	\"H1\"	\"Not in metropolitan statistical area--urban\"
\"GEOCOMP\"	\"H2\"	\"Not in metropolitan statistical area--urban--in urbanized area\"
\"GEOCOMP\"	\"H3\"	\"Not in metropolitan statistical area--urban--in urban cluster\"
\"GEOCOMP\"	\"H4\"	\"Not in metropolitan statistical area--rural\"
\"GEOCOMP\"	\"J0\"	\"In combined statistical area\"
\"GEOCOMP\"	\"L0\"	\"Not in combined statistical area\"
\"GEOCOMP\"	\"M0\"	\"In New England city and town area\"
\"GEOCOMP\"	\"M1\"	\"In New England city and town area--in principal city\"
\"GEOCOMP\"	\"M2\"	\"In New England city and town area--not in principal city\"
\"GEOCOMP\"	\"M3\"	\"In New England city and town area--urban\"
\"GEOCOMP\"	\"M4\"	\"In New England city and town area--urban--in urbanized area\"
\"GEOCOMP\"	\"M5\"	\"In New England city and town area--urban--in urban cluster\"
\"GEOCOMP\"	\"M6\"	\"In New England city and town area--rural\"
\"GEOCOMP\"	\"P0\"	\"In combined New England city and town area\"
\"CHARITER\"		\"Characteristic Iteration\"
\"CHARITER\"	\"0\"	\"Not a characteristic iteration\"
\"CHARITER\"	\"1\"	\"Total Population\"
\"REGION\"		\"Region (Geographic Area Code)\"
\"REGION\"	\"1\"	\"Northeast\"
\"REGION\"	\"2\"	\"Midwest\"
\"REGION\"	\"3\"	\"South\"
\"REGION\"	\"4\"	\"West\"
\"REGION\"	\"9\"	\"Not in a region (Puerto Rico)\"
\"DIVISION\"		\"Division (Geographic Area Code)\"
\"DIVISION\"	\"0\"	\"Not in a division (Puerto Rico)\"
\"DIVISION\"	\"1\"	\"New England\"
\"DIVISION\"	\"2\"	\"Middle Atlantic\"
\"DIVISION\"	\"3\"	\"East North Central\"
\"DIVISION\"	\"4\"	\"West North Central\"
\"DIVISION\"	\"5\"	\"South Atlantic\"
\"DIVISION\"	\"6\"	\"East South Central\"
\"DIVISION\"	\"7\"	\"West South Central\"
\"DIVISION\"	\"8\"	\"Mountain\"
\"DIVISION\"	\"9\"	\"Pacific\"
\"STATE\"		\"State (FIPS)\"
\"STATE\"	\"1\"	\"Alabama\"
\"STATE\"	\"2\"	\"Alaska\"
\"STATE\"	\"4\"	\"Arizona\"
\"STATE\"	\"5\"	\"Arkansas\"
\"STATE\"	\"6\"	\"California\"
\"STATE\"	\"8\"	\"Colorado\"
\"STATE\"	\"9\"	\"Connecticut\"
\"STATE\"	\"10\"	\"Delaware\"
\"STATE\"	\"11\"	\"District of Columbia\"
\"STATE\"	\"12\"	\"Florida\"
\"STATE\"	\"13\"	\"Georgia\"
\"STATE\"	\"15\"	\"Hawaii\"
\"STATE\"	\"16\"	\"Idaho\"
\"STATE\"	\"17\"	\"Illinois\"
\"STATE\"	\"18\"	\"Indiana\"
\"STATE\"	\"19\"	\"Iowa\"
\"STATE\"	\"20\"	\"Kansas\"
\"STATE\"	\"21\"	\"Kentucky\"
\"STATE\"	\"22\"	\"Louisiana\"
\"STATE\"	\"23\"	\"Maine\"
\"STATE\"	\"24\"	\"Maryland\"
\"STATE\"	\"25\"	\"Massachusetts\"
\"STATE\"	\"26\"	\"Michigan\"
\"STATE\"	\"27\"	\"Minnesota\"
\"STATE\"	\"28\"	\"Mississippi\"
\"STATE\"	\"29\"	\"Missouri\"
\"STATE\"	\"30\"	\"Montana\"
\"STATE\"	\"31\"	\"Nebraska\"
\"STATE\"	\"32\"	\"Nevada\"
\"STATE\"	\"33\"	\"New Hampshire\"
\"STATE\"	\"34\"	\"New Jersey\"
\"STATE\"	\"35\"	\"New Mexico\"
\"STATE\"	\"36\"	\"New York\"
\"STATE\"	\"37\"	\"North Carolina\"
\"STATE\"	\"38\"	\"North Dakota\"
\"STATE\"	\"39\"	\"Ohio\"
\"STATE\"	\"40\"	\"Oklahoma\"
\"STATE\"	\"41\"	\"Oregon\"
\"STATE\"	\"42\"	\"Pennsylvania\"
\"STATE\"	\"44\"	\"Rhode Island\"
\"STATE\"	\"45\"	\"South Carolina\"
\"STATE\"	\"46\"	\"South Dakota\"
\"STATE\"	\"47\"	\"Tennessee\"
\"STATE\"	\"48\"	\"Texas\"
\"STATE\"	\"49\"	\"Utah\"
\"STATE\"	\"50\"	\"Vermont\"
\"STATE\"	\"51\"	\"Virginia\"
\"STATE\"	\"53\"	\"Washington\"
\"STATE\"	\"54\"	\"West Virginia\"
\"STATE\"	\"55\"	\"Wisconsin\"
\"STATE\"	\"56\"	\"Wyoming\"
\"STATE\"	\"72\"	\"Puerto Rico\"
\"COUNTY\"		\"County (FIPS)\"
\"AREALAND\"		\"Land area measurement in square meters\"
\"AREAWATR\"		\"Water area measurement in square meters\""
     )
     res <- utils::read.table(
       con,
       header    = TRUE,
       row.names = NULL,
       sep       = "\t",
       as.is     = TRUE
     )
     close(con)
     res
   })
dfcolnames <- local({
     con <- textConnection(
       "\"fileid\"
\"stusab\"
\"sumlev\"
\"geocomp\"
\"chariter\"
\"cifsn\"
\"logrecno\"
\"region\"
\"division\"
\"state\"
\"county\"
\"countycc\"
\"countysc\"
\"cousub\"
\"cousubcc\"
\"cousubsc\"
\"place\"
\"placecc\"
\"placesc\"
\"tract\"
\"blkgrp\"
\"block\"
\"iuc\"
\"concit\"
\"concitcc\"
\"concitsc\"
\"aianhh\"
\"aianhhfp\"
\"aianhcc\"
\"aihhtli\"
\"aitsce\"
\"aits\"
\"aitscc\"
\"ttract\"
\"tblkgrp\"
\"anrc\"
\"anrccc\"
\"cbsa\"
\"cbsasc\"
\"metdiv\"
\"csa\"
\"necta\"
\"nectasc\"
\"nectadiv\"
\"cnecta\"
\"cbsapci\"
\"nectapci\"
\"ua\"
\"uasc\"
\"uatype\"
\"ur\"
\"cd\"
\"sldu\"
\"sldl\"
\"vtd\"
\"vtdi\"
\"reserve2\"
\"zcta5\"
\"submcd\"
\"submcdcc\"
\"sdelm\"
\"sdsec\"
\"sduni\"
\"arealand\"
\"areawatr\"
\"name\"
\"funcstat\"
\"gcuni\"
\"pop100\"
\"hu100\"
\"intplat\"
\"intptlon\"
\"lsadc\"
\"partflag\"
\"reserve3\"
\"uga\"
\"statens\"
\"countyns\"
\"cousubns\"
\"placens\"
\"concitns\"
\"aianhhns\"
\"aitsns\"
\"anrcns\"
\"submcdns\"
\"cd113\"
\"cd114\"
\"cd115\"
\"sldu2\"
\"sldu3\"
\"sldu4\"
\"sldl2\"
\"sldl3\"
\"sldl4\"
\"aianhhsc\"
\"csasc\"
\"cnectasc\"
\"memi\"
\"nmemi\"
\"puma\"
\"reserved\""
     )
     res <- utils::read.table(
       con,
       header    = FALSE,
       row.names = NULL,
       sep       = "\t",
       as.is     = TRUE
     )
     close(con)
     res
   })
maybetibble <- function(x) {
  "create as a tibble/dataframe, if tibble is installed"
  ## https://stackoverflow.com/a/4090208
  if (!is.null(tryCatch(loadNamespace("tibble"),
                        warning=function(x)NULL, 
                        error=function(x)NULL))) {
    x <- tibble::tibble(x)
  }
  x
}

#' (internal) Make Sure a Dataframe Exists
#'
#' ensure() returns the dataframe (or tibble), reading it in if necessary
#'
#' takes the name of the data and the suffix of the backing store for
#' the dataframe and makes sure it is in our private environment.
#'
#' @param name Name of the dataframe
#'
#' Stops in the case of an error
#' 
#' @return returns the (possibly retrieved) dataframe.
ensure <- function(name) {
  csvname <- paste(name, ".csv", sep="")
  csvgzname <- paste(name, ".csv.gz", sep="")

  ## if we exist as a package, take our "private" version; else,
  ## '.csv', else '.csv.gz', else error
  filenames <- c(tryCatch(system.file("private",
                                   csvgzname,
                                   package="us.census.geoheader",
                                   mustWork=TRUE),
                          error=function(e) NULL),
                 csvname, csvgzname)

  whiches <- file.exists(filenames)
  stopifnot(any(whiches)) # if nothing, we're in trouble...
  x <- utils::read.csv(filenames[which(whiches)[[1]]], stringsAsFactors=FALSE)
  maybetibble(x)
}
#' (internal) Return the Decode Table
#'
#' @return the decode table
get.decode <- function() {
  decodetable
}

#' (internal) Return the Database
#'
#' @return the database (as a tibble and/or dataframe)
get.data <- function() {
  ensure("us2010sf2_101_col_usrdsc") # get the main dataset
}

#' Return text describing the meaning of a value in a column
#'
#' uscgh.2010.decode() returns a textual description of a cell
#'
#' takes as input the name of a column in the Geographic Header file
#' from the 2010 US census, and a value found in that column, and
#' attempts to return a description of the meaning of that value.
#'
#' @param colname Name of the column to be described.
#' @param colvalue Value whose description is wanted (\"""\" -- the
#'   default -- to describe, generically, `colname` itself).
#' @param warnings Whether a `(colname,colvalue)` tuple that is
#'   not successfully decoded should produce a warning message.  (In
#'   any event, the result will be NULL.)
#'
#' @return A character string describing the `colname` or
#'   `(colname,colvalue)` tuple, if found.  If not found, `NULL` is
#'   returned (and, if `warnings` is `TRUE`, a warning is generated).
#'
#' @seealso 'RShowDoc("a-tour", package="us.census.geoheader")' for a
#'   short tour of a few columns of the database
#' 
#' @examples
#' uscgh.2010.decode('SUMLEV', 40)
#'
#' @export

uscgh.2010.decode <- function(colname, colvalue="", warnings=TRUE) {
  "what does COLVALUE in column COLNAME mean?"

  decode <- get.decode()


  stopifnot(any(grepl(colname, unlist(dfcolnames), ignore.case=TRUE)))

  rows <- decode[grepl(colname, decode$colname, ignore.case=TRUE),]
  if (nrow(rows) == 0) {
    if (warnings) {
      warning(sprintf("uscgh.decode: column name \"%s\" not in decode table -- returning NULL\n", colname))
    }
    return(NULL)
  }
  if (is.null(colvalue)) {
    rows <- rows[rows$colvalue=="",]
    if (nrow(rows) == 0) {
      ## here, no warning
      return(NULL)
    }
    stopifnot(nrow(rows) == 1) # should only be one!
  }
  rows <- rows[grepl(colvalue, rows$colvalue, ignore.case=TRUE),]
  if (nrow(rows) == 0) {
    ## your guess is as good as mine
    return(NULL)
  }
  return(rows$description[[1]])
}

#' Return the 2010 Census SF2 Geographic Header Data
#'
#' Returns the 2010 Census SF2 geographic header dataset
#'
#' `uscgh.2010.dataset` returns the SF2 geographic header data from
#' the 2010 US census
#'
#' @return a dataframe (tibble, if the tibble package is available)
#'
#' @seealso 'RShowDoc("a-tour", package="us.census.geoheader")' for a
#'   short tour of a few columns of the database
#' 
#' @examples
#' x <- uscgh.2010.sf2.geoheader()
#' 
#' @export

uscgh.2010.sf2.geoheader <- function() {
  get.data()
}
