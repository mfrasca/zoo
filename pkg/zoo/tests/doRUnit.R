#! /usr/bin/Rscript
if(require(svUnit, quietly=TRUE)) {

  pkg <- "zoo"
  require(zoo)

  unlink("report.xml")  # Make sure we generate a new report
  mypkgSuite <- svSuiteList(pkg, dirs="../../zoo/inst/unitTests")  # List all our test suites
  runTest(mypkgSuite, name = pkg)  # Run them...
  ## makeTestListFromExamples is in svUnit 0.7.8 or more
  doRunExamples <- TRUE
  svUnitVersion = as.integer(strsplit(installed.packages()[which(installed.packages()[, 'Package'] == "svUnit"), "Version"], "[\\.-]")[[1]])
  if (svUnitVersion[1] == 0) {
    if (svUnitVersion[2] < 7) {
      doRunExamples <- FALSE
    } else {
      if (svUnitVersion[2] == 7)
        doRunExamples <- svUnitVersion[3] >= 8
    }
  }
  if(doRunExamples)
    runTest(tryCatch(makeTestListFromExamples(pkg, "../../pkg/man/"), error=function(e) NULL))
  protocol(Log(), type = "junit", file = "report.xml")  # ... and write report

  errorLog(stopit = TRUE, summarize = FALSE)

} else {
  warning("cannot run unit tests -- package svUnit is not available")
}
