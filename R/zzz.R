.onLoad <- function (libname, pkgname)
{

  packageStartupMessage("\nTo pull UNAIDS 2021 estimates from 1990-2020, use munge_aids().
  For HIV estimates, use return_type == 'HIV Estimates'.
  For Test & Treat data, use return_type == 'Test & Treat'.
  Use indicator_type == 'Integer' for data in integer form or indicator_type == 'Percent' for data in percent form.\n")


  invisible ()
}
