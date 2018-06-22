.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Benvenidos a mi package')
}

# setting options without conflicting with other packages or user defs
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path='~/R-dev',
    devtools.install.args='',
    devtools.name='Damian',
    devtools.desc.author=(
      'person("Damian", "Sattethwaite-Phillips", "damiansp@gmail.com"'), 
    role=c('aut', 'cre'))
    devtools.desc.license='CCO', # or whatever
    devtools.des.suggests=NULL,
    devtools.desc=list())
  to.set <- !(names(op.devtools) %in% names(op))
  if (any(to.set)) { 
    options(op.devtools[to.set])
  }
  invisible()
}