.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        "কদম গাছে উঠিয়া আছে গোলমেলে ডেটা\n",
        "প্লট বানিয়ে সোজা করবে ggkodom-এর ব্যাটা!\n",
        "\n",
        "(English Translation: Messy data has climbed the Kadam tree, but ggkodom's lad will straighten it out with a plot!)\n",
        "Welcome to ggkodom! \n",
        "Version: ", utils::packageVersion(pkgname)
    )
}
