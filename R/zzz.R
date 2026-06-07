.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        "\u0995\u09a6\u09ae \u0997\u09be\u099b\u09c7 \u0989\u09a0\u09bf\u09af\u09bc\u09be \u0986\u099b\u09c7 \u0997\u09cb\u09b2\u09ae\u09c7\u09b2\u09c7 \u09a1\u09c7\u099f\u09be\n",
        "\u09aa\u09cd\u09b2\u099f \u09ac\u09be\u09a8\u09bf\u09af\u09bc\u09c7 \u09b8\u09cb\u099c\u09be \u0995\u09b0\u09ac\u09c7 ggkodom-\u098f\u09b0 \u09ac\u09cd\u09af\u09be\u099f\u09be!\n",
        "\n",
        "(English Translation: Messy data has climbed the Kodom tree, but ggkodom's lad will straighten it out with a plot!)\n",
        "Welcome to ggkodom! \n",
        "Version: ", utils::packageVersion(pkgname)
    )
}
