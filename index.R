






























main <- function() {
    userInput <- readline()

    cmd  <- nerual(userInput)
    return(nerualAnsw(cmd["cmd"], userInput))
}

print("Привет, я Юнона. Со мной весело")
while (True) {
    main()
}
