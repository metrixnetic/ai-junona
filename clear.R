library(rjson)

opts  <- fromJSON(file = "opts.json")

print(str(opts))
print(head(opts))
print(summary(opts))

na.omit(opts)
