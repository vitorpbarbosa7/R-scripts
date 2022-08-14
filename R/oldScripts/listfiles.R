
allfiles = list.files()

RScripts = grep("*.R",allfiles,value = T)

source(RScripts[1])
dev.off()

