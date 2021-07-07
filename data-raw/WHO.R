acanthro <- read.table("WHO/acanthro.txt", header = TRUE)
bfawho2007 <- read.table("WHO/bfawho2007.txt", header = TRUE)
bmianthro <- read.table("WHO/bmianthro.txt", header = TRUE)
hcanthro <- read.table("WHO/hcanthro.txt", header = TRUE)
hfawho2007 <- read.table("WHO/hfawho2007.txt", header = TRUE)
lenanthro <- read.table("WHO/lenanthro.txt", header = TRUE)
ssanthro <- read.table("WHO/ssanthro.txt", header = TRUE)
tsanthro <- read.table("WHO/tsanthro.txt", header = TRUE)
weianthro <- read.table("WHO/weianthro.txt", header = TRUE)
wfawho2007 <- read.table("WHO/wfawho2007.txt", header = TRUE)
wfhanthro <- read.table("WHO/wfhanthro.txt", header = TRUE)
wflanthro <- read.table("WHO/wflanthro.txt", header = TRUE)

usethis::use_data(acanthro, bfawho2007, bmianthro, hcanthro, hfawho2007,
                   lenanthro, ssanthro, tsanthro, weianthro, wfawho2007,
                   wfhanthro, wflanthro, internal = TRUE)
