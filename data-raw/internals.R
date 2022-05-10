# WHO anthro files
acanthro <- read.table("anthro_files/acanthro.txt", header = TRUE)
bfawho2007 <- read.table("anthro_files/bfawho2007.txt", header = TRUE)
bmianthro <- read.table("anthro_files/bmianthro.txt", header = TRUE)
hcanthro <- read.table("anthro_files/hcanthro.txt", header = TRUE)
hfawho2007 <- read.table("anthro_files/hfawho2007.txt", header = TRUE)
lenanthro <- read.table("anthro_files/lenanthro.txt", header = TRUE)
ssanthro <- read.table("anthro_files/ssanthro.txt", header = TRUE)
tsanthro <- read.table("anthro_files/tsanthro.txt", header = TRUE)
weianthro <- read.table("anthro_files/weianthro.txt", header = TRUE)
wfawho2007 <- read.table("anthro_files/wfawho2007.txt", header = TRUE)
wfhanthro <- read.table("anthro_files/wfhanthro.txt", header = TRUE)
wflanthro <- read.table("anthro_files/wflanthro.txt", header = TRUE)

# Expwected weight table
expected_weight_table <- read.csv("data-raw/exp_weight_table.csv")

usethis::use_data(acanthro, bfawho2007, bmianthro, hcanthro, hfawho2007,
                   lenanthro, ssanthro, tsanthro, weianthro, wfawho2007,
                   wfhanthro, wflanthro, expected_weight_table, internal = TRUE,
                  overwrite = TRUE)
