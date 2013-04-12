# Download datasets

if (!file.exists("data")) dir.create("data")

download.file("http://data.had.co.nz/13-flights/delay.rds", "data/delay.rds")
download.file("http://data.had.co.nz/13-flights/dist.rds", "data/dist.rds")
download.file("http://data.had.co.nz/13-flights/time.rds", "data/time.rds")
