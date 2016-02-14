setwd("c:/users/n/Skydrive/documents/AAA/Donnelly/Data/Malakoff2")
source ("c:/users/n/Skydrive/documents/research/r/txt/startup.R")
source("c:/users/n/Skydrive/documents/AAA/Donnelly/Data/Malakoff/directions.R")
labels = read.csv("labels.csv")

sqlitecon <- dbConnect(SQLite(), "malakoff.db")
tabletoread <-"eighth_grade_rows"
eg_rows <- dbGetQuery(sqlitecon, paste("SELECT * FROM ", tabletoread))
eg_rows$row_names= NULL
names(eg_rows)

sqlitecon <- dbConnect(SQLite(), "malakoff.db")
tabletoread <-"high_school_rows"
hs_rows <- dbGetQuery(sqlitecon, paste("SELECT * FROM ", tabletoread))
hs_rows$row_names= NULL
names(hs_rows)

options (digits =2 )
current_district = "All School Districts"
filetowrite = concat(c(current_district, ".tex"))

	source("tables.R")
	if (file.exists(filetowrite))
	{file.remove(filetowrite)}

	#WRITE PROLOGUE
	writeit (concat(c("%DATE: " , as.character(Sys.time()))))
	writeit ("\n")
	writeit("\\documentclass[11pt]{article}\n")
	writeit("\\usepackage[table]{xcolor}% http://ctan.org/pkg/xcolor\n")
	#\usepackage[xcdraw]{xcolor}

	writeit("\\usepackage{multirow}\n")
	writeit("\\usepackage{booktabs}\n")
	writeit("\\usepackage{graphicx}\n")
	writeit("\\usepackage{datetime}\n")
	writeit("\\usepackage[top=2in, bottom=1.5in, left=.5in, right=1in]{geometry} \n")
	#writeit("\\usepackage{caption} \n")
	writeit("\\usepackage{tabularx} \n")
	#writeit("\\usepackage{color} \n")

	writeit("\\begin{document}\n")

	#WRITE THE HEADER PAGE

	writeit("\\begin{center} \n")

	writeit(concat(c("\\textbf{\\LARGE Report for All School Districts }", "\n")))

	writeit ("\\newline \n")
	writeit ("\\newline \n")
	writeit ("\\newline \n")

	writeit(concat(c("\\textnormal{\\Large ", "Total Number of Paired Middle School Students: ", dim(eg_rows)[1], "} \n")))

	writeit ("\\newline \n")
	writeit ("\\newline \n")
	writeit ("\\newline \n")

	writeit(concat(c("\\textnormal{\\Large ", "Total Number of Paired High School Students: ", dim(hs_rows)[1], "} \n")))

	writeit ("\\newline \n")
	writeit ("\\newline \n" )
	writeit ("\\newline \n")

	writeit(concat(c("\\textnormal{The table header is highlighted in green if the results for that school (Chi-square test) are statistically significant", "} \n")))

	#writeit("\\[1.5cm]")
	#writeit("\\Generated on \\mmddyyyydate\\today \\currenttime")
	writeit("\\end{center} \n")

	writeit("\\newpage")

	#WRITE ONEWAY TABLES 1-6
	source ("one_way_tables.r")

	ns = c(1:6)

	for (n in seq_along(ns))
	{ 
		i = ns[n]
		{
		table_title = labels[labels$var_name==colnames(eg_rows)[i],"var_label"]
		table_num = i
		table_title = concat(c(table_num, ":", as.character(table_title), "\n\n"))
		
		table1 = table(eg_rows[[i]])
		table2 = table(hs_rows[[i]])
		percents1 = table1 /sum(table1)
		percents2 = table2 /sum(table2)

		write_one_way_table()
		}
	}
	#END WRITE ONEWAY TABLES AT BEGINNING

	
	#WRITE TWOWAY TABLES
	source ("tables.r")
	table_count = 0
	for(i in 7:57) 
	{ 
	table_title = labels[labels$var_name==colnames(eg_rows)[i],"var_label"]
	#table_title = labels[labels$var_num==i,2]
	table_title = concat(c(i, ":", as.character(table_title), "\n\n"))
	table_title

	eg_ct = CrossTable(eg_rows[[i]], eg_rows$time, prop.t = FALSE, prop.r = TRUE, prop.c = FALSE, prop.chisq = FALSE, format = "SAS", chisq=TRUE,dnn = c(names(eg_rows)[i], ""))
	hs_ct = CrossTable(hs_rows[[i]], hs_rows$time, prop.t = FALSE, prop.r = TRUE, prop.c = FALSE, prop.chisq = FALSE, format = "SAS", chisq=TRUE,dnn = c(names(eg_rows)[i], ""))

	direct = directions[[i]]
	percents1 = eg_ct$prop.col
	percents2 = hs_ct$prop.col

	eg_colortouse = "white"
	hs_colortouse = "white"
	if (eg_ct$chisq[3] < .05) eg_colortouse = "green"
	if (hs_ct$chisq[3] < .05) hs_colortouse = "green"

	write_table()
	} 
	#END WRITE TWOWAY TABLES
	
	#WRITE ONEWAY TABLES AT END
	source ("one_way_tables.r")

	ns = c(68:69, 58:67)

	for (n in seq_along(ns))
	{ 
		i = ns[n]
		{
		table_title = labels[labels$var_name==colnames(eg_rows)[i],"var_label"]
		table_num = i
		table_title = concat(c(table_num, ":", as.character(table_title), "\n\n"))
		
		table1 = table(eg_rows[[i]])
		table2 = table(hs_rows[[i]])
		percents1 = table1 /sum(table1)
		percents2 = table2 /sum(table2)
		print(dim(percents2))
		write_one_way_table()
		}
	}
	#END WRITE ONEWAY TABLES AT END
	
	writeit("\\end{document}\n")

	#Create the PDF
	pdffilename = paste(current_district, ".pdf", sep = "")
	if (file.exists(pdffilename)) {file.remove(pdffilename)}
	#system (paste("pdflatex ", filetowrite, sep = ""))
	#system (paste("bibtex ", filetowrite, sep = ""))
	#system (paste("pdflatex ", filetowrite, sep = ""))
	#system (paste("open ", filetowrite, ".pdf", sep = ""))

names(hs_rows)