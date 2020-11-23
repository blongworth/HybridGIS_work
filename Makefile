# Makefile for HGIS project


RDIR = .
DATA_DIR = $(RDIR)/Data
SRC_DIR = $(RDIR)/R
RES_DIR = //fileshare.whoi.edu/whoi/dept/gg/NOSAMS/USAMS/Results
FILES = '2018 Results/USAMS052318RHGIS.txt' '2018 Results/USAMS053018R.txt' USAMS012220R.txt USAMS101320R.txt

all: data


clean:
	rm -f $(DATA_DIR)/
	rm -f $(DATA_DIR)/
	rm -f $(DATA_DIR)/
	rm -f $(DATA_DIR)/
	rm -f $(DATA_DIR)/

data:
