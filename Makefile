# Makefile for HGIS project


RDIR = .
DATA_DIR = $(RDIR)/data
SRC_DIR = $(RDIR)/R
RES_DIR = /mnt/shared/USAMS/Results
FILES = 2017\ Results/USAMS032817R.txt \
				2017\ Results/USAMS040317R.txt \
				2018\ Results/USAMS052318RHIGS.txt \
				2018\ Results/USAMS053018R.txt \
				2018\ Results/USAMS060618R.txt \
				2020\ Results/USAMS012220R.txt \
				2020\ Results/USAMS101320R.txt \
				USAMS120320R.txt \
				USAMS012121R.txt

all: data

data: $(FILES)

$(FILES): 
	cp '$(RES_DIR)/$@' $(DATA_DIR)/

