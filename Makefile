# Makefile for HGIS project


RDIR = .
DATA_DIR = $(RDIR)/Data
SRC_DIR = $(RDIR)/R
RES_DIR = /mnt/shared/USAMS/Results
FILES = 2018\ Results/USAMS052318RHIGS.txt 2018\ Results/USAMS053018R.txt USAMS012220R.txt USAMS101320R.txt USAMS120320R.txt

all: data

data: $(FILES)

$(FILES): 
	cp '$(RES_DIR)/$@' $(DATA_DIR)/

