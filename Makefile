# Makefile for HGIS project

.PHONY: all data
RDIR = .
DATA_DIR = $(RDIR)/data
SRC_DIR = $(RDIR)/R
#RES_DIR = H:/USAMS/Results
RES_DIR = /mnt/shared/USAMS/Results
FILES = 2017\ Results/USAMS032817R.txt \
				2017\ Results/USAMS040317R.txt \
				2018\ Results/USAMS052318RHIGS.txt \
				2018\ Results/USAMS053018R.txt \
				2018\ Results/USAMS060618R.txt \
				2020\ Results/USAMS012220R.txt \
				2020\ Results/USAMS101320R.txt \
				USAMS120320R.txt \
				USAMS012121R.txt \
				USAMS020521R.txt \
				USAMS030421R.txt \
				USAMS040121R.txt \
				USAMS041521R.txt \
				USAMS051421R.txt \
				USAMS052621R.txt \
				USAMS061121R.txt \
				USAMS061821R.txt \
				USAMS062521R.txt \
				USAMS062921R.txt

all: data

data: $(FILES)

$(FILES): 
	cp '$(RES_DIR)/$@' $(DATA_DIR)/

