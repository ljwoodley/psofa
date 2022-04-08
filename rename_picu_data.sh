#!/bin/sh
OLDWD=$(pwd)
cd data/picu/
mv PICU-child_encounter_data.csv encounter.csv
mv PICU-child_flowsheets.csv flowsheets.csv
mv PICU-child_labs.csv labs.csv
mv PICU-child_medications.csv medications.csv

cd $OLDWD
