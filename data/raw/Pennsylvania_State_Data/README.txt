USER NOTES:

This dataset was provided by Measures for Justice
https://www.measuresforjustice.org

This dataset and other statewide datasets can be downloaded from the Measures
for Justice State datasets download page:
https://www.measuresforjustice.org/portal/datasets

Measures for Justice has developed a detailed methodology to standardize
criminal justice data across jurisdictions across the United States. The
development of the Measures is an iterative process, including updates to and
improvement of Measure calculation, ongoing data collection and management,
quality control, calculation, and engagement with data providers and county
stakeholders. Our full methodology is available at:
https://www.measuresforjustice.org/portal/methodology


FILE DESCRIPTIONS:

* measures.csv

This table has 3 columns that list the id, name, and format for each of
measures.

* locations.csv

This table has 3 columns that list the id, type, and name of all the locations
in the selected state.

* filters.csv

This table has 2 columns that provide the i and description for each of the filters.

* data-<Time Period>.csv

This is a table listing the data displayed in the Data Portal broken down by
measure, filter, and location.  The name of the file refers to the time period
the data is generated against.  The missing column lists how many cases were
unknown/missing on the variable used for the numerator, and the
missing_percentage displays that as a percentage out of the denominator.  The
status column describes whether there are any warnings or availability notes
associated with that measure/filter/location combination.  For percentage
measures, the denominator column shows the pool of cases for that measure, the
numerator column shows the number of cases that meet the criteria for the
measure, and the value column displays the calculation.  For median measures,
the value column shows the median, and the count, sum, sum_squares, minimum,
maximum, standard_devation, and average columns provide the descriptive
statistics for that measure/filter/location.

* legal-context-<Time Period>.txt

This is a summary of information about state statutory laws relevant to our
measures.  These data were collected by applying a standard set of questions
to the laws that were in place during the measurement period indicated in the
file name.  The file lists the name of each measure, followed by an outline
of the legal context relevant to that measure.

* measures-legal-context-<Time Period>.txt

This is a join file that maps a measure to a set of legal context that is
relevant to help interpret that measure.


