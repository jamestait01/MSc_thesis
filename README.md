outcome_defintion is used to define hypertensive cases using HES/self-reported Biobank data.
extraction_and_recoding is used to extract relevant covariates from the Biobank.
prescriptions/covariates/hospitalisations contains the code used to extract and clean their respective datasets.

* Step 1: Use outcome_definition to extract hypertensive cases.
* Step 2: Use extraction_and_recoding to extract variables, pre-process (see covariates folder)
* Step 3: Exclude eids that do not have prescription data available (list of eids found in prescription folder)
* Step 4: Merge case and covariates dataset together.
* Step 5: Extract medication information using code in prescriptions folder.
* Step 6: Merge datasets together.
