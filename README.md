Oncopeltus wing and body wall RNAi
==================================

Raw data and code for publication.

 
-

### File manifest

-   RNAi_Data.R

    -   R code using packages `dplyr` and `tidyr.` The script filters the raw
        data to adults and pharate/pre-emerged 5th instar nymphs. The data are
        subsetted for each of 10 body parts relevant to our study, and then
        filtered such that  only individuals that are “informative” for the body
        part, defined as having at least one scored (not NA) character for that
        body part, are kept. Finally, it synonymizes character information, in
        order to not double-count phenotypes scored that are part of the same
        body part.

-   RawScoringData.txt

    -   Tab-delimited flat text file. Each row is a single individual. The first
        9 columns are metadata, including

        -   Experiment\#

        -   Days post injection individual emerged

        -   Individual number (these three fields together constitute a unique
            identifier, eg 71.16.A)

        -   Freezer box location of individual

        -   Gene studied or control

        -   Treatment, i.e. dsRNA fragment 1 or fragment 2

        -   Concentration of dsRNA in injection buffer

        -   Stage injected

        -   Stage scored

    -   The next 78 are scoring columns, and the header row denotes what
        specific body part is scored. Scoring is 0 for normal or wild-type, 1
        for aberrant or affected by treatment, NS for “not scored”, meaning no
        attempt was made to score the character (many characters on the sheet
        were irrelevant for the study), and “NA” if the character was not
        available to be scored (though an attempt was made).

    -   The final 9 columns were excel calculated columns adding up the number
        of scored aberrant characters (severity), the number of scoreable
        characters (how.informative), and whether or not the individual was
        scoreable for some of the traits of interest. They are now static
        columns.

-   Uninformative_Table.tab, UninformativeNymphs.tab,
    Number_Uninformative_by_Gene.txt

    -   These files are produced by the R script, and are explained therein.
        They are for internal use, but indicate which specimens were not
        included in the final data reporting.

-   PhenotypeFrequencyTable.txt

    -   A count of aberrant phenotypes per relevant body part grouped by gene
        studied.

-   sessionInfo.txt

    -   The output of sessionInfo() when this code was run for publication.

-   DataSubsets/

    -   PhenotypeFrequencies.txt and PhenotypePenetrance.txt are other versions
        of the summary data output by this code and used for Table 1 of the
        publication.

    -   The other text files are subsets of the raw scoring data, and give a
        list of which individuals were included in the “informative specimens”
        group (ie, the group size) for each body part.
