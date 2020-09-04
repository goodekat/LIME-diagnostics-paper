
# LIME Diagnostics Paper

Research paper on visual diagnostic tools for LIME

# Instructions for Knitting Paper

1.  Pull repo from GitHub
2.  Install goodekat/lime and goodekat/limeaid
3.  Make sure the data folder has all up to date versions of these files
    (see note below about where to obtain these files)
      - hamby173and252\_train.csv
      - hamby224\_test.csv
      - sine\_lime\_explain.rds (optional - can delete and let code in paper create)
      - hamby\_lime.rds
      - hamby\_explain.rds
4.  Knit paper (via the paper.Rnw file)

Note: The data needed to knit the paper can be found in the Cybox folder called LIME-diagnostics-paper-data. Using my version of sine\_lime\_explain.rds is optional, since it does not take long to run, but the hamby files take a long time (approximately an hour).
