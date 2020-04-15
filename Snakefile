rule all:
    input:
        "report/report.pdf"

# Data generation =============================================================

rule sim:
    input:
        "data/sim.R"
    output:
        "data/sim.csv"
    shell:
        "Rscript data/sim.R"

rule suellen_clean:
    input:
        "data-raw/suellen.xlsx",
        "data/suellen.R"
    output:
        "data/suellen.csv"
    shell:
        "Rscript data/suellen.R"

# Data plotting ===============================================================

rule data_plot:
    input:
        "data-plot/data-plot.R",
        "data/sim.csv",
        "data/suellen.csv"
    output:
        "data-plot/sim-hist-cont.pdf",
        "data-plot/suellen-hist.pdf"
    shell:
        "Rscript data-plot/data-plot.R"

# Model fitting ===============================================================

rule fit:
    input:
        "model-fit/model-fit.R",
        "data/sim.csv",
        "data/suellen.csv"
    output:
        "model-fit/fit-sim.csv",
        "model-fit/fit-suellen.csv"
    shell:
        "Rscript model-fit/model-fit.R"

# Report ======================================================================

rule report:
    input:
        "data-plot/sim-hist-cont.pdf",
        "data-plot/suellen-hist.pdf",
        "report/report.Rmd"
    output:
        "report/report.pdf"
    shell:
        """Rscript -e 'rmarkdown::render("report/report.Rmd")'"""
