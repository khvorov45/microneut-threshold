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
        "model-fit/sim-lin.csv",
        "model-fit/suellen-lin.csv"
    shell:
        "Rscript model-fit/model-fit.R"

# Result plots ================================================================

rule testchar_plot:
    input:
        "model-fit/sim-lin.csv",
        "model-fit/suellen-lin.csv",
        "testchar-plot/testchar-plot.R"
    output:
        "testchar-plot/sim-testchar.pdf",
        "testchar-plot/suellen-testchar.pdf"
    shell:
        "Rscript testchar-plot/testchar-plot.R"

# Report ======================================================================

rule report:
    input:
        "data-plot/sim-hist-cont.pdf",
        "data-plot/suellen-hist.pdf",
        "testchar-plot/sim-testchar.pdf",
        "testchar-plot/suellen-testchar.pdf",
        "report/report.Rmd"
    output:
        "report/report.pdf"
    shell:
        """Rscript -e 'rmarkdown::render("report/report.Rmd")'"""
