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

rule data:
    input:
        "data-raw/suellen.xlsx",
        "data-raw/eia.xlsx",
        "data/data.R"
    output:
        "data/suellen.csv"
    shell:
        "Rscript data/data.R"

# Data plotting ===============================================================

rule data_plot:
    input:
        "data-plot/data-plot.R",
        "data/read_data.R",
        "data/sim.csv",
        "data/suellen.csv"
    output:
        "data-plot/sim-hist-cont.pdf",
        "data-plot/sim-scatter.pdf",
        "data-plot/suellen-hist.pdf",
        "data-plot/suellen-hist-iga.pdf",
        "data-plot/suellen-hist-igg.pdf"
    shell:
        "Rscript data-plot/data-plot.R"

# Model fitting ===============================================================

rule fit:
    input:
        "model-fit/model-fit.R",
        "data/read_data.R",
        "data/sim.csv",
        "data/suellen.csv"
    output:
        "model-fit/sim-lin.csv",
        "model-fit/suellen-lin.csv"
    shell:
        "Rscript model-fit/model-fit.R"

# Resampling ==================================================================

rule resample:
    input:
        "resample/resample.R",
        "data/read_data.R",
        "data/sim.csv",
        "data/suellen.csv"
    output:
        "resample/resample-sim.csv",
        "resample/resample-suellen.csv"
    shell:
        "Rscript resample/resample.R"

rule resample_summ:
    input:
        "resample/summ.R",
        "resample/resample-sim.csv",
        "resample/resample-suellen.csv"
    output:
        "resample/summ-sim.csv",
        "resample/summ-suellen.csv"
    shell:
        "Rscript resample/summ.R"

# Result plots ================================================================

rule testchar_plot:
    input:
        "model-fit/sim-lin.csv",
        "model-fit/suellen-lin.csv",
        "resample/summ-sim.csv",
        "resample/summ-suellen.csv",
        "testchar-plot/testchar-plot.R"
    output:
        "testchar-plot/sim-testchar.pdf",
        "testchar-plot/suellen-testchar.pdf",
        "testchar-plot/sim-resample-testchar.pdf",
        "testchar-plot/suellen-resample-testchar.pdf"
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
