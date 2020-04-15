rule all:
    input:
        "data-plot/sim-hist-cont.pdf",
        "data/suellen.csv"

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
