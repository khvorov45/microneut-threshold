rule all:
    input:
        "data-plot/sim-hist-log.pdf",
        "data/suellen.csv"

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

rule sim_plot:
    input:
        "data-plot/data-plot.R",
        "data/sim.csv"
    output:
        "data-plot/sim-hist-log.pdf"
    shell:
        "Rscript data-plot/data-plot.R"
