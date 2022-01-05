FROM hatmatrix/ml-base:latest

LABEL maintainer="Rahul S <rahul.sangole@gmail.com>"
LABEL project="High Performance Time Series"

ENV MRAN_BUILD_DATE=2021-10-01

RUN mkdir dotdotfiles
COPY dotfiles dotfiles/

RUN mkdir -p /home/rstudio/.R/snippets \
	&& mkdir -p /home/rstudio/.R/rstudio/keybindings \
	&& cp -r dotfiles/.Rprofile /home/rstudio/ \
	&& cp -r dotfiles/r.snippets /home/rstudio/.R/snippets/ \
	&& cp -r dotfiles/user-settings /home/rstudio/.rstudio/monitored/user-settings/ \
	&& cp dotfiles/keybindings/*.* /home/rstudio/.R/rstudio/keybindings/

RUN Rscript -e "install.packages(c('remotes', 'renv'), repos = 'https://cran.microsoft.com/snapshot/${MRAN_BUILD_DATE}')"

WORKDIR /renv
COPY renv.lock renv.lock
RUN Rscript -e 'renv::restore()'
WORKDIR /

# Install miniconda
ENV CONDA_DIR /opt/conda
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && \
     /bin/bash ~/miniconda.sh -b -p /opt/conda

ENV PATH=${CONDA_DIR}/bin:$PATH

RUN apt-get update && apt-get install -y --no-install-recommends \
  python3-pip \
 && Rscript -e "devtools::install_github('business-science/modeltime.gluonts')" \
 && Rscript -e "options('reticulate.conda_binary' = '/opt/conda/bin/conda'); modeltime.gluonts::install_gluonts()" \
 && conda install -c conda-forge conda-bash-completion

RUN conda init bash \
 && pip install -U radian

# Change shell to /bin/bash and conda env to r-gluonts from here onwards
SHELL ["conda", "run", "-n", "r-gluonts", "/bin/bash", "-c"]
RUN pip install -U radian

RUN echo "options(\"reticulate.conda_binary\" = \"/opt/conda/bin/conda\")" >> /root/.Rprofile \
  && echo "RETICULATE_MINICONDA_PATH=\"/opt/conda/bin/\"" >> /root/.Renviron

EXPOSE 8787
EXPOSE 3838

# ENTRYPOINT [ "/bin/bash" ]
# ENTRYPOINT ["conda", "activate", "r-gluonts"]
# ENTRYPOINT ["conda", "run", "--no-capture-output", "-n", "r-gluonts", "radian"]

# Install Tidymodels Development Versions:
# ----------------------------------------
# RUN Rscript -e 'devtools::install_github("tidymodels/tune")' && \
# 	Rscript -e 'devtools::install_github("tidymodels/recipes")' && \
# 	Rscript -e 'devtools::install_github("tidymodels/workflows")' && \
# 	Rscript -e 'devtools::install_github("tidymodels/parsnip")'

# Modeltime & Timetk Development Versions
# ----------------------------------------
# RUN Rscript -e 'devtools::install_github("business-science/modeltime")' && \
# 	Rscript -e 'devtools::install_github("business-science/timetk")'
