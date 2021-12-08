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

RUN apt-get update && apt-get install -y --no-install-recommends \
  python3-pip \
  && pip install -U radian

EXPOSE 8787
EXPOSE 3838

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
