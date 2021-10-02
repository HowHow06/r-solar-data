FROM rocker/tidyverse

RUN apt-get update -qq && apt-get install -y \ 
    git-core \ 
    libssl-dev \ 
    libcurl4-gnutls-dev 

#copy the folder api to usr/local/src/testing-api
COPY ./api/ /usr/local/src/testing-api
WORKDIR /usr/local/src/testing-api

RUN chmod 700 start.sh

# Install R packages / setup markovifyR.
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('imputeTS')"
RUN R -e "install.packages('rstudioapi')"

# Port 8000 for local usage, not used on Heroku.
EXPOSE 8000

ENTRYPOINT ["/usr/local/src/testing-api/start.sh"]
CMD ["routes.R"]