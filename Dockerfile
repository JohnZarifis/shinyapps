FROM rocker/shiny:latest 

MAINTAINER Ioannis Zarifis

RUN apt-get update && apt-get install -y \  
    unixodbc unixodbc-dev freetds-dev freetds-bin tdsodbc \
    libxml2-dev libssl-dev
#libiodbc2-dev 
#RUN sudo su 
#RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
#RUN curl https://packages.microsoft.com/config/ubuntu/16.10/prod.list > /etc/apt/sources.list.d/mssql-release.list
#RUN exit
#RUN sudo apt-get update
#RUN sudo ACCEPT_EULA=Y apt-get install msodbcsql



# install additional packages
RUN R -e "install.packages(c('tidyverse','shinydashboard', 'stringi', 'RSQLite', 'DT','RODBC','plotly','mlr','gamlss','shinycssloaders','shinyBS','Cairo','ggthemes','ggrepel','highcharter','rmarkdown','flexdashboard','dygraphs','rpivotTable'), repos='https://cran.rstudio.com/')"


ADD etc_freetds_freetds.conf /etc/freetds/freetds.conf
ADD etc_odbc.ini /etc/odbc.ini
ADD etc_odbcinst.ini /etc/odbcinst.ini

RUN /bin/bash -c "odbcinst -i -s -f /etc/odbc.ini"


CMD ["/usr/bin/shiny-server.sh"]

