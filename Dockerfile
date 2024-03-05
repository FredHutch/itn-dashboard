FROM fredhutch/r-shiny-server-base:4.3.2

# RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("dplyr", "tidyr", "readr", "lubridate", "janitor", "shiny", "udpipe", "wordcloud", "bslib", "bsicons", "htmltools", "ggplot2", "fontawesome", "DT", "ggrepel"), repos="https://cran.rstudio.com/")'

ADD check.R /tmp/

RUN R -f /tmp/check.R --args dplyr tidyr readr lubridate janitor shiny udpipe wordcloud bslib bsicons htmltools ggplot2 fontawesome DT ggrepel

RUN rm -rf /srv/shiny-server/
ADD ./ /srv/shiny-server/

WORKDIR /srv/shiny-server/

EXPOSE 3838

ENV APPLICATION_LOGS_TO_STDOUT=true
ENV SHINY_LOG_STDERR=1

CMD ["/usr/bin/shiny-server"]

