FROM ubuntu

RUN apt-get update && apt-get install -y \
    python3 \
    python3-selenium \
    wget \
    unzip \
    sudo \
    fonts-liberation \
    libappindicator3-1 \
    libasound2 \
    libatk-bridge2.0-0 \
    libatk1.0-0 \
    libcairo2 \
    libcups2 \
    libdbus-1-3 \
    libgdk-pixbuf2.0-0 \
    libnspr4 \
    libnss3 \
    libxss1 \
    lsb-release \
    xdg-utils

RUN cd $HOME
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN dpkg -i google-chrome-stable_current_amd64.deb
RUN apt update
RUN apt -f install -y

RUN wget https://chromedriver.storage.googleapis.com/2.40/chromedriver_linux64.zip
RUN unzip chromedriver_linux64.zip
RUN mv chromedriver /usr/bin/

RUN groupadd -g 1000 developer && \
    useradd  -g developer -G sudo -m -s /bin/bash chloe && \
    echo 'chloe:chloe' | chpasswd

RUN echo 'Defaults visiblepw' >> /etc/sudoers
RUN echo 'chloe ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER chloe

CMD ["/bin/bash"]
