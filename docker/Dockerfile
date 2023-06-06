FROM ubuntu:20.04
RUN apt-get update && apt-get -y install wget netbase
WORKDIR /archetype
RUN wget https://github.com/completium/archetype-lang/releases/download/1.4.3/archetype-x64-linux-rev1.2
RUN chmod +x /archetype/archetype-x64-linux-rev1.2
ENTRYPOINT ["/archetype/archetype-x64-linux-rev1.2"]
