FROM ubuntu:20.04
RUN apt-get update && apt-get -y install wget netbase
WORKDIR /archetype
RUN wget https://github.com/edukera/archetype-lang/releases/download/1.3.0/archetype-x64-linux
RUN chmod +x /archetype/archetype-x64-linux
ENTRYPOINT ["/archetype/archetype-x64-linux"]
