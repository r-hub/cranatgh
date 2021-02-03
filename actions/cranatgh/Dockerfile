
FROM docker.pkg.github.com/r-hub/r-minimal/r-minimal:latest

RUN installr -d -t "linux-headers curl-dev bash" r-hub/cranatgh && \
    rm -rf /usr/local/lib/R/library/*/help &&                      \
    rm -rf /usr/local/lib/R/library/*/doc
