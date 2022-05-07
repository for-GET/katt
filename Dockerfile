# syntax=docker/dockerfile:1
ARG FROM
FROM ${FROM}

LABEL maintainer="mejla@software.se"
LABEL vendor="Y Software AB"
LABEL url="https://github.com/for-get/katt"
LABEL vcs-url="https://github.com/for-get/katt"
ARG LABEL_VCS_REF="0"
LABEL vcs-ref=${LABEL_VCS_REF}
ARG LABEL_BUILD_DATE="1970-01-01T00:00:00Z"
LABEL build-date=${LABEL_BUILD_DATE}

LABEL io.whalebrew.name katt
LABEL io.whalebrew.config.keep_container_user 'true'
LABEL io.whalebrew.config.working_dir '$PWD'

COPY . /katt
RUN cd /katt && ./Dockerfile.build.sh
RUN cd /katt && make

ENTRYPOINT ["/katt/bin/katt"]
