
############################################################
# build
############################################################
ARG repo=erlang:22.3.4.20-alpine
FROM ${repo} AS build
# Install required tools.

RUN apk update
RUN apk add make curl gcc build-base bsd-compat-headers git
RUN cd /tmp/ && git clone https://github.com/sos-hub-vn/sos-backend.git
RUN cd /tmp/sos-backend/ && git checkout master

RUN cd /tmp/sos-backend && make; exit 0
RUN version=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell) && echo $version
RUN echo $version
RUN echo "create release"
RUN cd /tmp/sos-backend && echo $version && make rel

############################################################
# dist
############################################################
FROM erlang:22.3.4.20-alpine AS dist

EXPOSE 8080
RUN mkdir -p /usr/local/
RUN mkdir -p /tmp/sos-backend/_build/default/lib
COPY --from=build /tmp/sos-backend/_build/default/rel/sos /usr/local/sos/
COPY --from=build /tmp/sos-backend/_build/default/lib /tmp/sos-backend/_build/default/lib

WORKDIR /usr/local/sos