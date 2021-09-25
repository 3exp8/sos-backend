############################################################
# build
############################################################
ARG repo=erlang:22.3.4.20-alpine

FROM ${repo} AS build

# Install required tools.

RUN apk update

RUN apk add make curl gcc build-base bsd-compat-headers git

# Set the working directory
WORKDIR /usr/local/app

# Add the source code to app
COPY ./ /usr/local/app/

RUN make; exit 0
RUN version=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell) && echo $version
RUN echo $version
RUN echo "create release"
RUN echo $version && make rel

############################################################
# dist
############################################################
FROM erlang:22.3.4.20-alpine AS dist

EXPOSE 8080
RUN mkdir -p /usr/local/
RUN mkdir -p /tmp/sos-backend/_build/default/lib
COPY --from=build /usr/local/app/_build/default/rel/sos /usr/local/sos/
COPY --from=build /usr/local/app/_build/default/lib /usr/local/app/_build/default/lib

WORKDIR /usr/local/sos