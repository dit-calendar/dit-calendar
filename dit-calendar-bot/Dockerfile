
## Stage 1 : install gradle dependencies
FROM quay.io/quarkus/centos-quarkus-maven:20.0.0-java8 AS setup
COPY build.gradle.kts settings.gradle /usr/src/app/
USER root
RUN chown -R quarkus /usr/src/app
USER quarkus
RUN gradle -b /usr/src/app/build.gradle.kts -c /usr/src/app/settings.gradle build --refresh-dependencies

## Stage 2 : build with gradle builder image with native capabilities
FROM setup AS build
COPY src /usr/src/app/src
USER root
RUN chown -R quarkus /usr/src/app
USER quarkus
RUN gradle -b /usr/src/app/build.gradle.kts -c /usr/src/app/settings.gradle clean buildNative

## Stage 3 : create the docker final image
FROM registry.access.redhat.com/ubi8/ubi-minimal
WORKDIR /work/
COPY --from=build /usr/src/app/build/*-runner /work/application
RUN chmod 775 /work
EXPOSE 8080
CMD ["./application", "-Dquarkus.http.host=0.0.0.0"]