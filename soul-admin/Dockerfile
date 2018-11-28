FROM java:openjdk-8-jre-alpine
VOLUME /tmp
ADD target/soul-admin.jar soul-admin.jar
EXPOSE 8086
ENTRYPOINT ["java","-Djava.security.egd=file:/dev/./urandom","-jar","/soul-admin.jar"]
