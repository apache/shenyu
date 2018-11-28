FROM java:openjdk-8-jre-alpine
VOLUME /tmp
ADD target/soul-bootstrap.jar soul-bootstrap.jar
EXPOSE 8089
ENTRYPOINT ["java","-Djava.security.egd=file:/dev/./urandom","-jar","/soul-bootstrap.jar"]
