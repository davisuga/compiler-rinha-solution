FROM sbtscala/scala-sbt:graalvm-ce-22.3.3-b1-java17_1.9.6_3.3.1

COPY ./ ./

RUN [ "sbt", "assembly" ]

ENTRYPOINT [ "java", "-jar", "target/scala-3.3.1/graalinterpreter-assembly-0.1.0-SNAPSHOT.jar" ]