
Welcome to NB_JAVAC!
===================

Please run the below command to build nb-javac. Build of nb-javac will generate two jars 
namley javac-api.jar and javac-impl.jar at location ./make/langtools/netbeans/nb-javac/dist/

ant -f ./make/langtools/netbeans/nb-javac clean jar

Please run below command to zip the source code of nb-javac

ant -f ./make/langtools/netbeans/nb-javac zip-nb-javac-sources

[Note : use JDK-8 for building nb-javac ]
