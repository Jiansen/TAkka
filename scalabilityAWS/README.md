# AWS Setup #

1. Start instance (right click on it on amazon EC2 dashboard).

2. Right click on your running instance (in the My Instances window on your dashboard and select 'Connect' - follow the instructions to set up your SSH client (I added the key with ssh-add ec2-keypair.pem rather than use -i).
(Note: change user to ubuntu@ or others rather than root@)

3. install java:   sudo apt-get install openjdk-7-jdk

4. install sbt: 

$ mkdir bin

$ cd bin 

$ http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch//0.12.1/sbt-launch.jar

$ cat > sbt

java -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar `dirname $0`/sbt-launch.jar "$@"

$ chmod u+x sbt

5. install git $sudo apt-get install git


6. pull TAkka repository from github

7. run application

