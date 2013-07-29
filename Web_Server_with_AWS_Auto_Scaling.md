# Steps to setting up an auto-scaling webserver using AWS


AWS Console: https://console.aws.amazon.com/ec2


## set up & test on one machine  (basic machine)

### create account, OBTAIN X.509 CREDENTIALS AND API KEY, and launch free Ubuntu machine.

AWS Console: https://console.aws.amazon.com/ec2



### create elastic IP for the basic machine



### connect to elastic IP

sshfs -o IdentityFile=~/AWS/autokey.pem ubuntu@176.34.229.204:/home/ubuntu ~/aws_auto/
 
ssh -i autokey.pem ubuntu@176.34.229.204


### set-up environment, upload content, test using the elastic IP.

[How to install oracle java 7 in ubuntu 13.04/12.10/12.04](http://www.ubuntugeek.com/how-to-install-oracle-java-7-in-ubuntu-12-04.html)




### create the boot script for the web server

/home/ubuntu/startplay.sh
<pre><code>
cd /home/ubuntu/Play20/samples/scala/framework_benchmarks/

/home/ubuntu/Play20/play  run
</code></pre>

/etc/rc.local
<pre><code>
#!/bin/sh -e
#
# rc.local
#
# This script is executed at the end of each multiuser runlevel.
# Make sure that the script will "exit 0" on success or any other
# value on error.
#
# In order to enable or disable this script just change the execution
# bits.
#
# By default this script does nothing.

/home/ubuntu/startplay.sh

exit 0
</code></pre>


### test
ubuntu$sudo reboot

access webpage using elastic IP.


## set-up your own AMI from basic machine


[AWS Console](https://console.aws.amazon.com/ec2)


Instances -> select instance -> Action -> Create Image (EBS AMI)



## Set Auto-Scaling Group

http://docs.aws.amazon.com/AutoScaling/latest/GettingStartedGuide/Welcome.html
 

http://www.robertsindall.co.uk/blog/how-to-use-amazons-auto-scaling-groups/


### set configuration  

$ export AWS_CREDENTIAL_FILE=/home/jiansen/AWS/aws-credentials.txt

$ as-create-launch-config AutoScaleLC --image-id ami-a9f8e6dd --region eu-west-1 --instance-type t1.micro --group awseb-e-ictxp7cjjz-stack-AWSEBSecurityGroup-YAXYRWU3MQO8 --key autokey
OK-Created launch config

 (Ubuntu 13.04 t1.mociro eu-west-1)


ami-a9f8e6dd is the AMI ID of the created EBS AMI.



### check configuration

$ as-describe-launch-configs --headers --region eu-west-1 --max-records 50


LAUNCH-CONFIG NAME IMAGE-ID TYPE 

LAUNCH-CONFIG AutoScaleLC ami-a9f8e6dd t1.micro



### set auto scale group

$ as-create-auto-scaling-group auto-scaling-group --region eu-west-1 --availability-zones eu-west-1a eu-west-1b eu-west-1c --launch-configuration AutoScaleLC 1 --max-size 10 --min-size 1

OK-Created AutoScalingGroup



### check auto scale  group

$ as-describe-auto-scaling-groups --headers --region eu-west-1


AUTO-SCALING-GROUP GROUP-NAME LAUNCH-CONFIG AVAILABILITY-ZONES MIN-SIZE MAX-SIZE DESIRED-CAPACITY

AUTO-SCALING-GROUP auto-scaling-group AutoScaleLC eu-west-1a,eu-west-1b,eu-west-1c 1 10 1 

INSTANCE INSTANCE-ID AVAILABILITY-ZONE STATE STATUS LAUNCH-CONFIG

INSTANCE i-ed9650a3 eu-west-1b InService Healthy AutoScaleLC



### edit instance details from the console

https://console.aws.amazon.com/ec2



### After use, set zero instances in the Auto Scaling Group?
$as-update-auto-scaling-group auto-scaling-group --desired-capacity 0 --max-size 0 --min-size 0 --region eu-west-1



### Update Auto-Scaling Group status

$ as-update-auto-scaling-group MyGroup --launch-configuration MyLC --availability-zones us-west-2a --min-size 1 --max-size 12
OK-Updated AutoScalingGroup






