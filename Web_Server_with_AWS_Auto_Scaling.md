# Steps to setting up an auto-scaling webserver using AWS


AWS Console: https://console.aws.amazon.com/ec2


## set up & test on one machine  (basic machine)

### create account, OBTAIN X.509 CREDENTIALS AND API KEY, and launch free Ubuntu machine.

AWS Console: https://console.aws.amazon.com/ec2



### create elastic IP for the basic machine



### connect to elastic IP

sshfs -o IdentityFile=~/AWS/autokey.pem ubuntu@176.34.229.204:/home/ubuntu ~/aws_auto/
 
ssh -i autokey.pem ubuntu@176.34.229.204


### upload content, set-up web server, test using the elastic IP.


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

## set-up image machine






## Set Auto-Scaling Group

http://docs.aws.amazon.com/AutoScaling/latest/GettingStartedGuide/Welcome.html
 

http://www.robertsindall.co.uk/blog/how-to-use-amazons-auto-scaling-groups/





## set up auto-scaling

