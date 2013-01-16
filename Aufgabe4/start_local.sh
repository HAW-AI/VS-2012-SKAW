#!/bin/bash
# example usage: 15100 10 1 "225.10.1.2" wlan

IF_IP=$(/sbin/ifconfig $5 | grep 'inet addr:' | cut -d: -f2 | awk '{ print $1}')
echo "Using IP:" $IF_IP "for interface" $5
CLASSPATH=`dirname $0`
HOST_NO=${HOSTNAME##lab}
erl -pa ebin/ -sname sender$3 -setcookie skaw -boot start_sasl -noshell -s controller start $1 $2 $3 $4 $IF_IP > $3.log &
echo $! >> pidlist
