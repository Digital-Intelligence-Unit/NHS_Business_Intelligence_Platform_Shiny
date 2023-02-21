#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server

# Set environment variables
env > /home/shiny/.Renviron

# Take ownership of DIRs
chown shiny.shiny /home/shiny/.Renviron
chown shiny.shiny /var/log/shiny-server

# Start shiny server with or without logs
if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ]
then
    exec xtail /var/log/shiny-server/ & # With logs
fi

exec shiny-server 2>&1

