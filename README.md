# Introduction 

To whom it may concern, 

Hello. I’m assuming you are here because you’d like a glimpse of what kinds of work I typically do. Thank you for taking the time to check out this repository. Below are some descriptions containing the context behind some of work here. 

## AI-Hackathon 

This is code from a short coding event hosted by Genentech’s programming teams aimed at giving us programmers some insight into how large language models and chatbots worked. 

The first app (app.R) and the related functions in the file get_query_body.R contain functions and scripts meant to emulate how a chatbot works. The questions fed in from the sample_questions.txt file would be read in and processed into json object to be passed to Azure API. The responses were then collected and displayed in the order that the questions were asked. 

The second app (app2.R) contains functions and a script for using embeddings to answer questions based on a provided reference text. 

Both of these scripts were meant to be run using command line. 

## make_main

The code in this file is a large function that I wrote for a R Shiny application. The context here was that there were two packages that a team used for analyzing FMI datasets, a newer one and an older one. I was tasked with creating an app that used the interface of an app built around the older package that used the outputs of the newer package. 

This script essentially transforms the outputs of the newer package into a form resembling the outputs of the older package so they can be passed into the app’s scripts. 

NOTE: I only included this function’s script in the repository because that’s what *I* wrote for this project. 
