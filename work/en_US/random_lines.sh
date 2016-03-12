#!/bin/bash
shuf en_US.blogs.txt > salida
head -n 500 salida > 1_en_US.blogs.txt 
tail -n 500 salida > 2_en_US.blogs.txt 
