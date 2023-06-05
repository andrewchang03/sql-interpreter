# SQL Interpreter in OCaml

The semester-long final project for Cornell University's CS 3110: 
Data Structures and Functional Programming course taught in OCaml.

We build a database management system that interprets fundamental SQL queries
CREATE TABLE, DROP TABLE, SELECT FROM WHERE, INSERT INTO, ALTER TABLE, 
DELETE FROM, UPDATE, AGGREGATE (for MIN, MAX, COUNT, AVG, SUM) 
and simulates performing them. A custom REPL for the system is built to test 
queries and handle file I/O in CSV. Please visit [INSTALL.md](INSTALL.md) 
to see how to configure OCaml environment on your local machine to test this project.