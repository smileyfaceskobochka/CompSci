-- create database
CREATE DATABASE test_db;
\c test_db;

-- Drop tables if exist
DROP TABLE IF EXISTS employees;
DROP TABLE IF EXISTS departments;

-- Create tables
CREATE TABLE departments (
  dept_id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE employees (
  emp_id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  dept_id INT REFERENCES departments(dept_id) ON DELETE SET NULL
);

-- Insert sample departments
INSERT INTO departments (name) VALUES 
  ('Engineering'),
  ('HR'),
  ('Marketing'),
  ('Finance');

-- Insert sample employees
INSERT INTO employees (name, dept_id) VALUES
  ('Alice', 1),
  ('Bob', 2),
  ('Charlie', 1),
  ('Dana', 4),
  ('Eve', 1),
  ('Frank', 2),
  ('Grace', 1);

