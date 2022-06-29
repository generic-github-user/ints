# intdb

A large database of integers and their relationships (mainly as Lisp
programming practice). The current strategy is roughly:

- Enumerate integers over a range of interest
- Get a random pair of two entries and check the following properties:
	- divisibility
	- sum, product
- Repeat several million times
- Filter for interesting items in the database and generate human-readable
  summaries of their properties
