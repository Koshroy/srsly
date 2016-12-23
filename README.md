Srsly
=====

A minimalist SRS flashcard tool born out of frustration with massive dependencies in Anki. Rather
than pull in 30 dependencies, I decided to make a simpler tool. A clean separation exists between
the frontend and backend. An initial implementation of the backend is done in shell scripts, but
will eventually become Haskell. The frontend is a Gtk2 app written in Haskell.

The flashcard deck format is a simple one easily editable with simple text editors, easily
searchable through simple shell commands, and easy to visualize using simple file explorer
tools. The actual flashcard content is stored in JSON files.

Simple is better here.
