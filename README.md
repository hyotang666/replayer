# REPLAYER 3.1.1
## What is this?
Music player in REPL.

## Features

* Play wav, mp3 files.
* Add tag to files.
* Search by tags.
* Supported ros script.

## Usage
### PLAY

One file.

```lisp
* (play "~/path/to/file.wav")
```
Some files.

```lisp
* (play (uiop:directory-files "~/Music/directory/" "*.wav"))
```

### TAG
Adding tag to files.

```lisp
* (tag "tag" (uiop:directory-files "~/Music/directory/" "*.wav"))
```

Play by tag.

```lisp
* (play (make-tag :exp "tag"))
```

Logical expressions are supported.

```lisp
* (play (make-tag :exp '(and "tag1" "tag2" "tag3")))
```

### From shell
TODO

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with

## Installation
TODO
