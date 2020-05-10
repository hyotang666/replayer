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
Start replayer server.

```shell
replayer start
```

Play one music.

```shell
replayer play path/to/music.wav
```

Play some files

```shell
replayer play $(ls music/directory/*.wav)
```

Play list

```shell
cat play-list | xargs replayer play
```

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with

## Installation
### REQUIREMENT

* [roswell](https://github.com/roswell/roswell)
* [sqlite3](https://sqlite.org/index.html)

### REPL only

```shell
ros install hyotang666/r-iff hyotang666/wav-parser hyotang666/replayer
```

```lisp
* (ql:quickload :replayer)
```

### With shell

```shell
ros build $HOME/.roswell/local-projects/hyotang666/roswell/replayer.ros

ros exec replayer
```

If you want reduce `ros exec` do like

```shell
echo 'export PATH="$HOME/.roswell/bin:$PATH"' >> ~/.zshrc
```
