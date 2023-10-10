
# Install Instructions

## Changes since MS2
Previously, this project relied on the ocaml [graphics module](https://github.com/ocaml/graphics). In our experience, this module was unreliable, and we removed it from the project. The project has been updated to use [PyML](https://opam.ocaml.org/packages/pyml/) with a custom OCaml graphics library and a [PyGame](https://www.pygame.org) backend. The project can now run natively on Mac, but still requires a virtual machine for windows

## Windows Instructions

This project requires a virtual linux machine to run. We recommend using the official 3110 VirtualBox VM.

### Install the CS 3110 VirtualBox VM
Instructions for installing the virtual machine are available in the appendix of the online textbook. You can find the instructions at [this link](https://cs3110.github.io/textbook/chapters/appendix/vm.html).

After you have the virtual machine set up, proceed with the Mac/Linux instructions on the VM.

## Mac/Linux Instructions

### Step 0: Install OCaml

Install OCaml by following the instructions available in the [CS 3110 online textbook](https://cs3110.github.io/textbook/chapters/preface/install.html). Be sure to install all the necessary packages. You may skip this step if you are using the official 3110 virtual machine.

```
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc
```

### Step 1: Install Python and PyGame

Install [python](https://www.python.org/downloads/) and [PyGame](https://www.pygame.org). They may already be installed. Ensure that PyGame is installed in the correct python installation.

You *must* deactivate any [Conda](https://docs.conda.io/en/latest/) environments, including the base environment, if you have conda installed.

```
conda deactivate
```

To determine which python installation the project will use, run 

```
make python
```

Intsall PyGame:

```
[path to python interpreter] -m pip install pygame
```

 You may use a [venv virtual environment](https://docs.python.org/3/tutorial/venv.html) with pygame installed.

### Step 2: Install OCaml modules
This project requires several modules from opam. Open the terminal and run:

```
opam update
opam install pyml
opam install async
```

### Step 3: Run the project
Download the project source code to your VM. Open the terminal, navigate to the project directory and run this command:

```
make play
```

The code will compile, and the GUI will launch with the game. For other helpful make commands, see README.md.
