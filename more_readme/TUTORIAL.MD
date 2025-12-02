# Tutorial for playing the Kabul Game

There are two games that is implemented here, that is the [CLI version](#cli-version) and [Web version](#web-version). Make sure Stack and Cabal is installed.

## CLI Version

Go to the project directory

```bash
cd cabo-card-functional
```

After that, there are two options.

- Run the game instantly by entering the command

    ```bash
    stack build
    ```

    ```
    stack run
    ```

    This will compile the `Main.hs` file in [app](/app/Main.hs) folder.

- Run the game using `.exe`.

    ```bash
    stack build
    ```

    Locate the folder 

    ```text
    .\cabo-card-functional\.stack-work\install\....\bin
    ```

    There should be an executable files that you can run.


## Web version

Go to the project directory

```bash
cd cabo-card-functional
```

Now, run the command

```bash
stack test
```

There should be an output in terminan that looks like this

```text
====== KABUL SERVER READY (PORT 3000) ======
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

Go to your browser and then type [http://localhost:3000/](http://localhost:3000/). The game then can be played there.