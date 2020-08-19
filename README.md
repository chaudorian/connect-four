# Connect Four

Scala program allowing a user to play a Connect 4 game against a player or a computer.

## Prerequisites

What you need to install, compile and run the project :

- **Java** - 1.8
- **Scala** - 2.13.1
- **sbt** - 1.3.8 (optional for execution)


## Compiling

1. Go to the `connect-four` folder containing the `build.sbt` file.

```bash
cd connect-four\
```

2. Run `sbt`

```bash
sbt
```

3. Enter `compile` to compile (obviously)

```bash
compile
```

You can also compile with the following command without running sbt

```bash
sbt run
```


## Running

### With Scala

1. Go to the `connect-four` folder containing the JAR file `puissance-4.jar`

```bash
cd connect-four\
```

2. Enter the following command to execute the program

```bash
scala puissance-4.jar
```

### With SBT

1. Go to the `connect-four` folder containing the file `build.sbt`

```bash
cd connect-four\
```

2. Run `sbt`

```bash
sbt
```

3. Enter `~run`. The `~` is optional and causes sbt to re-run every time you save a file, allowing a fast edit/run/debug cycle. sbt also generates a target directory that you can ignore.

```bash
~run
```

You can also execute with the following command without running sbt

```bash
sbt run
```

## Authors

* **Dorian Chau** - *Initial work* - [chaudorian](https://github.com/chaudorian)


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details