# glasp-code
Simulations, data and figures of paper GLASP

## Usage

You may clone this repository and run the code directly in your computer, with 

```bash
git clone https://github.com/jlaria/glasp-code.git
```

However, due to the large number of dependencies, running the examples will probably fail. If you have `vscode`, `docker` and the `ms-vscode-remote.remote-containers` extension for `vscode`, you can open the cloned repository in a remote container and `vscode` will automatically install the required dependencies. Additional documentation can be found [here](https://code.visualstudio.com/docs/remote/containers).

If you have `docker` installed, you can run the docker container with 

```bash
docker run -it jlaria/glasp:0.0.1
```

and use `R` inside this container.

