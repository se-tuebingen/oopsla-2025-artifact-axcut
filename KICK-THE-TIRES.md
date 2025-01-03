## Kick-the-tires
Clone the artifact repository

```
git clone https://github.com/ps-tuebingen/oopsla-2025-artifact-axcut.git
```

or download the archive from [Zenodo](https://doi.org/10.5281/zenodo.14597445) and unpack it.
Change into the resulting directory.

Make sure that the Docker service is running.
Build the docker image (note the period)

```
docker build -t axcut-image .
```

This will download and install the necessary languages, which will take roughly 25 minutes (depending on your connection and computer power).
It will take about 5GB of disk space.

Now start a container from the Docker image with the following command:

```
docker run -itd --init --name axcut-container axcut-image
```

The container should now show up when running

```
docker ps
```

#### Quick test
For a quick check that compilation and execution of all benchmark programs works properly for all languages, run

```
docker exec -it axcut-container bash -c "cd benchmark-programs && make test"
```

This compiles the benchmark programs in all benchmarked languages and runs them on small inputs.

To test that the implementation of the compiler for AxCut works correctly, run

```
docker exec -it axcut-container bash -c "cd axcut && make test"
```

This builds the compiler, generates code for a few examples for all backends (aarch64, RISC-V, x86-64) and simulates execution of the generated code.

### Cleanup
Run the following commands (after `exit`ing the interactive session) to remove the docker container and image.

```
docker rm -f axcut-container
docker image rm -f axcut-image
```

Note that this does not remove the Ubuntu base-image, as it may be shared with other images.
If you want to remove that one, too, run

```
docker image rm -f ubuntu:24.04
```

To remove dangling intermediate images, run

```
docker image prune
```
