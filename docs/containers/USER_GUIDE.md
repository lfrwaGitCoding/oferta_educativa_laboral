# Container User Guide

This guide shows how to run the pipeline using a prebuilt Docker image distributed as a tarball or by building the image locally. It also covers converting the image to Apptainer for high-performance computing (HPC) environments.

## Using Docker

Load the prebuilt image tarball and execute the pipeline:

```bash
docker load < oferta-laboral.tar.gz
docker run --rm -v "$PWD/data:/data" oferta-laboral:latest \
    python oferta_educativa_laboral/pipeline/pipeline_oferta_laboral.py make full -v5
```

If you do not have the tarball, build the image locally:

```bash
docker build -t oferta-laboral:latest .
```

- `-v "$PWD/data:/data"` mounts a host `data` directory inside the container.
- Outputs are written to `results/` in the container. With the above mount, the host will see them at `data/results/`.
- The image bundles all dependencies so no manual environment activation is required.

## Using Apptainer (rootless HPC)

Build an Apptainer image from the repository's `Singularity.def` and run the pipeline:

```bash
apptainer build oferta-laboral.sif Singularity.def
apptainer run --bind "$PWD/data:/data" oferta-laboral.sif \
    python oferta_educativa_laboral/pipeline/pipeline_oferta_laboral.py make full -v5
```

The `--bind` flag performs the same volume mount as Docker. Results appear under `data/results/` on the host, and the container's environment is automatically available during execution.
