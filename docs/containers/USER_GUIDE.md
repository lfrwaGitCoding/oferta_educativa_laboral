# Container User Guide

This guide shows how to run the pipeline using the prebuilt image from the GitHub Container Registry (GHCR) and how to convert that image to Apptainer for high-performance computing (HPC) environments.

## Using Docker

Pull the image from GHCR and execute the pipeline:

```bash
docker pull ghcr.io/<owner>/oferta_educativa_laboral:latest
docker run --rm -v "$PWD/data:/data" ghcr.io/<owner>/oferta_educativa_laboral:latest \
    python oferta_educativa_laboral/pipeline/pipeline_oferta_laboral.py make full -v5
```

- `-v "$PWD/data:/data"` mounts a host `data` directory inside the container.
- Outputs are written to `results/` in the container. With the above mount, the host will see them at `data/results/`.
- The image bundles all dependencies so no manual environment activation is required.
- Replace `<owner>` with the GitHub organization or username hosting the image.

## Using Apptainer (rootless HPC)

Build an Apptainer image from the GHCR image and run the pipeline:

```bash
apptainer build oferta-laboral.sif docker://ghcr.io/<owner>/oferta_educativa_laboral:latest
apptainer run --bind "$PWD/data:/data" oferta-laboral.sif \
    python oferta_educativa_laboral/pipeline/pipeline_oferta_laboral.py make full -v5
```

The `--bind` flag performs the same volume mount as Docker. Results appear under `data/results/` on the host, and the container's environment is automatically available during execution.
