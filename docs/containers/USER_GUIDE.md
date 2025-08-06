# Container User Guide

This guide shows how to run the pipeline using the prebuilt image from the GitHub Container Registry (GHCR) and how to convert that image to Apptainer for high-performance computing (HPC) environments.

## Using Docker

Pull the image from GHCR and execute the pipeline:

> **Important:** Replace `<owner>` in the commands below with the GitHub organization or username that hosts the image. For example, if your organization is `acme-corp`, use `ghcr.io/acme-corp/oferta_educativa_laboral:latest`.
```bash
docker pull ghcr.io/<owner>/oferta_educativa_laboral:latest
docker run --rm -v "$PWD/data:/data" -v "$PWD/results:/results" ghcr.io/<owner>/oferta_educativa_laboral:latest \
    python oferta_educativa_laboral/pipeline/pipeline_oferta_laboral.py make full -v5
```

- `-v "$PWD/data:/data"` mounts the host `data` directory inside the container.
- `-v "$PWD/results:/results"` mounts the host `results` directory for outputs.
- Outputs are written to `results/` in the container and appear directly in the host `results/` directory.
- The image bundles all dependencies so no manual environment activation is required.

## Using Apptainer (rootless HPC)

Build an Apptainer image from the GHCR image and run the pipeline:

**Remember to replace `<owner>` with your GitHub organization or username as described above.**
```bash
apptainer build oferta-laboral.sif docker://ghcr.io/<owner>/oferta_educativa_laboral:latest
apptainer run --bind "$PWD/data:/data" --bind "$PWD/results:/results" oferta-laboral.sif \
    python oferta_educativa_laboral/pipeline/pipeline_oferta_laboral.py make full -v5
```

The `--bind` flags perform the same volume mounts as Docker. Outputs appear directly in the host `results/` directory, and the container's environment is automatically available during execution.
