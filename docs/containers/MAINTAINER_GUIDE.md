# Container Maintainer Guide

This document describes how to manage container images for the project.

## Update dependencies

1. Edit [`environment.yml`](../../environment.yml) when dependencies change.
2. Rebuild the development image and push it to GHCR (authenticate with a token that has `write:packages` scope):
   > **Note:** Replace `{YOUR_GITHUB_USERNAME}` with your actual GitHub username or organization name in the commands below.
   ```bash
   docker build -t ghcr.io/{YOUR_GITHUB_USERNAME}/oferta_educativa_laboral:dev .
   docker push ghcr.io/{YOUR_GITHUB_USERNAME}/oferta_educativa_laboral:dev
   ```
3. The workflow [`docker-build.yml`](../../.github/workflows/docker-build.yml) builds the image in CI and pushes it to GHCR using the `GHCR_TOKEN` secret. Ensure this secret is defined in the repository settings with appropriate scopes.

## Build Apptainer images

- **Local build**
  ```bash
  apptainer build oferta-laboral.sif Singularity.def
  ```
- **CI build**
  The workflow [`singularity-build.yml`](../../.github/workflows/singularity-build.yml) performs the same step in GitHub Actions and also requires `GHCR_TOKEN` for authentication.

## Testing

Before publishing any image, run the project tests inside the container to ensure the environment works as expected. For example:

```bash
docker run --rm ghcr.io/{GITHUB_ORG_OR_USERNAME}/oferta_educativa_laboral:dev pytest --maxfail=1 --disable-warnings -q
```
