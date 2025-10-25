FROM mambaorg/micromamba:1.5

COPY environment.yml /tmp/environment.yml

USER root

RUN micromamba env create -f /tmp/environment.yml -n oferta_educativa_laboral \
    && micromamba clean --all --yes

ENV PATH=/opt/conda/envs/oferta_educativa_laboral/bin:$PATH

COPY . /workspace/oferta_educativa_laboral

WORKDIR /workspace/oferta_educativa_laboral

ENTRYPOINT ["/bin/bash"]

