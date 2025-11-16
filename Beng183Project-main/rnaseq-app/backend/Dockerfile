FROM mambaorg/micromamba:1.5.8

ARG ENV_NAME=rnaseq-bio
# Add Flask + flask-cors + dotenv + sqlalchemy AND the bio tools
RUN micromamba create -y -n $ENV_NAME -c conda-forge -c bioconda \
    python=3.11 \
    flask flask-cors python-dotenv sqlalchemy \
    fastqc fastp star subread samtools pigz \
 && micromamba clean -a -y

# create data dirs as root
USER root
RUN mkdir -p /data/storage /data/qc

WORKDIR /app
COPY . /app

# run inside the env
SHELL ["micromamba","run","-n","rnaseq-bio","/bin/bash","-lc"]

ENV FLASK_RUN_HOST=0.0.0.0
ENV PYTHONUNBUFFERED=1
ENV API_PORT=5050
ENV STORAGE_ROOT=/data/storage
ENV QC_ROOT=/data/qc
EXPOSE 5050

CMD ["micromamba","run","-n","rnaseq-bio","python","/app/app.py"]
