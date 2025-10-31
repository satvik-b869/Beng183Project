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
COPY rnaseq-app/backend/ /app/

# run inside the env
SHELL ["micromamba","run","-n","rnaseq-bio","/bin/bash","-lc"]

ENV FLASK_RUN_HOST=0.0.0.0
ENV PORT=5050
EXPOSE 5050

CMD ["micromamba","run","-n","rnaseq-bio","python","/app/app.py"]
