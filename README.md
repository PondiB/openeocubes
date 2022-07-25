
# OpenEO Compliant Lightweight R Platform for Processing Time Series Satellite Images

The service integrates STAC API, OpenEO standards and gdalcubes to be a lightweight platform to enable analysis of time series satellite images via OpenEO Compliant RESTful endpoints using R-Client . It also supports users to run their custom R functions.


![](docs/lightweight-architecture-v1.png)


After processing the data , one can zip and export the final output to AWS S3 bucket where they can download and explore on open source tools like QGIS.

## Easy Deployment from DockerHub
Assuming you have Docker installed. This is the easiest approach.
You can get a hosted Docker image of the platform on DockerHub
https://hub.docker.com/r/brianpondi/openeocubes

```bash
docker run -p 8000:8000  brianpondi/openeocubes
```

## Easy Deployment with Docker
If you want to change the source code then this approach is recommended.
You first need to clone the repository via this command:

```bash
git clone https://github.com/PondiB/openeogdalcubes.git
```

then you can change to that directory

```bash
cd openeogdalcubes
```



Run it :

```bash
docker-compose up
```

Run in detached mode :

```bash
docker-compose up -d
```

Shutting it down:

```bash
docker-compose down
```

Force restart  and rebuild:

```bash
docker-compose up --build --force-recreate --no-deps -d
```
