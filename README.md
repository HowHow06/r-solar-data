# R-solar-data
This is an API written to get daily and monthly Solar Irradiance, Cloud Amount, Temperature and Precitipation from [NASA POWER Portal](https://power.larc.nasa.gov/). This API is written using R, plumber, docker and Heroku.

## Documentation
API Docs can be found here: [Click me to view Documentation](https://demo-r-solar-data.herokuapp.com/__docs__/)


## Hosting it on Heorku
To Host it on Heroku, first download Docker Desktop and Heroku CLI

At root folder, run the commands:
```sh
$ heroku login
```

```sh
$ heroku container:login 
```

```sh
$ heroku container:push web -a <your-heroku-app-name>
```

```sh
$ heroku container:release web -a <your-heroku-app-name>
```
