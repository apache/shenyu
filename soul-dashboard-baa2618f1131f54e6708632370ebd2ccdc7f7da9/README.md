# Soul DashBoard

![build](https://github.com/dromara/soul-dashboard/workflows/build/badge.svg)


## Overview
Soul DashBoard is frontend of a management background for [soul](https://github.com/dromara/soul).

### Soul Admin Backend
soul-admin is a standard spring boot project,click [here](https://github.com/dromara/soul/tree/master/soul-admin) for more information.

## Prerequisite
- node v8.0+

## How Build

### Configuration

modify the api url for different environment, eg: `http://192.168.1.100:8000`
![index.ejs](https://raw.githubusercontent.com/dromara/soul-dashboard/master/doc/img/index.ejs.png)


### Develop Environment

```shell
# install dependencies in this project root path.
npm install
# start
npm start
```

### Production Environment

```shell
# install dependencies in this project root path.
npm install
# build for production
npm run build
```

## Screenshot

#### Divide Plugin
![index](https://raw.githubusercontent.com/dromara/soul-dashboard/master/doc/img/index.jpg)

#### Add Rules
![add rules](https://raw.githubusercontent.com/dromara/soul-dashboard/master/doc/img/add-rules.png)

#### Plugin Management
![Plugin Management](https://raw.githubusercontent.com/dromara/soul-dashboard/master/doc/img/plugin-management.jpg)

