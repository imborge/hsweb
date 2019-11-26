# hsweb

A cookiecutter-template for Haskell.

- [Servant](https://www.servant.dev/)
- [Beam](https://tathougies.github.io/beam/)

## Features

- [x] Simple user system
- [x] JWT authentication

Implements routes and handlers for accounts:

- Account creation: POST /account
- Change password: POST /account/setPassword
- Get auth token: POST /auth/grant
