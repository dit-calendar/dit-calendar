# Revision history for dit-calendar

## 0.6.0.0 -- 2020-06-01
* create telegram bot
  * written in kotlin with kt-telegram-bot lib
  * for post a calendar entry with id
  * assing/unassing telegram user to task
  * every account should have own telegram bot
* backend
  * environment variables overwrite config
  * replace happstack-foundation with custom setup
  * filter calendar entries with date
* k8s deployment
  * pods for server and ui
  * ingress controller for server and ui
  * PersistentVolumeClaim for acid state
* use travis build matrix for multiple projects

## 0.5.0.0 -- 2019-12-14
* ui
  * first simple ui client for creating calendar entries and tasks. written in elm
* backend
  * user permission resource control
  * Reader monad for holding AppContext
  * docker included
  * dto validation

## 0.4.0.0 -- 2019-03-12

* add optimistic locking
* add better error responses
* create mapper between dto and dbentity
* create config file

## 0.3.0.0 -- 2018-12-26

* change routing
* add dto layer
* change content media type to json

## 0.2.0.0 -- 2018-07-18

* Link between Auth and User
* User authentification with happstack-authenticate
* use web-routes-boomerang for routing
* Create Calendar with Tasks and user assignment
* use acid as db
* create webserver with happstack

## 0.1.0.0  -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
