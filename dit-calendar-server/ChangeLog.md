# Revision history for dit-calendar-server

## 0.6.0.0 -- 2019-

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
