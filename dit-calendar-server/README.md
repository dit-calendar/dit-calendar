# Build

## Cabal
* Dependency hinzufügen: in dit-calendar.cabal unter dem "build-depends:"-Tag
* alles installieren und bauen: `cabal install -j`

### Sandbox
* beim erstem Mal sandbox aufsetzen: `cabal sandbox init`
  * alles Dependency und Compelierte Dateinen werden nicht mehr global sondern local(im Projektverzeichnis) installiert
* Play around with the code in GHCi
  * `cabal repl`

### Programm bauen und ausführen
* bauen: `cabal build `
* starten: `.cabal-sandbox/bin/dit-calendar`
* einzelne Datein ausführen: ?
* bei kaputtem cabal: `cabal clean`

### Test
* builds tests and runs them: `cabal install --enable-tests`
  * oder `cabal configure --enable-tests`
* alle Tests ausführen: `cabal test`
* einzelne Tests ausführen: ?

## Stack
* buil: `stack build`
  * `stack build --fast --file-watch`
* execute: `stack exec dit-calendar`
* if stack broken: `stack clean`
* repl: `stack ghci`
* test: `stack test`
* Manche IDE's benötigen das Packet "intero" dies muss beim ersten mal manuell installiert werden stack install intero-0.1.24 --resolver=lts-10.10

# Code Quality
* http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/


# URL's

## User

### Login
```sh
curl -H "Content-Type: application/json" -X POST -d '{"user":"alex2","password":"12345"}' http://localhost:8080/authenticate/authentication-methods/password/token --insecure
```

### Create User
```sh
curl -H "Content-Type: application/json" -X POST -d '{"naPassword":"12345","naPasswordConfirm":"12345", "naUser":{"email": "a.befort@googlemail.com", "username":"alex2", "userId":0}}' http://localhost:8080/authenticate/authentication-methods/password/account --insecure
```

### Get Logged User
```sh
curl -H "Authorization: Bearer xxx" -X GET http://localhost:8080/users/me --insecure
```

### update User
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X PUT -d '{"loginName":"test", "userId":1}' http://localhost:8080/user/me --insecure
```

### send request with token
```
curl -H "Authorization: Bearer xxx" -X GET http://localhost:8080/ --insecure
```

### show all users
```sh
curl -X GET http://localhost:8080/users --insecure
```

### show specific user
```sh
curl -X GET http://localhost:8080/users/1 --insecure
```

## Calendar

### show calendar
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X GET http://localhost:8080/calendarentries/1 --insecure
```

### show calendar of logged in user
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X GET http://localhost:8080/calendarentries/ --insecure
```

### create calendar
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer XXX" -X POST -d '{"description":"testHeute","startDate":"2011-11-19T18:28:52.607875Z", "endDate":"2011-11-20T12:15:53.102875Z"}' http://localhost:8080/calendarentries --insecure
```

### calendar update
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer XX" -X PUT -d '{"description":"testHeute","startDate":"2011-11-19T18:28:52.607875Z", "endDate":"2011-11-20T12:15:53.102875Z"}' http://localhost:8080/calendarentries/1 --insecure
```

## Task

### show task
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X GET http://localhost:8080/calendarentries/1/tasks/1 --insecure
```

### show task of calendar
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X GET http://localhost:8080/calendarentries/2/tasks --insecure
```

### create task
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X POST -d '{"description":"testHeute", "belongingUsers":[1], "startTime":"2011-11-19T18:28:52.607875Z"}' http://localhost:8080/calendarentries/1/tasks --insecure
```

### update task
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X PUT -d '{"description":"testHeute2", "belongingUsers":[1]}' http://localhost:8080/calendarentries/1/tasks/1 --insecure
```

### add user to task
```sh
curl -H "Content-Type: application/json" -H "Authorization: Bearer xxx" -X PUT http://localhost:8080/calendarentries/1/tasks/1/assignment --insecure
```
