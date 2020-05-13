Very simple web-ui written in elm. For creating events and tasks from the perspective of the organization group.

# Build

## first
* `sudo npm install elm elm-test --unsafe-perm  -g`
* `sudo npm install -g elm --unsafe-perm=true --allow-root`

## Programm bauen und ausführen
* bauen: `elm make src/Main.elm --output src/Main.js`
* starten: `elm reactor`

## Test
* elm-test


# Deploy
* create docker image
  * build index.html: `elm make src/Main.elm --output src/Main.js --optimize`
    * minimize file `uglifyjs src/Main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=src/Main.js && uglifyjs src/Main.js --mangle --output=src/Main.js`
  * build docker image `docker build -t ditcalendar/dit-calendar-ui .`
  * publish image to docker hub `docker push ditcalendar/dit-calendar-ui`
* kubernetes
  * set up kubectl
    * `export KUBECONFIG=$HOME/Downloads/okteto-kube.config:${KUBECONFIG:-$HOME/.kube/config}`
    * `kubectl get all`
  * update pod `kubectl apply -f k8s.yml`