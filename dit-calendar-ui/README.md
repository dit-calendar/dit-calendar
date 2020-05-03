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
  * build docker image `docker build -t ditcalendar/dit-calendar-ui .`
  * update image to docker hub `docker push ditcalendar/dit-calendar-ui`
* kubernetes
  * set up kubectl
    * `export KUBECONFIG=$HOME/Downloads/okteto-kube.config:${KUBECONFIG:-$HOME/.kube/config}`
    * `kubectl get all`
  * update pod `kubectl apply -f k8s.yml`