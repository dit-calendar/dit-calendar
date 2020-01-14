# deployment
* mvn clean package
* heroku deploy:jar target/bot-1.0-SNAPSHOT-jar-with-dependencies.jar --app dit-calendar

# manual test
* https://api.telegram.org/bot{token}/getWebhookInfo