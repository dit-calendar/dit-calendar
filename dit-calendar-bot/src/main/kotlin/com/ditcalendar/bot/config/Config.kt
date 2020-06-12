package com.ditcalendar.bot.config

import com.natpryce.konfig.*
import com.natpryce.konfig.ConfigurationProperties.Companion.systemProperties


fun config(): Lazy<Configuration> {
    return lazy {
        systemProperties() overriding
                EnvironmentVariables() overriding
                ConfigurationProperties.fromResource("config.properties")
    }
}

val bot_name = Key("bot.name", stringType)
val webhook_is_enabled = Key("webhook.enabled", booleanType)

val server_port = Key("port", intType)
val telegram_token = Key("telegram.token", stringType)
val heroku_app_name = Key("heroku.app.name", stringType)

val dit_calendar_server_url = Key("dit.calendar.server.url", stringType)
val dit_calendar_user_name = Key("dit.calendar.user.name", stringType)
val dit_calendar_user_password = Key("dit.calendar.user.password", stringType)

val dit_calendar_deployment_url = Key("dit.calendar.deployment.url", stringType)

