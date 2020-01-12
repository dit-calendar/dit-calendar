package com.ditcalendar.bot

import com.natpryce.konfig.*
import com.natpryce.konfig.ConfigurationProperties.Companion.systemProperties


fun config(): Lazy<Configuration> {
    return lazy { systemProperties() overriding
            EnvironmentVariables() overriding
            ConfigurationProperties.fromResource("config.properties") }
}

val server_port = Key("port", intType)
val telegram_token =  Key("telegram.token", stringType)
val heroku_app_name =  Key("heroku.app.name", stringType)


