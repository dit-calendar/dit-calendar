package com.ditcalendar.bot.service

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_deployment_url
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.github.kittinunf.fuel.coroutines.awaitStringResponseResult
import com.github.kittinunf.fuel.httpGet

class ServerDeploymentService {

    private val config by config()
    private val ditCalendarUrl = config[dit_calendar_server_url]
    private val ditCalendarDeploymentUrl = config[dit_calendar_deployment_url]

    suspend fun deployServer() {
        if (!healthCheck())
            wakeUpServer()
    }

    private fun healthCheck(): Boolean =
            "$ditCalendarUrl/"
                    .httpGet()
                    .response()
                    .second
                    .statusCode == 200

    private suspend fun wakeUpServer() {
        "$ditCalendarDeploymentUrl/"
                .httpGet().awaitStringResponseResult()
    }
}