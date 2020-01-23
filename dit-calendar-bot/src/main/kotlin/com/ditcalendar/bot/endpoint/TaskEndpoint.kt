package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.Task
import com.github.kittinunf.fuel.core.extensions.authentication
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.result.Result
import kotlinx.serialization.internal.ArrayListSerializer
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.list

class TaskEndpoint {

    private val config by config()

    fun readTasks(calendarId: Long, token: String): List<Task>? {

        val ditCalendarUrl = config[dit_calendar_server_url]

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId/tasks"
                .httpGet()
                .authentication().bearer(token)
                .responseString()

        return when (result) {
            is Result.Failure -> {
                val ex = result.getException()
                println(ex)
                null
            }
            is Result.Success -> Json(JsonConfiguration.Stable.copy(strictMode = false))
                    .parse(Task.serializer().list, result.get())
        }
    }
}