package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.Task
import com.github.kittinunf.fuel.core.extensions.authentication
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import kotlinx.serialization.internal.ArrayListSerializer
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.list

class TaskEndpoint {

    private val config by config()

    private val ditCalendarUrl = config[dit_calendar_server_url]

    fun readTasks(calendarId: Long, token: String): Result<List<Task>, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId/tasks"
                .httpGet()
                .authentication().bearer(token)
                .responseString()

        return result.flatMap {
            Result.of<List<Task>, Exception> {
                Json(JsonConfiguration.Stable.copy(strictMode = false))
                        .parse(Task.serializer().list, result.get())
            }
        }
    }
}