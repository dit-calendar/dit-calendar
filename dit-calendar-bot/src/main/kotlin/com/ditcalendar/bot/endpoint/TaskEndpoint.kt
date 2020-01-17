package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.Task
import com.github.kittinunf.fuel.core.extensions.authentication
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.result.Result

class TaskEndpoint {

    private val config by config()

    fun readTasks(calendarId: Long, token: String) : Array<Task>? {

        val ditCalendarUrl = config[dit_calendar_server_url]

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId/tasks"
                .httpGet()
                .authentication().bearer(token)
                .responseObject(Task.Deserializer())

        return when (result) {
            is Result.Failure -> {
                val ex = result.getException()
                println(ex)
                null
            }
            is Result.Success -> result.get()
        }
    }
}