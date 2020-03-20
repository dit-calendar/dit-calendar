package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.Task
import com.ditcalendar.bot.data.Tasks
import com.elbekD.bot.types.User
import com.github.kittinunf.fuel.core.extensions.authentication
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.fuel.httpPut
import com.github.kittinunf.result.Result
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.list

class TaskEndpoint {

    private val config by config()

    private val ditCalendarUrl = config[dit_calendar_server_url]

    fun readTasks(calendarId: Long, token: String): Result<Tasks, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId/tasks"
                .httpGet()
                .authentication().bearer(token)
                .responseString()

        return Result.of {
            Json(JsonConfiguration.Stable.copy(strictMode = false))
                    .parse(Task.serializer().list, result.get())
        }
    }

    fun assignUserToTask(taskId: Long, user: User, token: String): Result<Unit, Exception> {
        val (_, response, result) = "$ditCalendarUrl/calendarentries/1/tasks/$taskId/assignment"
                .httpPut()
                //TODO: Send proper body
                .body("user")
                .authentication().bearer(token)
                .responseString()
        return if (response.statusCode == 200) Result.Success(Unit) else Result.error(result.component2()!!)
    }
}