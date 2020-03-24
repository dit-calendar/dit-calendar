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
import com.github.kittinunf.result.flatMap
import kotlinx.serialization.builtins.list
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration

class TaskEndpoint {

    private val config by config()

    private val ditCalendarUrl = config[dit_calendar_server_url]

    private val json = Json(JsonConfiguration.Stable.copy(ignoreUnknownKeys = true))

    fun readTasks(calendarId: Long, token: String): Result<Tasks, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId/tasks"
                .httpGet()
                .authentication().bearer(token)
                .responseString()

        return result.flatMap {
            Result.of<Tasks, Exception> {
                json.parse(Task.serializer().list, result.get())
            }
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