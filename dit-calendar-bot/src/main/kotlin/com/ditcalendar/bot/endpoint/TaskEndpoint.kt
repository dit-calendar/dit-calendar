package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.Task
import com.ditcalendar.bot.data.Tasks
import com.ditcalendar.bot.data.TelegramLink
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

    fun assignUserToTask(taskId: Long, telegramLink: TelegramLink, token: String): Result<Task, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/tasks/$taskId/assignment"
                .httpPut()
                .body(json.stringify(TelegramLink.serializer(), telegramLink))
                .authentication().bearer(token)
                .responseString()

        return result.flatMap {
            Result.of<Task, Exception> {
                json.parse(Task.serializer(), result.get())
            }
        }
    }
}